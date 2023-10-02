/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*        Xavier Leroy, Collège de France and Inria project Cambium       */
/*                                                                        */
/*   Copyright 2023 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Support for ZSTD-compressed marshaling.
   Included in the bytecode interpreters (boot/ocamlrun, runtime/ocamlrun*).
   Not included by default in ocamlopt-generated executables, must be linked
   explicitly. */

#define CAML_INTERNALS

#include <string.h>
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"

#ifdef HAS_ZSTD

#include <zstd.h>

CAMLprim value caml_compression_supported(value vunit)
{ return Val_true; }

/* Header format:
       4 bytes         magic_number
       1 to 10 bytes   length of compressed data, in bytes, VLQ format
       1 to 10 bytes   length of uncompressed data, in bytes, VLQ format

  VLQ format is one or several bytes like 1xxxxxxx 1yyyyyyy 0zzzzzzz.
  First bytes have top bit 1, last byte has top bit 0.
  Each byte carries 7 bits of the number.
  Bytes come in big-endian order: xxxxxxx are the 7 high-order bits,
  zzzzzzzz the 7 low-order bits.
*/

static void putvlq(struct channel * chan, uintnat n)
{
  char buf[10];
  int pos;
  /* Convert number in little endian */
  buf[0] = n & 0x7F;
  for (pos = 0, n >>= 7; n != 0; n >>= 7) buf[++pos] = 0x80 | (n & 0x7F);
  /* Write number in big endian */
  for (/*nothing*/; pos >= 0; pos--) caml_putch(chan, buf[pos]);
}

static int getvlq(struct channel * chan, /*out*/ uintnat * res)
{
  unsigned char c = caml_getch(chan);
  uintnat n = c & 0x7F;
  while ((c & 0x80) != 0) {
    c = caml_getch(chan);
    uintnat n7 = n << 7;
    if (n != n7 >> 7) return -1;
    n = n7 | (c & 0x7F);
  }
  *res = n;
  return 0;
}

static void compress_blocks(char header[MAX_INTEXT_HEADER_SIZE],
                            int header_len,
                            struct output_block * input,
                            /*out*/ struct output_block ** result,
                            /*out*/ uintnat * result_len)
{
  ZSTD_CCtx * ctx;
  ZSTD_inBuffer in;
  ZSTD_outBuffer out;
  struct output_block * output, * output_head;
  uintnat output_len;
  int rc;

  ctx = ZSTD_createCCtx();
  if (ctx == NULL) goto oom1;
  /* First output block */
  output_head = caml_stat_alloc_noexc(sizeof(struct output_block));
  if (output_head == NULL) goto oom2;
  output = output_head;
  output->next = NULL;
  output_len = 0;
  out.dst = output->data; out.size = SIZE_EXTERN_OUTPUT_BLOCK; out.pos = 0;
  /* Start by compressing the header */
  in.src = header; in.size = header_len; in.pos = 0;
  do {
    ZSTD_compressStream2(ctx, &out, &in, ZSTD_e_continue);
  } while (in.pos < in.size);
  /* Now, compress the input blocks */
  in.src = input->data; in.size = input->end - input->data; in.pos = 0;
  do {
    if (out.pos == out.size) {
      /* Finish current output block */
      output->end = output->data + out.pos;
      output_len += out.pos;
      /* Allocate fresh output block */
      struct output_block * next =
        caml_stat_alloc_noexc(sizeof(struct output_block));
      if (next == NULL) goto oom3;
      output->next = next;
      output = next;
      output->next = NULL;
      out.dst = output->data; out.size = SIZE_EXTERN_OUTPUT_BLOCK; out.pos = 0;
    }
    if (in.pos == in.size && input != NULL) {
      /* Move to next input block and free current input block */
      struct output_block * next = input->next;
      caml_stat_free(input);
      input = next;
      if (input != NULL) {
        in.src = input->data; in.size = input->end - input->data;
      } else {
        in.src = NULL; in.size = 0;
      }
      in.pos = 0;
    }
    rc = ZSTD_compressStream2(ctx, &out, &in,
                              input == NULL ? ZSTD_e_end : ZSTD_e_continue);
  } while (! (input == NULL && rc == 0));
  /* Finish last block */
  output->end = output->data + out.pos;
  output_len += out.pos;
  /* Return results */
  *result = output_head;
  *result_len = output_len;
  ZSTD_freeCCtx(ctx);
  return;
oom3:
  /* Free the remaining input */
  for (struct output_block * blk = input; blk != NULL; /*nothing*/) {
    struct output_block * next = blk->next;
    caml_stat_free(blk);
    blk = next;
  }
  /* Free the output allocated so far */
  for (struct output_block * blk = output_head; blk != NULL; /*nothing*/) {
    struct output_block * next = blk->next;
    caml_stat_free(blk);
    blk = next;
  }
oom2:
  ZSTD_freeCCtx(ctx);
oom1:
  caml_raise_out_of_memory();
}

CAMLprim value caml_compressed_marshal_to_channel(value vchan,
                                                  value v, value flags)
{
  CAMLparam3(vchan, v, flags);
  struct channel * chan = Channel(vchan);
  caml_channel_lock(chan);
  /* Marshal the value to a header and a list of blocks */
  char header[MAX_INTEXT_HEADER_SIZE];
  int header_len;
  struct output_block * uncompressed;
  uintnat uncompressed_len;
  caml_output_value_to_blocks(v, flags, header, &header_len,
                              &uncompressed, &uncompressed_len);
  uncompressed_len += header_len;
  /* Compress the marshaled data */
  struct output_block * compressed;
  uintnat compressed_len;
  compress_blocks(header, header_len, uncompressed,
                  &compressed, &compressed_len);
  /* Write our header and the compressed marshaled data */
  caml_putword(chan, Intext_magic_number_compressed);
  putvlq(chan, compressed_len);
  putvlq(chan, uncompressed_len);
  for (struct output_block * blk = compressed; blk != NULL; /*nothing*/) {
    caml_really_putblock(chan, blk->data, blk->end - blk->data);
    struct output_block * nextblk = blk->next;
    caml_stat_free(blk);
    blk = nextblk;
  }
  caml_flush_if_unbuffered(chan);
  caml_channel_unlock(chan);
  CAMLreturn(Val_unit);
}

static value caml_input_compressed_val(struct channel * chan)
{
  /* Read the header */
  uint32_t magic = caml_getword(chan);
  if (magic != Intext_magic_number_compressed) {
#if 0
    caml_failwith("Compression.Marshal.from_channel: bad object");
#else
    /* Assume data is not compressed and retry.
       Works only if [chan] is open on a file. */
    caml_seek_in(chan, caml_pos_in(chan) - 4);
    return caml_input_val(chan);
#endif
  }
  uintnat compressed_len, uncompressed_len;
  if (getvlq(chan, &compressed_len) == -1
      || getvlq(chan, &uncompressed_len) == -1)
    caml_failwith("Compression.Marshal.from_channel: "
                  "object too large to be read back on a 32-bit platform");
  /* Read the compressed data and decompress on the fly */
  char * uncompressed = malloc(uncompressed_len);
  if (uncompressed == NULL) goto oom1;
  ZSTD_DStream * ctx = ZSTD_createDStream();
  if (ctx == NULL) goto oom2;
  ZSTD_outBuffer out;
  out.dst = uncompressed; out.size = uncompressed_len; out.pos = 0;
  ZSTD_inBuffer in;
  char buf[IO_BUFFER_SIZE];
  in.src = buf;
  uintnat remaining = compressed_len;
  while (remaining > 0) {
    int nread =
      caml_getblock(chan, buf,
                    remaining < IO_BUFFER_SIZE ? remaining : IO_BUFFER_SIZE);
    if (nread == 0) goto truncated;
    remaining -= nread;
    in.size = nread; in.pos = 0;
    do {
      size_t rc = ZSTD_decompressStream(ctx, &out, &in);
      if (ZSTD_isError(rc)) goto zstd_error;
    } while (in.pos < in.size);
  }
  if (out.pos < out.size) goto zstd_error;
  ZSTD_freeDStream(ctx);
  /* Unmarshal the uncompressed data */
  value v = caml_input_value_from_malloc(uncompressed, 0);
  /* uncompressed is freed by caml_input_value_from_malloc */
  return v;
 zstd_error:
  ZSTD_freeDStream(ctx);
  free(uncompressed);
  caml_failwith("Compression.Marshal.from_channel: decompression error");
 truncated:
  ZSTD_freeDStream(ctx);
  free(uncompressed);
  caml_failwith("Compression.Marshal.from_channel: truncated file");
 oom2:
  free(uncompressed);
 oom1:
  caml_raise_out_of_memory();
}

#else

value caml_compression_supported(value vunit) { return Val_false; }

value caml_compressed_marshal_to_channel(value vchan, value v, value flags)
{
  caml_invalid_argument("Compression.Marshal.to_channel: unsupported");
}

value caml_input_compressed_val(struct channel * chan)
{
  return caml_input_val(chan);
}

#endif

CAMLprim value caml_compressed_marshal_from_channel(value vchan)
{
  CAMLparam1(vchan);
  struct channel * chan = Channel(vchan);
  caml_channel_lock(chan);
  value v = caml_input_compressed_val(chan);
  caml_channel_unlock(chan);
  CAMLreturn(v);
}
