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

value caml_compression_supported(value vunit) { return Val_true; }

static char magic_number[4] = {
  (char) (Intext_magic_number_compressed >> 24),
  (char) (Intext_magic_number_compressed >> 16),
  (char) (Intext_magic_number_compressed >> 8),
  (char) Intext_magic_number_compressed
};

#define MAX_HEADER_SIZE (4 + 1 + 10 + 10)

/* Header format:
       4 bytes   magic_number
       1 byte    total length of header, in bytes
       N bytes   length of compressed data, in bytes, VLQ format
       M bytes   length of uncompressed data, in bytes, VLQ format

  VLQ format is one or several bytes like 1xxxxxxx 1yyyyyyy 0zzzzzzz.
  First bytes have top bit 1, last byte has top bit 0.
  Each byte carries 7 bits of the number.
  Bytes come in big-endian order: xxxxxxx are the 7 high-order bits,
  zzzzzzzz the 7 low-order bits.
*/

static int storevlq(char * dst, uintnat n)
{
  /* Find number of base-128 digits (always at least one) */
  int ndigits = 1;
  for (uintnat m = n >> 7; m != 0; m >>= 7) ndigits++;
  /* Convert number */
  dst += ndigits - 1;
  *dst = n & 0x7F;
  for (n >>= 7; n != 0; n >>= 7) *--dst = 0x80 | (n & 0x7F);
  /* Return length of number */
  return ndigits;
}

static int storeheader(char dst[MAX_HEADER_SIZE],
                       uintnat compressed_len, uintnat uncompressed_len)
{
  memcpy(dst, magic_number, 4);
  int pos = 5;
  pos += storevlq(dst + pos, compressed_len);
  pos += storevlq(dst + pos, uncompressed_len);
  dst[4] = pos;
  return pos;
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

value caml_compressed_marshal_to_channel(value vchan, value v, value flags)
{
  CAMLparam3(vchan, v, flags);
  struct channel * chan = Channel(vchan);
  caml_channel_lock(chan);
  /* Marshal the value to a header and a list of blocks */
  char header1[MAX_INTEXT_HEADER_SIZE];
  int header1_len;
  struct output_block * uncompressed;
  uintnat uncompressed_len;
  caml_output_value_to_blocks(v, flags, header1, &header1_len, &uncompressed, &uncompressed_len);
  uncompressed_len += header_len;
  /* Compress the marshaled data */
  struct output_block * compressed;
  uintnat compressed_len;
  compress_blocks(header, header_len, uncompressed, &compressed, &compressed_len);
  /* Write our header and the compressed marshaled data */
  char header2[MAX_HEADER_SIZE];
  int header2_len = storeheader(header2, compressed_len, uncompressed_len);
  caml_really_putblock(chan, header2, header2_len);
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

static int readvlq(char * src, /*out*/ uintnat * res)
{
  int pos = 0;
  unsigned char c = src[pos++];
  uintnat n = c & 0x7F;
  while ((c & 0x80) != 0) {
    c = src[pos++];
    uintnat n7 = n << 7;
    if (n != n7 >> 7) return -1;
    n = n7 | (c & 0x7F);
  }
  *res = n;
  return pos;
}

static int readheader(char src[MAX_HEADER_SIZE],
                      uintnat * compressed_len, uintnat * uncompressed_len)
{
  int pos = 5;
  int len = readvlq(src + pos, compressed_len);
  if (len == -1) return -1;
  pos += len;
  len = readvlq(src + pos, uncompressed_len);
  if (len == -1) return -1;
  pos += len;
  return pos;
}

value caml_compressed_marshal_from_channel(value vchan)
{
  CAMLparam1(vchan);
  struct channel * chan = Channel(vchan);
  caml_channel_lock(chan);
  /* Read the header.  This is subtle because it has variable length. */
  char header[MAX_HEADER_SIZE];
  int r = caml_really_getblock(chan, header, 5);
  if (r == 0) caml_raise_end_of_file();
  if (r < 5) goto truncated1;
  if (memcmp(header, magic_number, 4) != 0) caml_failwith("bad magic number");
  int header_len = header[4];
  CAMLassert(header_len >= 5 && header_len <= MAX_HEADER_SIZE);
  if (caml_really_getblock(chan, header + 5, header_len - 5) != header_len - 5)
    goto truncated1;
  /* Determine compressed and uncompressed sizes */
  uintnat compressed_len, uncompressed_len;
  r = readheader(header, &compressed_len, &uncompressed_len);
  if (r == -1) caml_failwith("too big for a 32-bit machine");
  CAMLassert (r == header_len);
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
    if (nread == 0) goto truncated2;
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
  caml_channel_unlock(chan);
  CAMLreturn(v);
 zstd_error:
  ZSTD_freeDStream(ctx);
  free(uncompressed);
  caml_failwith("Compression.Marshal.from_channel: decompression error");
 truncated2:
  ZSTD_freeDStream(ctx);
  free(uncompressed);
 truncated1:
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

value caml_compressed_marshal_from_channel(value vchan)
{
  caml_invalid_argument("Compression.Marshal.from_channel: unsupported");
}

#endif

