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

value caml_compressed_marshal_to_channel(value vchan, value v, value flags)
{
  CAMLparam3(vchan, v, flags);
  struct channel * chan = Channel(vchan);
  caml_channel_lock(chan);
  /* Marshal the value to a malloc-ed block */
  char * uncompressed;
  intnat uncompressed_len;
  caml_output_value_to_malloc(v, flags, &uncompressed, &uncompressed_len);
  /* Compress the marshaled data */
  uintnat max_compressed_len = ZSTD_compressBound(uncompressed_len);
  char * compressed = malloc(max_compressed_len);
  if (compressed == NULL) { free(uncompressed); caml_raise_out_of_memory(); }
  intnat compressed_len =
    ZSTD_compress(compressed, max_compressed_len,
                  uncompressed, uncompressed_len,
                  ZSTD_CLEVEL_DEFAULT);
  free(uncompressed);
  /* Write header and compressed marshaled data */
  char header[MAX_HEADER_SIZE];
  int header_len = storeheader(header, compressed_len, uncompressed_len);
  caml_really_putblock(chan, header, header_len);
  caml_really_putblock(chan, compressed, compressed_len);
  free(compressed);
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
  /* Read the compressed data */
  char * compressed = malloc(compressed_len);
  if (compressed == NULL) goto oom1;
  if (caml_really_getblock(chan, compressed, compressed_len) != compressed_len)
    goto truncated2;
  /* Uncompress */
  char * uncompressed = malloc(uncompressed_len);
  if (uncompressed == NULL) goto oom2;
  size_t actual_len = ZSTD_decompress(uncompressed, uncompressed_len,
                                      compressed, compressed_len);
  free(compressed);
  if (actual_len != uncompressed_len) {
    free(uncompressed); caml_failwith("decompression error");
  }
  /* Unmarshal the uncompressed data */
  value v = caml_input_value_from_malloc(uncompressed, 0);
  /* uncompressed is freed by caml_input_value_from_malloc */
  caml_channel_unlock(chan);
  CAMLreturn(v);
 truncated2:
  free(compressed);
 truncated1:
  caml_failwith("truncated file");
 oom2:
  free(compressed);
 oom1:
  caml_raise_out_of_memory();
}

#else

value caml_compression_supported(value vunit) { return Val_false; }

value caml_compressed_marshal_to_channel(value vchan, value v, value flags)
{
  caml_invalid_argument("Compressed.Marshal.to_channel: unsupported");
}

value caml_compressed_marshal_from_channel(value vchan)
{
  caml_invalid_argument("Compressed.Marshal.from_channel: unsupported");
}

#endif

