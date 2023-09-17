/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, Collège de France and Inria                  */
/*                                                                        */
/*   Copyright 2023 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* ZSTD compression/decompression functions, with dynamic loading */

#ifndef CAML_COMPRESS_H
#define CAML_COMPRESS_H

#include "config.h"
#include "misc.h"

CAMLextern _Bool caml_zstd_available(void);

#ifdef HAS_ZSTD

#include <zstd.h>

struct zstd_operations {
  ZSTD_CCtx* (*createCCtx)(void);
  size_t (*compressStream2)(ZSTD_CCtx* cctx,
                            ZSTD_outBuffer* output,
                            ZSTD_inBuffer* input,
                            ZSTD_EndDirective endOp);
  size_t (*freeCCtx)(ZSTD_CCtx* cctx);
  size_t (*decompress)(void* dst, size_t dstCapacity,
                       const void* src, size_t compressedSize);
};

CAMLextern struct zstd_operations caml_zstd_ops;

#endif

#endif
