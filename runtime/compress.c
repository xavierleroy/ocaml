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

#define CAML_INTERNALS

#include "caml/compress.h"
#include "caml/config.h"
#include "caml/misc.h"
#include "caml/osdeps.h"

#ifdef HAS_ZSTD

// FIXME atomic updates and/or locking
static enum { UNKNOWN = -1, UNAVAILABLE = 0, AVAILABLE = 1 }
  caml_zstd_status = UNKNOWN;
static void * caml_zstd_handle = NULL;
struct zstd_operations caml_zstd_ops;

// FIXME Windows
#define ZSTD_DLL_NAME "libzstd.so"

_Bool caml_zstd_available(void)
{
  if (caml_zstd_status != UNKNOWN)
    return caml_zstd_status;
  if (! (caml_zstd_handle = caml_dlopen(ZSTD_DLL_NAME, 0)))
    goto err;
  if (! (caml_zstd_ops.createCCtx =
           caml_dlsym(caml_zstd_handle, "ZSTD_createCCtx")))
    goto err;
  if (! (caml_zstd_ops.compressStream2 =
           caml_dlsym(caml_zstd_handle, "ZSTD_compressStream2")))
    goto err;
  if (! (caml_zstd_ops.freeCCtx =
           caml_dlsym(caml_zstd_handle, "ZSTD_freeCCtx")))
    goto err;
  if (! (caml_zstd_ops.decompress =
           caml_dlsym(caml_zstd_handle, "ZSTD_decompress")))
    goto err;
  caml_zstd_status = AVAILABLE;
  return 1;
 err:
  caml_zstd_status = UNAVAILABLE;
  return 0;
}

#else

_Bool caml_zstd_available(void)
{
  return 0;
}

#endif
