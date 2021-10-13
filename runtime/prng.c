/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy, projet Cambium, College de France and Inria     */
/*                                                                        */
/*   Copyright 2021 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/mlvalues.h"

/* The L64X128 member of the LXM family.  Taken from figure 1 in
   "LXM: Better Splittable Pseudorandom Number Generators
    (and Almost as Fast)" by Guy L. Steele Jr. and Sebastiano Vigna,
    OOPSLA 2021.  */

static const uint64_t M = 0xd1342543de82ef95;

struct LXM_state {
  uint64_t a;            /* per-instance additive parameter (odd) */
  uint64_t s;            /* state of the LCG subgenerator */
  uint64_t x[2];         /* state of the XBG subgenerator (not 0) */
};

/* In OCaml, states are represented as a 1D big array of 64-bit integers */

#define LXM_val(v) ((struct LXM_state *) Caml_ba_data_val(v))

static inline uint64_t rotl(const uint64_t x, int k) {
  return (x << k) | (x >> (64 - k));
}

CAMLprim uint64_t caml_lxm_next_unboxed(value v)
{
  uint64_t z, q0, q1;
  struct LXM_state * st = LXM_val(v);

  /* Combining operation */
  z = st->s + st->x[0];
  /* Mixing function */
  z = (z ^ (z >> 32)) * 0xdaba0b6eb09322e3;
  z = (z ^ (z >> 32)) * 0xdaba0b6eb09322e3;
  z = (z ^ (z >> 32));
  /* LCG update */
  st->s = st->s * M + st->a;
  /* XBG update */
  q0 = st->x[0]; q1 = st->x[1];
  q1 ^= q0;
  q0 = rotl(q0, 24);
  q0 = q0 ^ q1 ^ (q1 << 16);
  q1 = rotl(q1, 37);
  st->x[0] = q0; st->x[1] = q1;
  /* Return result */
  return z;
}

CAMLprim value caml_lxm_next(value v)
{
  return caml_copy_int64(caml_lxm_next_unboxed(v));
}

static void caml_lxm_set(value v, uint64_t i1, uint64_t i2,
                                  uint64_t i3, uint64_t i4)
{
  struct LXM_state * st = LXM_val(v);
  st->a = i1 | 1;              /* must be odd */
  st->x[0] = i2 != 0 ? i2 : 1; /* must be nonzero */
  st->x[1] = i3 != 0 ? i3 : 2; /* must be nonzero */
  st->s = i4;
}

CAMLprim value caml_lxm_init(value v, value a)
{
  const uint64_t mix = 6364136223846793005;
  /* Multiplier taken from the MMIX LCG, Knoth TAOCP vol 2, 1998 edition */
  uint64_t d[4] = {0, 0, 0, 0};
  mlsize_t i, len;

  for (i = 0, len = Wosize_val(a); i < len; i++) {
    d[i % 4] = d[i % 4] * mix + Long_val(Field(a, i));
  }
  caml_lxm_set(v, d[0], d[1], d[2], d[3]);
  return Val_unit;
}

CAMLprim value caml_lxm_init_split(value vnew, value vold)
{
  uint64_t i1 = caml_lxm_next_unboxed(vold);
  uint64_t i2 = caml_lxm_next_unboxed(vold);
  uint64_t i3 = caml_lxm_next_unboxed(vold);
  uint64_t i4 = caml_lxm_next_unboxed(vold);
  caml_lxm_set(vnew, i1, i2, i3, i4);
  return Val_unit;
}
