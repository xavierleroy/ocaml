/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Machine-dependent interface with the asm code */

#ifndef CAML_STACK_H
#define CAML_STACK_H

#ifdef CAML_INTERNALS

/* Macros to access the stack frame by frame */

/* If [sp] points to a stack frame for an activation of function [f],
   [Saved_return_address(sp)] is the return address into [f],
   either from a function call or a GC interrupt.

   If [sp] points to the bottom of an OCaml stack,
   [First_frame(sp)] is the first stack frame in this stack.
*/

#ifdef TARGET_power
/* Size of the gc_regs structure, in words.
   See power.S and power/proc.ml for the indices */
#define Wosize_gc_regs (23 /* int regs */ + 14 /* caller-save float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#define First_frame(sp) (sp)
#endif

#ifdef TARGET_s390x
#define Wosize_gc_regs (2 + 9 /* int regs */ + 16 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 8)
#endif

#ifdef TARGET_amd64
/* Size of the gc_regs structure, in words.
   See amd64.S and amd64/proc.ml for the indices */
#define Wosize_gc_regs (13 /* int regs */ + 16 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#ifdef WITH_FRAME_POINTERS
#define First_frame(sp) ((sp) + 16)
#else
#define First_frame(sp) ((sp) + 8)
#endif
#endif

#ifdef TARGET_arm64
/* Size of the gc_regs structure, in words.
   See arm64.S and arm64/proc.ml for the indices */
#define Wosize_gc_regs (2 + 24 /* int regs */ + 24 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 16)
#endif

#ifdef TARGET_riscv
/* Size of the gc_regs structure, in words.
   See riscv.S and riscv/proc.ml for the indices */
#define Wosize_gc_regs (2 + 22 /* int regs */ + 20 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 16)
#endif

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern intnat caml_globals_inited;

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
