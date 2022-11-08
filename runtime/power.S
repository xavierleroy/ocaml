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

        .abiversion 2

/* Special registers */
#define SP 1
#define TOC 2
#define TMP 11
#define TMP2 12
#define START_PRG_ARG 12
#define START_PRG_DOMAIN_STATE_PTR 7
#define STACK_ARG_BEGIN 23
#define STACK_ARG_END 24
#define C_CALL_FUN 25
#define C_CALL_TOC 26
#define C_CALL_RET_ADDR 27
#define C_CALL_TMP 28
#define TRAP_PTR 29
#define DOMAIN_STATE_PTR 30
#define ALLOC_PTR 31

/* Stack layout */
#define RESERVED_STACK 32
#define PARAM_SAVE_AREA 0
#define LR_SAVE 16
#define TOC_SAVE_PARENT 8
#define TOC_SAVE 24
#define TRAP_SIZE 32
#define TRAP_HANDLER_OFFSET 40
#define TRAP_PREVIOUS_OFFSET 48
#define CALLBACK_LINK_SIZE 32
#define CALLBACK_LINK_OFFSET 32

/* struct stack_info */
#define Stack_sp(reg)           0(reg)
#define Stack_sp_offset         0
#define Stack_exception(reg)    8(reg)
#define Stack_handler(reg)      16(reg)
#define Stack_handler_from_cont(reg) 15(reg)

/* struct c_stack_link */
#define Cstack_stack(reg)       0(reg)
#define Cstack_sp(reg)          8(reg)
#define Cstack_sp_offset        8
#define Cstack_prev(reg)        16(reg)

/* struct stack_handler */
#define Handler_value(reg)      0(reg)
#define Handler_exception(reg)  8(reg)
#define Handler_effect(reg)     16(reg)
#define Handler_parent(reg)     24(reg)
#define Handler_parent_offset   24

/* Function definitions */

#define FUNCTION(name) \
  .section ".text"; \
  .globl name; \
  .type name, @function; \
  .align 2; \
  name: ; \
  0: addis 2, 12, (.TOC. - 0b)@ha; \
  addi 2, 2, (.TOC. - 0b)@l; \
  .localentry name, . - 0b

#define ENDFUNCTION(name) \
  .size name, . - name

/* Accessing global variables.  */

#define LSYMB(glob) .L##glob

#define Addrglobal(reg,glob) \
        ld      reg, LSYMB(glob)@toc(TOC)

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) 8*domain_field_caml_##var(DOMAIN_STATE_PTR)

/* Switch from OCaml stack to C stack */

.macro SWITCH_OCAML_TO_C
   /* Fill in Caml_state->current_stack->sp */
       ld      TMP, Caml_state(current_stack)
       std     SP, Stack_sp(TMP)
   /* Fill in Caml_state->c_stack */
       ld      TMP2, Caml_state(c_stack)
       std     TMP, Cstack_stack(TMP2)
       std     SP, Cstack_sp(TMP2)
   /* Switch to C stack */
       mr      SP, TMP2
.endm

/* Switch from C stack to OCaml stack */

.macro SWITCH_C_TO_OCAML
       ld      TMP, Cstack_sp(SP)
       mr      SP, TMP
.endm

/* Save ALLOC_PTR and TRAP_PTR to domain state, and save
   the registers used by the code generator to a free gc_regs bucket.
   Address of bucket is then written to Caml_state(gc_regs) */

.macro SAVE_ALL_REGS
    /* Save allocation pointer and exception pointer */
       std      ALLOC_PTR, Caml_state(young_ptr)
       std      TRAP_PTR, Caml_state(exn_handler)
    /* Point TMP to the gc_regs bucket and skip to next bucket */
       ld       TMP, Caml_state(gc_regs_bucket)
       ld       0, 0(TMP)
       std      0, Caml_state(gc_regs_bucket)
    /* Save all allocatable integer registers */
       std      3,  0x000(TMP)
       std      4,  0x008(TMP)
       std      5,  0x010(TMP)
       std      6,  0x018(TMP)
       std      7,  0x020(TMP)
       std      8,  0x028(TMP)
       std      9,  0x030(TMP)
       std      10, 0x038(TMP)
       std      14, 0x040(TMP)
       std      15, 0x048(TMP)
       std      16, 0x050(TMP)
       std      17, 0x058(TMP)
       std      18, 0x060(TMP)
       std      19, 0x068(TMP)
       std      20, 0x070(TMP)
       std      21, 0x078(TMP)
       std      22, 0x080(TMP)
       std      23, 0x088(TMP)
       std      24, 0x090(TMP)
       std      25, 0x098(TMP)
       std      26, 0x0A0(TMP)
       std      27, 0x0A8(TMP)
       std      28, 0x0B0(TMP)
    /* Save caller-save floating-point registers
       (callee-saves are preserved by C functions) */
       stfd     1,  0x0B8(TMP)
       stfd     2,  0x0C0(TMP)
       stfd     3,  0x0C8(TMP)
       stfd     4,  0x0D0(TMP)
       stfd     5,  0x0D8(TMP)
       stfd     6,  0x0E0(TMP)
       stfd     7,  0x0E8(TMP)
       stfd     8,  0x0F0(TMP)
       stfd     9,  0x0F8(TMP)
       stfd     10, 0x100(TMP)
       stfd     11, 0x108(TMP)
       stfd     12, 0x110(TMP)
       stfd     13, 0x118(TMP)
       stfd     14, 0x120(TMP)
    /* Save bucket to gc_regs */
       std      TMP, Caml_state(gc_regs)
.end

/* Undo SAVE_ALL_REGS: load all the registers saved to Caml_state(gc_regs)
   and refresh ALLOC_PTR and TRAP_PTR from Caml_state */
.macro RESTORE_ALL_REGS
       ldr      TMP, Caml_state(gc_regs)
    /* Restore all allocatable integer registers */
       ld       3,  0x000(TMP)
       ld       4,  0x008(TMP)
       ld       5,  0x010(TMP)
       ld       6,  0x018(TMP)
       ld       7,  0x020(TMP)
       ld       8,  0x028(TMP)
       ld       9,  0x030(TMP)
       ld       10, 0x038(TMP)
       ld       14, 0x040(TMP)
       ld       15, 0x048(TMP)
       ld       16, 0x050(TMP)
       ld       17, 0x058(TMP)
       ld       18, 0x060(TMP)
       ld       19, 0x068(TMP)
       ld       20, 0x070(TMP)
       ld       21, 0x078(TMP)
       ld       22, 0x080(TMP)
       ld       23, 0x088(TMP)
       ld       24, 0x090(TMP)
       ld       25, 0x098(TMP)
       ld       26, 0x0A0(TMP)
       ld       27, 0x0A8(TMP)
       ld       28, 0x0B0(TMP)
    /* Save caller-save floating-point registers
       (callee-saves are preserved by C functions) */
       lfd      1,  0x0B8(TMP)
       lfd      2,  0x0C0(TMP)
       lfd      3,  0x0C8(TMP)
       lfd      4,  0x0D0(TMP)
       lfd      5,  0x0D8(TMP)
       lfd      6,  0x0E0(TMP)
       lfd      7,  0x0E8(TMP)
       lfd      8,  0x0F0(TMP)
       lfd      9,  0x0F8(TMP)
       lfd      10, 0x100(TMP)
       lfd      11, 0x108(TMP)
       lfd      12, 0x110(TMP)
       lfd      13, 0x118(TMP)
       lfd      14, 0x120(TMP)
    /* Put gc_regs struct back in bucket linked list */
       ld       TMP2, Caml_state(gc_regs_buckets)
       std      TMP2, 0(TMP)  /* next ptr */
       std      TMP, Caml_state(gc_regs_buckets)
    /* Reload new allocation pointer and exception pointer */
       ld       ALLOC_PTR, Caml_state(young_ptr)
       ld       TRAP_PTR, Caml_state(exn_handler)
.end

        .section ".text"
        .globl  caml_system__code_begin
caml_system__code_begin:

/* Reallocate the stack when it is too small. */
/* Desired size is passed in register TMP2. */

FUNCTION(caml_call_realloc_stack)
   /* Allocate the minimal frame that the caller did not allocate yet.
      Save return address there. */
        addi    SP, SP, -RESERVED_SIZE
        mflr    0
        std     0, LR_SAVE(SP)
   /* Save all registers, as well as ALLOC_PTR and TRAP_PTR */
        SAVE_ALL_REGS  /* TMP2 is preserved */
   /* Recover desired size, to be passed in r3 */
        mr      3, TMP2
   /* Switch stacks and call caml_try_realloc_stack */
        SWITCH_OCAML_TO_C
        addi    SP, SP, -RESERVED_STACK
        bl      caml_try_realloc_stack
        SWITCH_C_TO_OCAML
        cmpdi   3, 0
   /* Restore all registers, and also return address and TOC register */
        RESTORE_ALL_REGS
        ld      0, LR_SAVE(SP)
        mtlr    0
        addi    SP, SP, RESERVED_SIZE
   /* Check status */
        beq     1f
   /* Reallocation successful: return to caller */
        ld      TOC, TOC_SAVE(SP)    /* restore caller's TOC */
        blr
   /* Reallocation failed: raise the Stack_overflow exception */
1:      Addrglobal(3, caml_exn_Stack_overflow)
        b       caml_raise_exn        
ENDFUNCTION(caml_call_realloc_stack)

/* Invoke the garbage collector. */

FUNCTION(caml_call_gc)
   /* Save return address in caller's frame */
        mflr    0
        std     0, LR_SAVE(SP)
   /* Save all registers, as well as ALLOC_PTR and TRAP_PTR */
        SAVE_ALL_REGS
   /* Switch stacks and call caml_garbage_collection */
        SWITCH_OCAML_TO_C
        addi    SP, SP, -RESERVED_STACK
        bl      caml_garbage_collection
        SWITCH_C_TO_OCAML
   /* Restore registers and return to caller */
        RESTORE_ALL_REGS
        ld      0, LR_SAVE(SP)
        mtlr    0
        ld      TOC, TOC_SAVE(SP)
        blr
ENDFUNCTION(caml_call_gc)

/* Call a C function from OCaml.  Function to call is in C_CALL_FUN */

FUNCTION(caml_c_call)
   /* Save return address in caller's frame AND in a callee-save register */
        mflr    C_CALL_RET_ADDR
        std     C_CALL_RET_ADDR, LR_SAVE(SP)
   /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
   /* Make the exception handler and alloc ptr available to the C code */
        std     ALLOC_PTR, Caml_state(young_ptr)
        std     TRAP_PTR, Caml_state(exception_pointer)
    /* Call C function (address in C_CALL_FUN) */
        addi    SP, SP, -RESERVED_STACK
        mtctr   C_CALL_FUN
        mr      12, C_CALL_FUN
        mr      C_CALL_TOC, 2   /* save current TOC in a callee-save register */
        bctrl
        mr      2, C_CALL_TOC   /* restore current TOC */
    /* Restore return address (in register C_CALL_RET_ADDR, preserved by C) */
        mtlr    C_CALL_RET_ADDR
    /* Reload allocation pointer*/
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return to caller */
        blr
ENDFUNCTION(caml_c_call)

FUNCTION(caml_c_call_stack_args)
   /* Extra arguments to be passed on stack: 
      at offsets STACK_ARG_BEGIN to STACK_ARG_END from SP */
        mr      C_CALL_TMP, SP
   /* Save return address in caller's frame AND in a callee-save register */
        mflr    C_CALL_RET_ADDR
        std     C_CALL_RET_ADDR, LR_SAVE(SP)
   /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C
   /* Make the exception handler and alloc ptr available to the C code */
        std     ALLOC_PTR, Caml_state(young_ptr)
        std     TRAP_PTR, Caml_state(exception_pointer)
   /* Set up a C stack frame that mimicks the frame on the OCaml stack.
          arguments passed on stack
          reserved space for arguments passed in registers
          32 bytes of standard reserved space */
        addi    0, STACK_ARG_END, 8
        andi.   0, 0, -16    /* align to 16 */
        subfc   SP, 0, SP    /* allocate stack frame */
        /* Copy from original SP + [STACK_ARG_BEGIN...STACK_ARG_END)
           to new SP + [STACK_ARG_BEGIN...STACK_ARG_END) */
1:      ldx     0, C_CALL_TMP, STACK_ARG_BEGIN        
        stdx    0, SP, STACK_ARG_END
        addi    STACK_ARG_BEGIN, STACK_ARG_BEGIN, 8
        cmpd    STACK_ARG_BEGIN, STACK_ARG_END
    /* Call C function (address in C_CALL_FUN) */
        mtctr   C_CALL_FUN
        mr      12, C_CALL_FUN
        mr      C_CALL_TOC, 2   /* save current TOC in a callee-save register */
        bctrl
        mr      2, C_CALL_TOC   /* restore current TOC */
    /* Restore return address (in register C_CALL_RET_ADDR, preserved by C) */
        mtlr    C_CALL_RET_ADDR
    /* Reload allocation pointer*/
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Switch from C to OCaml */
        SWITCH_C_TO_OCAML
    /* Return to caller */
        blr
ENDFUNCTION(caml_c_call_stack_args)


/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        ld      0, Caml_state(backtrace_active)
        cmpwi   0, 0
        bne     .L111
.L110:
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(TRAP_PTR)
        mr      1, TRAP_PTR
        mtctr   0
        ld      TRAP_PTR, TRAP_PREVIOUS_OFFSET(1)
        addi    1, 1, TRAP_SIZE
    /* Branch to handler */
        bctr
.L111:
        mr      27, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        mflr    4               /* arg2: PC of raise */
        mr      5, 1            /* arg3: SP of raise */
        mr      6, TRAP_PTR     /* arg4: SP of handler */
        addi    1, 1, -(PARAM_SAVE_AREA + RESERVED_STACK)
                                /* reserve stack space for C call */
        bl      caml_stash_backtrace
        nop
        mr      3, 27           /* restore exn bucket */
        b       .L110           /* raise the exn */
ENDFUNCTION(caml_raise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
    /* Load domain state pointer */
        mr      DOMAIN_STATE_PTR, 3
        mr      3, 4
        ld      0, Caml_state(backtrace_active)
        cmpwi   0, 0
        bne     .L121
.L120:
    /* Reload OCaml global registers */
        ld      1, Caml_state(exception_pointer)
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(1)
        mtctr   0
        ld      TRAP_PTR, TRAP_PREVIOUS_OFFSET(1)
        addi    1, 1, TRAP_SIZE
    /* Branch to handler */
        bctr
.L121:
        mr      27, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        ld      4, Caml_state(last_return_address) /* arg2: PC of raise */
        ld      5, Caml_state(bottom_of_stack)     /* arg3: SP of raise */
        ld      6, Caml_state(exception_pointer)   /* arg4: SP of handler */
        addi    1, 1, -(PARAM_SAVE_AREA + RESERVED_STACK)
                                         /* reserve stack space for C call */
        bl      caml_stash_backtrace
        nop
        mr      3, 27           /* restore exn bucket */
        b       .L120           /* raise the exn */
ENDFUNCTION(caml_raise_exception)

/* Start the OCaml program */

FUNCTION(caml_start_program)
        .cfi_startproc
#define STACKSIZE (8*18 + 8*18 + CALLBACK_LINK_SIZE + RESERVED_STACK)
  /* 18 callee-save GPR14...GPR31 + 18 callee-save FPR14...FPR31 */
  /* Domain state pointer is the first arg to caml_start_program. Move it */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        Addrglobal(START_PRG_ARG, caml_program)
/* Code shared between caml_start_program and caml_callback */
.L102:
    /* Allocate and link stack frame */
        stdu    1, -STACKSIZE(1)
        .cfi_adjust_cfa_offset STACKSIZE
    /* Save return address */
        mflr    0
        std     0, (STACKSIZE + LR_SAVE)(1)
        .cfi_offset 65, LR_SAVE
    /* Save TOC pointer */
        std     2, (STACKSIZE + TOC_SAVE_PARENT)(1)
    /* Save all callee-save registers */
        addi    11, 1, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        stdu    14, 8(11)
        stdu    15, 8(11)
        stdu    16, 8(11)
        stdu    17, 8(11)
        stdu    18, 8(11)
        stdu    19, 8(11)
        stdu    20, 8(11)
        stdu    21, 8(11)
        stdu    22, 8(11)
        stdu    23, 8(11)
        stdu    24, 8(11)
        stdu    25, 8(11)
        stdu    26, 8(11)
        stdu    27, 8(11)
        stdu    28, 8(11)
        stdu    29, 8(11)
        stdu    30, 8(11)
        stdu    31, 8(11)
        stfdu   14, 8(11)
        stfdu   15, 8(11)
        stfdu   16, 8(11)
        stfdu   17, 8(11)
        stfdu   18, 8(11)
        stfdu   19, 8(11)
        stfdu   20, 8(11)
        stfdu   21, 8(11)
        stfdu   22, 8(11)
        stfdu   23, 8(11)
        stfdu   24, 8(11)
        stfdu   25, 8(11)
        stfdu   26, 8(11)
        stfdu   27, 8(11)
        stfdu   28, 8(11)
        stfdu   29, 8(11)
        stfdu   30, 8(11)
        stfdu   31, 8(11)
    /* Load domain state pointer from argument */
        mr      DOMAIN_STATE_PTR, START_PRG_DOMAIN_STATE_PTR
    /* Set up a callback link */
        ld      11, Caml_state(bottom_of_stack)
        std     11, CALLBACK_LINK_OFFSET(1)
        ld      11, Caml_state(last_return_address)
        std     11, (CALLBACK_LINK_OFFSET + 8)(1)
        ld      11, Caml_state(gc_regs)
        std     11, (CALLBACK_LINK_OFFSET + 2 * 8)(1)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        bl      .L103
        b       .L104
.L103:
        addi    1, 1, -TRAP_SIZE
        .cfi_adjust_cfa_offset TRAP_SIZE
        mflr    0
        std     0, TRAP_HANDLER_OFFSET(1)
        ld      11, Caml_state(exception_pointer)
        std     11, TRAP_PREVIOUS_OFFSET(1)
        mr      TRAP_PTR, 1
    /* Reload allocation pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Call the OCaml code (address in r12) */
        mtctr   12
        std     2, TOC_SAVE(1)
.L105:  bctrl
        ld      2, TOC_SAVE(1)
    /* Pop the trap frame, restoring caml_exception_pointer */
        ld      0, TRAP_PREVIOUS_OFFSET(1)
        std     0, Caml_state(exception_pointer)
        addi    1, 1, TRAP_SIZE
        .cfi_adjust_cfa_offset -TRAP_SIZE
    /* Pop the callback link, restoring the global variables */
.L106:
        ld      0, CALLBACK_LINK_OFFSET(1)
        std     0, Caml_state(bottom_of_stack)
        ld      0, (CALLBACK_LINK_OFFSET + 8)(1)
        std     0, Caml_state(last_return_address)
        ld      0, (CALLBACK_LINK_OFFSET + 2 * 8)(1)
        std     0, Caml_state(gc_regs)
    /* Update allocation pointer */
        std     ALLOC_PTR, Caml_state(young_ptr)
    /* Restore callee-save registers */
        addi    11, 1, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        ldu     14, 8(11)
        ldu     15, 8(11)
        ldu     16, 8(11)
        ldu     17, 8(11)
        ldu     18, 8(11)
        ldu     19, 8(11)
        ldu     20, 8(11)
        ldu     21, 8(11)
        ldu     22, 8(11)
        ldu     23, 8(11)
        ldu     24, 8(11)
        ldu     25, 8(11)
        ldu     26, 8(11)
        ldu     27, 8(11)
        ldu     28, 8(11)
        ldu     29, 8(11)
        ldu     30, 8(11)
        ldu     31, 8(11)
        lfdu    14, 8(11)
        lfdu    15, 8(11)
        lfdu    16, 8(11)
        lfdu    17, 8(11)
        lfdu    18, 8(11)
        lfdu    19, 8(11)
        lfdu    20, 8(11)
        lfdu    21, 8(11)
        lfdu    22, 8(11)
        lfdu    23, 8(11)
        lfdu    24, 8(11)
        lfdu    25, 8(11)
        lfdu    26, 8(11)
        lfdu    27, 8(11)
        lfdu    28, 8(11)
        lfdu    29, 8(11)
        lfdu    30, 8(11)
        lfdu    31, 8(11)
    /* Reload return address */
        ld      0, (STACKSIZE + LR_SAVE)(1)
        mtlr    0
    /* Return */
        addi    1, 1, STACKSIZE
        blr

    /* The trap handler: */
.L104:
    /* Restore TOC pointer */
        ld      2, (STACKSIZE + TOC_SAVE_PARENT)(1)
    /* Update caml_exception_pointer */
        std     TRAP_PTR, Caml_state(exception_pointer)
    /* Encode exception bucket as an exception result and return it */
        ori     3, 3, 2
        b       .L106
#undef STACKSIZE
        .cfi_endproc
ENDFUNCTION(caml_start_program)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
    /* Initial shuffling of arguments */
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        ld      3, 0(5)             /* r3 = Argument */
                                    /* r4 = Closure */
        ld      START_PRG_ARG, 0(4) /* Code pointer */
        b       .L102
ENDFUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg,
       8(r5) = second arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      0, 4
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        mr      5, 0                /* r5 = Closure */
        Addrglobal(START_PRG_ARG, caml_apply2)
        b       .L102
ENDFUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg, 8(r5) = second arg,
       2*8(r5) = third arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      6, 4                /* r6 = Closure */
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        ld      5, 2*8(5)        /* r5 = Third argument */
        Addrglobal(START_PRG_ARG, caml_apply3)
        b       .L102
ENDFUNCTION(caml_callback3_asm)

        .section ".text"
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .globl  caml_system.frametable
        .type   caml_system.frametable, @object
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .L105 + 4       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */

/* TOC entries */

        .section ".toc", "aw"

#define TOCENTRY(glob) LSYMB(glob): .quad glob

TOCENTRY(caml_apply2)
TOCENTRY(caml_apply3)
TOCENTRY(caml_program)

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
