# Supported platforms

IBM POWER processors, 64-bit, little-endian, ELF ABI v2.
(Called `ppc64el` in Debian.)

No longer supported:
* 32 bits, ELF ABI (Debian's `powerpc`)
* 64 bits big-endian, ELF ABI v1 (Debian's `powerpc`)
* AIX
* MacOS X.

# Reference documents

* Instruction set architecture:
  _Power ISA User Instruction Set Architecture_,
  book 1 of _Power Instruction Set_
  (https://openpowerfoundation.org/specifications/isa/)

* ELF ABI 64 bits version 2:
  _OpenPOWER ABI for Linux Supplement for the Power Architecture
   64-bit ELF V2 ABI_
  (https://openpowerfoundation.org/specifications/64bitelfabi/)

* _The PowerPC Compiler Writer's Guide_, Warthman Associates, 1996.
  (PDF available from various sources on the Web.)

# Notes on calling conventions

## The minimal stack frame

The stack pointer SP (register 1) is always 16-byte aligned.

Every stack frame contains a 32-byte reserved area at SP to SP + 31.

* The 64-bit word at SP + 0 is reserved for a back pointer to the previous frame.
  ocamlopt-generated code does not maintain this back link.
* The 32-bit word at SP + 8 is reserved for saving the CR register.
  ocamlopt-generated code does not use this word.
* The 32-bit word at SP + 12 is reserved for future uses.
* The 64-bit word at SP + 16 is used by the *callee* to save its return address
  (the value of the LR register on entry to the callee).  So, a function
  saves its return address in the callee's frame, at offset 16 from the SP
  on entry to the function.
* The 64-bit word at SP + 24 is used by the *caller* or by
  linker-generated call *veeners* to save the TOC register r2 across calls.

A typical function prologue that allocates N bytes of stack and saves LR:
```
        addi 1, 1, -N
        mflr 0
        std  0, (N + 16)(1)    ; in caller's frame!
```
If the back pointer is to be maintained as well:
```
        stdu 1, -N(1)
        mflr 0
        std  0, (N + 16)(1)    ; in caller's frame!
```

If the function needs to stack-allocate M extra bytes, e.g. for an exception handler, it should leave the bottom 32 bytes reserved.  So, it decrements SP by M and uses bytes SP + 32 ... SP + 32 + M - 1 as the stack-allocated space:
```
        addi 1, 1, -32          ; allocate exception handler
        std  29, 40(1)          ; save previous trap ptr at exception handler + 8
```

Because the bottom 32 bytes are used by linker-generated call veeners, we need to reserve them both in C stack frames and in OCaml stack frames.

To speed up stack switching between OCaml and C, the stack pointers saved in struct stack_info and struct c_stack_link point to the bottom of the stack frame and include the reserved 32 bytes area.

## TOC references and register r2

Register r2 points to a table of contents (TOC) that contains absolute addresses for symbols.  Different compilation units, or maybe even different functions within a compilation unit (?), can have different TOCs.  Hence, register r2 must be properly initialized at the beginning of functions that access the TOC, and properly saved and restored around function calls.

The general protocol is as follows:

* When a function is called, its address (the address of the first instruction of the function) must be in register r12.

* Every function that makes TOC references starts with two instructions that initialize r2 to a fixed offset from r12, offset determined by the linker:
```
0:  addis 2, 12, (.TOC. - 0b)@ha
    addi  2, 2,  (.TOC. - 0b)@l
```

* Register r2 is caller-save.  Hence, if the caller makes TOC references, it must save r2 before the call and restore it after.  The reserved area of its caller's frame is used to save r2.

For example, here is a call to a function pointer found at offset 0 from register r3:
```
     std   2, (FRAMESIZE + 24)(1)   ; save r2
     ld    12, 0(3)                 ; address of function in r12
     mtctr 12
     bctrl                          ; call the function
     ld    2, (FRAMESIZE + 24)(1)   ; restore r2
```

If the caller does not make TOC references, the `std 2` and `ld 2` can be omitted.  Likewise for a tail call:
```
     ld    12, 0(3)                 ; address of function in r12
     mtctr 12
     bctr                           ; tail call the function
```




