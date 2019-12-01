#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Jeremie Dimino, Jane Street Europe                     *
#*                                                                        *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

TOPDIR = ../..

COMPILERLIBSDIR = $(TOPDIR)/compilerlibs

RUNTIME_VARIANT ?=
ASPPFLAGS ?=

include $(TOPDIR)/Makefile.tools

expect_MAIN=expect_test
expect_PROG=$(expect_MAIN)$(EXE)
expect_DIRS = parsing utils driver typing toplevel
expect_OCAMLFLAGS = $(addprefix -I $(TOPDIR)/,$(expect_DIRS))
expect_LIBS := $(addprefix $(COMPILERLIBSDIR)/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

tools := $(expect_PROG)

all: $(tools)

$(expect_PROG): $(expect_LIBS:=.cma) $(expect_MAIN).cmo
	$(OCAMLC) -linkall -o $@ $^

$(expect_PROG): COMPFLAGS = $(expect_OCAMLFLAGS)

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

.PHONY: clean
clean:
	rm -f *.cm* *.$(O)
	rm -f $(tools)

