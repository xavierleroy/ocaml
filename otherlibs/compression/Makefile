#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                        David Allsopp, Tarides                          *
#*                                                                        *
#*   Copyright 2023 David Allsopp Ltd.                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the ZSTD compression library

LIBNAME = compression
CLIBNAME = camlzstd
CAMLOBJS = compression.cmo
C_SOURCES = zstd.c
LINKOPTS = $(addprefix -cclib , $(ZSTD_LIBS))
LDOPTS = $(addprefix -ldopt , $(ZSTD_LIBS))

include ../Makefile.otherlibs.common

.PHONY: depend
depend:
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml > .depend

include .depend
