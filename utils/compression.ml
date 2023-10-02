(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Xavier Leroy, Collège de France and Inria project Cambium       *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external supported_ : unit -> bool = "caml_compression_supported"

let supported = supported_ ()

external to_channel : out_channel -> 'a -> Marshal.extern_flags list -> unit
       = "caml_compressed_marshal_to_channel"

let output_value chan v =
  if supported then to_channel chan v [] else Stdlib.output_value chan v

external from_channel : in_channel -> 'a
       = "caml_compressed_marshal_from_channel"

let input_value chan =
  if supported then from_channel chan else Stdlib.input_value chan
