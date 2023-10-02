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

module Marshal = struct

external to_channel_ : out_channel -> 'a -> Marshal.extern_flags list -> unit
       = "caml_compressed_marshal_to_channel"

let to_channel =
  if supported then to_channel_ else Marshal.to_channel

external from_channel : in_channel -> 'a
       = "caml_compressed_marshal_from_channel"

end
