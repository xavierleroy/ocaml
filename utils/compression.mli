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

val supported : bool
(** [true] if  compression is supported, [false] otherwise. *)

module Marshal : sig

val to_channel : out_channel -> 'a -> Marshal.extern_flags list -> unit
(** [Compression.Marshal.to_channel chan v flags] writes the representation
    of [v] on channel [chan].
    The [flags] argument is as described in {!Marshal.to_channel}.
    If compression is supported, the marshaled data
    representing value [v] is compressed before being written to
    channel [chan].
    If compression is not supported, this function behaves like
    {!Marshal.to_channel}. *)

val from_channel : in_channel -> 'a
(** [Compression.Marshal.from_channel chan] reads from channel [chan] the
    byte representation of a structured value, as produced by
    [Compression.Marshal.to_channel], and reconstructs and
    returns the corresponding value.
    If compression is not supported, this function behaves like
    {!Marshal.from_channel}. *)

end
