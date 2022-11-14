(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, Collège de France                            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Identifiers, used in lexing and parsing, with modest support for 
    Unicode characters.

    The only Unicode characters supported currently, beyond ASCII,
    are Latin-1 accented letters encoded in UTF-8. *)

type t = string

val check: string -> unit
  (** Check that the given string is correctly UTF-8 encoded and is
      a valid OCaml identifier.  Otherwise, raise [Error] with an
      explanation of the error. *)

val is_valid: string -> bool
  (** Like [check] but returns [true] if the string is valid and [false]
      otherwise.  Does not raise [Error]. *)

val normalize: string -> t
  (** [normalize s] is [s] where the supported Unicode characters
      are NFD-normalized.  Other Unicode characters are left unchanged.
      Can be applied to valid identifiers to get their canonical
      representation.  Can also be applied to e.g. file names that are
      not necessarily valid identifiers, but need to be compared modulo
      different representations of the supported Unicode characters. *)

val is_uppercase: t -> bool
  (** Return true if the given normalized identifier starts with an
      uppercase letter, false otherwise. *)

val capitalize: t -> t
  (** Return the given normalized identifier with the first letter
      changed to upper case. *)

val uncapitalize: t -> t
  (** Return the given normalized identifier with the first letter
      changed to lower case. *)

type error =
  | Bad_UTF8_encoding
  | Character_not_allowed of Uchar.t
  | Wrong_start of char

exception Error of error

val describe_error: error -> string
