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

val parse: string -> t
  (** Check that the given string is correctly UTF-8 encoded and is
      a valid OCaml identifier.  Returns a normalized form of the
      string (currently: in NFD form).
      Raises [Error] in case of invalid UTF-8 data or characters not
      supported. *)

val is_uppercase: t -> bool
  (** Return true if the given identifier starts with an uppercase letter,
      false otherwise. *)

val capitalize: t -> t
  (** Return the given identifier with the first letter capitalized. *)

type error =
  | Bad_UTF8_encoding
  | Character_not_allowed of Uchar.t
  | Wrong_start of char

exception Error of error

val describe_error: error -> string
