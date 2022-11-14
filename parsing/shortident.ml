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

open Printf

type t = string

type error =
  | Bad_UTF8_encoding
  | Character_not_allowed of Uchar.t
  | Wrong_start of char

exception Error of error

(* This is the list of supported accented letters. *)

let valid_letters =
[
  0xc0; (* À *)  0xc1; (* Á *)  0xc2; (* Â *)  0xc3; (* Ã *)
  0xc4; (* Ä *)  0xc5; (* Å *)  0xc6; (* Æ *)  0xc7; (* Ç *)
  0xc8; (* È *)  0xc9; (* É *)  0xca; (* Ê *)  0xcb; (* Ë *)
  0xcc; (* Ì *)  0xcd; (* Í *)  0xce; (* Î *)  0xcf; (* Ï *)
  0xd0; (* Ð *)  0xd1; (* Ñ *)  0xd2; (* Ò *)  0xd3; (* Ó *)
  0xd4; (* Ô *)  0xd5; (* Õ *)  0xd6; (* Ö *)  0xd8; (* Ø *)
  0xd9; (* Ù *)  0xda; (* Ú *)  0xdb; (* Û *)  0xdc; (* Ü *)
  0xdd; (* Ý *)  0xde; (* Þ *)  0xdf; (* ß *)  0xe0; (* à *)
  0xe1; (* á *)  0xe2; (* â *)  0xe3; (* ã *)  0xe4; (* ä *)
  0xe5; (* å *)  0xe6; (* æ *)  0xe7; (* ç *)  0xe8; (* è *)
  0xe9; (* é *)  0xea; (* ê *)  0xeb; (* ë *)  0xec; (* ì *)
  0xed; (* í *)  0xee; (* î *)  0xef; (* ï *)  0xf0; (* ð *)
  0xf1; (* ñ *)  0xf2; (* ò *)  0xf3; (* ó *)  0xf4; (* ô *)
  0xf5; (* õ *)  0xf6; (* ö *)  0xf8; (* ø *)  0xf9; (* ù *)
  0xfa; (* ú *)  0xfb; (* û *)  0xfc; (* ü *)  0xfd; (* ý *)
  0xfe; (* þ *)  0xff; (* ÿ *)
]

(* This is the list of supported (plain letter, accent) pairs,
   along with the corresponding accented letter. *)

let valid_combinations =
[
  ('A', 0x300, 0xc0); (* À *)  ('A', 0x301, 0xc1); (* Á *)
  ('A', 0x302, 0xc2); (* Â *)  ('A', 0x303, 0xc3); (* Ã *)
  ('A', 0x308, 0xc4); (* Ä *)  ('A', 0x30a, 0xc5); (* Å *)
  ('C', 0x327, 0xc7); (* Ç *)  ('E', 0x300, 0xc8); (* È *)
  ('E', 0x301, 0xc9); (* É *)  ('E', 0x302, 0xca); (* Ê *)
  ('E', 0x308, 0xcb); (* Ë *)  ('I', 0x300, 0xcc); (* Ì *)
  ('I', 0x301, 0xcd); (* Í *)  ('I', 0x302, 0xce); (* Î *)
  ('I', 0x308, 0xcf); (* Ï *)  ('N', 0x303, 0xd1); (* Ñ *)
  ('O', 0x300, 0xd2); (* Ò *)  ('O', 0x301, 0xd3); (* Ó *)
  ('O', 0x302, 0xd4); (* Ô *)  ('O', 0x303, 0xd5); (* Õ *)
  ('O', 0x308, 0xd6); (* Ö *)  ('U', 0x300, 0xd9); (* Ù *)
  ('U', 0x301, 0xda); (* Ú *)  ('U', 0x302, 0xdb); (* Û *)
  ('U', 0x308, 0xdc); (* Ü *)  ('Y', 0x301, 0xdd); (* Ý *)
  ('a', 0x300, 0xe0); (* à *)  ('a', 0x301, 0xe1); (* á *)
  ('a', 0x302, 0xe2); (* â *)  ('a', 0x303, 0xe3); (* ã *)
  ('a', 0x308, 0xe4); (* ä *)  ('a', 0x30a, 0xe5); (* å *)
  ('c', 0x327, 0xe7); (* ç *)  ('e', 0x300, 0xe8); (* è *)
  ('e', 0x301, 0xe9); (* é *)  ('e', 0x302, 0xea); (* ê *)
  ('e', 0x308, 0xeb); (* ë *)  ('i', 0x300, 0xec); (* ì *)
  ('i', 0x301, 0xed); (* í *)  ('i', 0x302, 0xee); (* î *)
  ('i', 0x308, 0xef); (* ï *)  ('n', 0x303, 0xf1); (* ñ *)
  ('o', 0x300, 0xf2); (* ò *)  ('o', 0x301, 0xf3); (* ó *)
  ('o', 0x302, 0xf4); (* ô *)  ('o', 0x303, 0xf5); (* õ *)
  ('o', 0x308, 0xf6); (* ö *)  ('u', 0x300, 0xf9); (* ù *)
  ('u', 0x301, 0xfa); (* ú *)  ('u', 0x302, 0xfb); (* û *)
  ('u', 0x308, 0xfc); (* ü *)  ('y', 0x301, 0xfd); (* ý *)
  ('y', 0x308, 0xff); (* ÿ *)
]

(* Table used for validating (plain letter, accent) pairs *)

let tbl_pairs : (char * Uchar.t, unit) Hashtbl.t = Hashtbl.create 32
let _ =
  List.iter
    (fun (c, u, _) -> Hashtbl.add tbl_pairs (c, Uchar.of_int u) ())
    valid_combinations

(* Table used for validating accented letters *)

let tbl_letters : (Uchar.t, unit) Hashtbl.t = Hashtbl.create 32
let _ =
  List.iter
     (fun u -> Hashtbl.add tbl_letters (Uchar.of_int u) ())
     valid_letters

(* Table used for normalization *)

let tbl_normalize : (Uchar.t, char * Uchar.t) Hashtbl.t = Hashtbl.create 32
let _ =
  List.iter
     (fun (c, u1, u2) ->
        Hashtbl.add tbl_normalize (Uchar.of_int u2) (c, Uchar.of_int u1))
     valid_combinations

(* Check that a string is a valid UTF8 encoding of a valid identifier *)

let check s =
  let rec check prev i =
    if i < String.length s then begin
      let c = s.[i] in
      if c < '\x80' then begin
        begin match c with
        | 'A'..'Z' | 'a'..'z' | '_' -> ()
        | '0'..'9' | '\'' -> if i = 0 then raise (Error (Wrong_start c))
        | _ -> raise (Error (Character_not_allowed (Uchar.of_char c)))
        end;
        check c (i + 1)
      end else begin
        let d = String.get_utf_8_uchar s i in
        if not (Uchar.utf_decode_is_valid d) then
          raise (Error Bad_UTF8_encoding);
        let l = Uchar.utf_decode_length d in
        let u = Uchar.utf_decode_uchar d in
        if Hashtbl.mem tbl_pairs (prev, u)
        || Hashtbl.mem tbl_letters u
        then check '\x00' (i + l)
        else raise (Error (Character_not_allowed u))
      end
    end
  in
    check '\x00' 0

let is_valid s =
  try check s; true with Error _ -> false

(* Normalization, NFD style *)

let normalize s =
  let buf = Buffer.create (String.length s + 16) in
  let rec norm i =
    if i < String.length s then begin
      let c = s.[i] in
      if c < '\x80' then begin
        Buffer.add_char buf c;
        norm (i + 1)
      end else begin
        let d = String.get_utf_8_uchar s i in
        assert (Uchar.utf_decode_is_valid d);
        let l = Uchar.utf_decode_length d in
        let u = Uchar.utf_decode_uchar d in
        begin match Hashtbl.find_opt tbl_normalize u with
        | Some(c, u') ->
            Buffer.add_char buf c;
            Buffer.add_utf_8_uchar buf u'
        | None ->
            Buffer.add_utf_8_uchar buf u
        end;
        norm (i + l)
      end
    end
  in
    norm 0; Buffer.contents buf        

(* For capitalization, we rely on the NFD form always starting with a plain
   letter. *)

let is_uppercase s =
  String.length s > 0 && match s.[0] with 'A'..'Z' -> true | _ -> false

let capitalize = String.capitalize_ascii
let uncapitalize = String.uncapitalize_ascii

(* Explaining errors *)

let describe_error = function
  | Bad_UTF8_encoding ->
      "Bad UTF-8 encoding"
  | Character_not_allowed c ->
      sprintf "Character U+%X not allowed in identifiers" (Uchar.to_int c)
  | Wrong_start c ->
      sprintf "An identifier cannot start with %c" c
