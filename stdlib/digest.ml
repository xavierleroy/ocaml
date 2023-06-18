(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Utility functions *)

let hex_of_string d =
  let char_hex n =
    Char.chr (if n < 10 then Char.code '0' + n
                        else Char.code 'a' + n - 10) in
  let len = String.length d in
  let result = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let x = Char.code d.[i] in
    Bytes.unsafe_set result (i*2) (char_hex (x lsr 4));
    Bytes.unsafe_set result (i*2+1) (char_hex (x land 0x0f));
  done;
  Bytes.unsafe_to_string result

let string_of_hex s =
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> invalid_arg "Digest.from_hex" in
  let byte i = digit s.[i] lsl 4 + digit s.[i+1] in
  String.init (String.length s / 2) (fun i -> Char.chr (byte (2 * i)))

(* Generic interface *)

module type S = sig
  type t = string
  val hash_size : int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val string : string -> t
  val bytes : bytes -> t
  val substring : string -> int -> int -> t
  val subbytes : bytes -> int -> int -> t
  val channel : in_channel -> int -> t
  val file : string -> t
  val output : out_channel -> t -> unit
  val input : in_channel -> t
  val to_hex : t -> string
  val from_hex : string -> t
  type state
  val create : unit -> state
  val get_digest : state -> t
  val add_string : state -> string -> unit
  val add_bytes : state -> bytes -> unit
  val add_substring : state -> string -> int -> int -> unit
  val add_subbytes : state -> bytes -> int -> int -> unit
  val add_channel : state -> in_channel -> int -> unit
end

(* Interface for a low-level hash function *)

module type HASH = sig
  type state
  val hash_size : int
  val create : unit -> state
  val update : state -> string -> int -> int -> unit
  val final : state -> string
  val unsafe_string : string -> int -> int -> string
end

(* Generic construction, parameterized by a hash function *)

module Make (H: HASH) : S = struct

type t = string

let compare = String.compare
let equal = String.equal

let hash_size = H.hash_size

type state = H.state

let create = H.create

let get_digest = H.final

let add_string st s = H.update st s 0 (String.length s)

let add_bytes st b =  add_string st (Bytes.unsafe_to_string b)

let add_substring st str ofs len =
  if ofs < 0 || len < 0 || ofs > String.length str - len
  then invalid_arg "Digest.add_substring";
  H.update st str ofs len

let add_subbytes st b ofs len =
  add_substring st (Bytes.unsafe_to_string b) ofs len

let add_channel st ic toread =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  if toread < 0 then begin
    let rec do_read () =
      let n = In_channel.input ic buf 0 buf_size in
      if n > 0 then (H.update st (Bytes.unsafe_to_string buf) 0 n; do_read ())
    in do_read ()
  end else begin
    let rec do_read toread =
      if toread > 0 then begin
        let n = In_channel.input ic buf 0 (Int.min buf_size toread) in
        if n = 0
        then raise End_of_file
        else (H.update st (Bytes.unsafe_to_string buf) 0 n; do_read (toread-n))
      end
    in do_read toread
  end

let string str =
  H.unsafe_string str 0 (String.length str)

let bytes b =
  string (Bytes.unsafe_to_string b)

let substring str ofs len =
  if ofs < 0 || len < 0 || ofs > String.length str - len
  then invalid_arg "Digest.substring";
  H.unsafe_string str ofs len

let subbytes b ofs len =
  substring (Bytes.unsafe_to_string b) ofs len

let channel ic toread =
  let st = H.create() in
  add_channel st ic toread;
  H.final st

let file filename =
  In_channel.with_open_bin filename (fun ic -> channel ic (-1))

let output chan digest = output_string chan digest

let input chan = really_input_string chan hash_size

let to_hex d =
  if String.length d <> hash_size then invalid_arg "Digest.to_hex";
  hex_of_string d

let from_hex s =
  if String.length s <> hash_size * 2 then invalid_arg "Digest.from_hex";
  string_of_hex s

end

(* BLAKE2b hashing *)

module BLAKE2_hash (X: sig val hash_size : int val key : string end) : HASH =
  struct
    type state
    let hash_size = X.hash_size

    external create_gen : int -> string -> state = "caml_blake2_create"
    let create () = create_gen X.hash_size X.key

    external update : state -> string -> int -> int -> unit
                          = "caml_blake2_update"

    external final_gen : state -> int -> string = "caml_blake2_final"
    let final st = final_gen st X.hash_size

    external unsafe_string_gen : int -> string -> string -> int -> int -> string
                          = "caml_blake2_string"
    let unsafe_string s ofs len =
      unsafe_string_gen X.hash_size X.key s ofs len
  end

module BLAKE128 = 
  Make(BLAKE2_hash(struct let hash_size = 16 let key = "" end))
module BLAKE256 = 
  Make(BLAKE2_hash(struct let hash_size = 32 let key = "" end))
module BLAKE512 = 
  Make(BLAKE2_hash(struct let hash_size = 64 let key = "" end))

(* MD5 hashing *)

module MD5_hash : HASH =
  struct
    type state
    let hash_size = 16
    external create : unit -> state = "caml_md5_create"
    external update : state -> string -> int -> int -> unit = "caml_md5_update"
    external final : state -> string = "caml_md5_final"
    external unsafe_string : string -> int -> int -> string = "caml_md5_string"
end

module MD5 = Make(MD5_hash)

(* Default exported implementation is MD5 *)

include MD5
