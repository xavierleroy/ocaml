let known_pairs : (Uchar.t * Uchar.t, Uchar.t) Hashtbl.t = Hashtbl.create 32

let _ =
  List.iter
    (fun (c1, n2, n) ->
      Hashtbl.add known_pairs
        (Uchar.of_char c1, Uchar.of_int n2) (Uchar.of_int n))
[
  ('A', 0x300, 0xc0); (* À *)
  ('A', 0x301, 0xc1); (* Á *)
  ('A', 0x302, 0xc2); (* Â *)
  ('A', 0x303, 0xc3); (* Ã *)
  ('A', 0x308, 0xc4); (* Ä *)
  ('A', 0x30a, 0xc5); (* Å *)
  ('C', 0x327, 0xc7); (* Ç *)
  ('E', 0x300, 0xc8); (* È *)
  ('E', 0x301, 0xc9); (* É *)
  ('E', 0x302, 0xca); (* Ê *)
  ('E', 0x308, 0xcb); (* Ë *)
  ('I', 0x300, 0xcc); (* Ì *)
  ('I', 0x301, 0xcd); (* Í *)
  ('I', 0x302, 0xce); (* Î *)
  ('I', 0x308, 0xcf); (* Ï *)
  ('N', 0x303, 0xd1); (* Ñ *)
  ('O', 0x300, 0xd2); (* Ò *)
  ('O', 0x301, 0xd3); (* Ó *)
  ('O', 0x302, 0xd4); (* Ô *)
  ('O', 0x303, 0xd5); (* Õ *)
  ('O', 0x308, 0xd6); (* Ö *)
  ('U', 0x300, 0xd9); (* Ù *)
  ('U', 0x301, 0xda); (* Ú *)
  ('U', 0x302, 0xdb); (* Û *)
  ('U', 0x308, 0xdc); (* Ü *)
  ('Y', 0x301, 0xdd); (* Ý *)
  ('a', 0x300, 0xe0); (* à *)
  ('a', 0x301, 0xe1); (* á *)
  ('a', 0x302, 0xe2); (* â *)
  ('a', 0x303, 0xe3); (* ã *)
  ('a', 0x308, 0xe4); (* ä *)
  ('a', 0x30a, 0xe5); (* å *)
  ('c', 0x327, 0xe7); (* ç *)
  ('e', 0x300, 0xe8); (* è *)
  ('e', 0x301, 0xe9); (* é *)
  ('e', 0x302, 0xea); (* ê *)
  ('e', 0x308, 0xeb); (* ë *)
  ('i', 0x300, 0xec); (* ì *)
  ('i', 0x301, 0xed); (* í *)
  ('i', 0x302, 0xee); (* î *)
  ('i', 0x308, 0xef); (* ï *)
  ('n', 0x303, 0xf1); (* ñ *)
  ('o', 0x300, 0xf2); (* ò *)
  ('o', 0x301, 0xf3); (* ó *)
  ('o', 0x302, 0xf4); (* ô *)
  ('o', 0x303, 0xf5); (* õ *)
  ('o', 0x308, 0xf6); (* ö *)
  ('u', 0x300, 0xf9); (* ù *)
  ('u', 0x301, 0xfa); (* ú *)
  ('u', 0x302, 0xfb); (* û *)
  ('u', 0x308, 0xfc); (* ü *)
  ('y', 0x301, 0xfd); (* ý *)
  ('y', 0x308, 0xff); (* ÿ *)
]

let normalize_generic transform s =
  let buf = Buffer.create (String.length s) in
  let rec norm prev i =
    if i >= String.length s then begin
      Buffer.add_utf_8_uchar buf (transform prev)
    end else begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let i' = i + Uchar.utf_decode_length d in
      match Hashtbl.find_opt known_pairs (prev, u) with
      | Some u' ->
          norm u' i'
      | None ->
          Buffer.add_utf_8_uchar buf (transform prev);
          norm u i'
    end in
  if s = "" then s else begin
    let d = String.get_utf_8_uchar s 0 in
    norm (Uchar.utf_decode_uchar d) (Uchar.utf_decode_length d);
    Buffer.contents buf
  end

let normalize s =
  normalize_generic (fun u -> u) s

let uchar_is_uppercase u =
  let c = Uchar.to_int u in
  c >= 65 && c <= 90 || c >= 192 && c <= 222 && c <> 215

let uchar_is_lowercase u =
  let c = Uchar.to_int u in
  c >= 97 && c <= 122 || c >= 224 && c <= 255 && c <> 247

let uchar_lowercase u =
  if uchar_is_uppercase u
  then Uchar.of_int (Uchar.to_int u + 32)
  else u

let uchar_uppercase u =
  if uchar_is_lowercase u
  then Uchar.of_int (Uchar.to_int u - 32)
  else u

let capitalize s =
  let first = ref true in
  normalize_generic
    (fun u -> if !first then (first := false; uchar_uppercase u) else u)
    s
  
let uncapitalize s =
  let first = ref true in
  normalize_generic
    (fun u -> if !first then (first := false; uchar_lowercase u) else u)
    s

let uchar_valid_in_identifier u =
  let c = Uchar.to_int u in
  if c < 97 (* a *) then
       c >= 48 (* 0 *) && c <= 57 (* 9 *)
    || c >= 65 (* A *) && c <= 90 (* Z *)
    || c = 95 (* _ *)
    || c = 39 (* ' *)
  else
       c <= 122 (* z *)
    || c >= 192 && c <= 222 && c <> 215  (* Latin-1 uppercase *)
    || c >= 224 && c <= 255 && c <> 247  (* Latin-1 uppercase *)

let uchar_not_identifier_start u =
  let c = Uchar.to_int u in
     c >= 48 (* 0 *) && c <= 57 (* 9 *)
  || c = 39 (* ' *)

type result =
  | Valid                          (** Identifier is valid *)
  | Invalid_character of Uchar.t   (** Character not allowed *)
  | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

let check_identifier s =
  let rec check i =
    if i >= String.length s then Valid else begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let i' = i + Uchar.utf_decode_length d in
      if not (uchar_valid_in_identifier u) then
        Invalid_character u
      else if i = 0 && uchar_not_identifier_start u then
        Invalid_beginning u
      else
        check i'
    end
  in check 0

let valid_identifier s =
  check_identifier s = Valid

let is_capitalized s =
  s <> "" && 
  uchar_is_uppercase (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))
