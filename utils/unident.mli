(** Supported identifier characters are, currently:
      - ASCII letters A, ..., Z, a, ..., z
      - Latin-1 accented letters
      - digits 0...9, underscore, single quote
*)

type t
  (** The type of normalized, UTF8-encoded strings.
      If all characters are identifier characters (i.e. if [valid_identifier]
      returns [true]), the whole string is in NFC normalized form
      and can be compared with other NFC strings by normal equality.
      A string of type [t] can also contain other characters.  These
      are not normalized. *)

val to_string: t -> string
  (** Return the normalized string as a string in UTF-8 encoding. *)

val normalize: string -> t
  (** Normalize the given UTF-8 encoded string.
      Identifier characters are put in NFC normalized form.
      Invalid UTF-8 sequences are replaced by U+FFFD. *)

val capitalize: string -> t
  (** Like [normalize], but if the string starts with a lowercase identifier
      character, it is replaced by the corresponding uppercase character.
      Subsequent characters are not changed. *)

val uncapitalize: string -> t
  (** Like [normalize], but if the string starts with an uppercase identifier
      character, it is replaced by the corresponding lowercase character.
      Subsequent characters are not changed. *)

val valid_identifier: t -> bool
  (** Check whether the given normalized string is a valid OCaml identifier:
      - all characters are identifier characters
      - it does not start with a digit or a single quote
  *)

type result =
  | Valid                          (** Identifier is valid *)
  | Invalid_character of Uchar.t   (** Character not allowed *)
  | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

val check_identifier: t -> result
  (** Like [valid_identifier], but returns a more detailed error code. *)

val is_capitalized: t -> bool
  (** Returns [true] if the given normalized string starts with an uppercase
      identifier character, [false] otherwise. *)

val unsafe_is_capitalized: string -> bool
  (** Like [is_capitalized], but accepts any string and may return wrong
      results if the string is not normalized.  To be called with
      pure ASCII strings or with the result of [Unident.to_string]. *)
