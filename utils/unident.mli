(** Supported identifier characters are, currently:
      - ASCII letters A, ..., Z, a, ..., z
      - Latin-1 accented letters
      - digits 0...9, underscore, single quote
*)

val normalize: string -> string
  (** Normalize the given UTF-8 encoded string.
      Identifier characters are put in NFC normalized form.
      Invalid UTF-8 sequences are replaced by U+FFFD. *)

val capitalize: string -> string
  (** Like [normalize], but if the string starts with a lowercase identifier
      character, it is replaced by the corresponding uppercase character.
      Subsequent characters are not changed. *)

val uncapitalize: string -> string
  (** Like [normalize], but if the string starts with an uppercase identifier
      character, it is replaced by the corresponding lowercase character.
      Subsequent characters are not changed. *)

val valid_identifier: string -> bool
  (** Check whether the given normalized string is a valid OCaml identifier:
      - all characters are identifier characters
      - it does not start with a digit or a single quote
  *)

type result =
  | Valid                          (** Identifier is valid *)
  | Invalid_character of Uchar.t   (** Character not allowed *)
  | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

val check_identifier: string -> result
  (** Like [valid_identifier], but returns a more detailed error code. *)

val is_capitalized: string -> bool
  (** Returns [true] if the given normalized string starts with an
      uppercase identifier character, [false] otherwise.  May return
      wrong results if the string is not normalized. *)
