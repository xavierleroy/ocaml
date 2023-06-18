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

(** Message digest.

   This module provides functions to compute 'digests', also known as
   'hashes', of arbitrary-length strings or files.
   The supported hashing algorithms are BLAKE2 and MD5. *)

(** {1 Basic functions} *)

(** The functions in this section use the MD5 hash function to produce
   128-bit digests (16 bytes).  MD5 is not cryptographically secure.
   Hence, these functions should not be used for security-sensitive
   applications.  The BLAKE2 functions below are cryptographically secure. *)

type t = string
(** The type of digests: 16-character strings. *)

val compare : t -> t -> int
(** The comparison function for 16-character digest, with the same
    specification as {!Stdlib.compare} and the implementation
    shared with {!String.compare}. Along with the type [t], this
    function [compare] allows the module [Digest] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}.
    @since 4.00 *)

val equal : t -> t -> bool
(** The equal function for 16-character digest.
    @since 4.03 *)

val string : string -> t
(** Return the digest of the given string. *)

val bytes : bytes -> t
(** Return the digest of the given byte sequence.
    @since 4.02 *)

val substring : string -> int -> int -> t
(** [Digest.substring s ofs len] returns the digest of the substring
   of [s] starting at index [ofs] and containing [len] characters. *)

val subbytes : bytes -> int -> int -> t
(** [Digest.subbytes s ofs len] returns the digest of the subsequence
    of [s] starting at index [ofs] and containing [len] bytes.
    @since 4.02 *)

val channel : in_channel -> int -> t
(** If [len] is nonnegative, [Digest.channel ic len] reads [len]
   characters from channel [ic] and returns their digest, or raises
   [End_of_file] if end-of-file is reached before [len] characters
   are read.  If [len] is negative, [Digest.channel ic len] reads
   all characters from [ic] until end-of-file is reached and return
   their digest. *)

val file : string -> t
(** Return the digest of the file whose name is given. *)

val output : out_channel -> t -> unit
(** Write a digest on the given output channel. *)

val input : in_channel -> t
(** Read a digest from the given input channel. *)

val to_hex : t -> string
(** Return the printable hexadecimal representation of the given digest.
    @raise Invalid_argument if the argument is not exactly 16 bytes.
 *)

val of_hex : string -> t
(** Convert a hexadecimal representation back into the corresponding digest.
    @raise Invalid_argument if the argument is not exactly 32 hexadecimal
           characters.
    @since 5.2 *)

val from_hex : string -> t
(** Same function as {!Digest.of_hex}.
    @since 4.00 *)

(** {1 Generic interface} *)

module type S = sig

  type t = string
    (** The type of digests. *)

  val hash_size : int
    (** The length of digests, in bytes. *)

  val compare : t -> t -> int
    (** Compare two digests, , with the same specification as
        {!Stdlib.compare}. *)

  val equal : t -> t -> bool
    (** Test two digests for equality. *)

  val string : string -> t
    (** Return the digest of the given string. *)

  val bytes : bytes -> t
    (** Return the digest of the given byte sequence. *)

  val substring : string -> int -> int -> t
    (** [substring s ofs len] returns the digest of the substring
        of [s] starting at index [ofs] and containing [len] characters. *)

  val subbytes : bytes -> int -> int -> t
    (** [subbytes b ofs len] returns the digest of the subsequence
        of [b] starting at index [ofs] and containing [len] bytes. *)

  val channel : in_channel -> int -> t
    (** Read characters from the channel and return their digest.
        See {!Digest.channel} for the full specification. *)

  val file : string -> t
    (** Return the digest of the file whose name is given. *)

  val output : out_channel -> t -> unit
    (** Write a digest on the given output channel. *)

  val input : in_channel -> t
    (** Read a digest from the given input channel. *)

  val to_hex : t -> string
    (** Return the printable hexadecimal representation of the given digest.
        @raise Invalid_argument if the length of the argument
        is not [hash_size], *)

  val of_hex : string -> t
    (** Convert a hexadecimal representation back into the corresponding digest.
        @raise Invalid_argument if the length of the argument
        is not [2 * hash_size], or if the arguments contains non-hexadecimal
        characters. *)

(** {2 Rolling digests} *)

  type state
    (** The state of a rolling digest.  It records all the data that
        has been added to it since it was created.  *)

  val create : unit -> state
    (** Return a fresh state. *)

  val get_digest : state -> t
    (** [get_digest st] returns the digest of all the data that has
        been added to [st].
        The rolling digest [st] should not be used afterwards. *)

  val add_string : state -> string -> unit
    (** [add_string st s] adds string [s] to the rolling digest [st]. *)

  val add_bytes : state -> bytes -> unit
    (** [add_bytes st b] adds byte sequence [b] to the rolling digest [st]. *)

  val add_substring : state -> string -> int -> int -> unit
    (** [add_substring st s ofs len] adds the substring
        of [s] starting at index [ofs] and containing [len] characters
        to the rolling digest [st]. *)

  val add_subbytes : state -> bytes -> int -> int -> unit
    (** [add_subbytes st b ofs len] adds the subsequence
        of [b] starting at index [ofs] and containing [len] bytes
        to the rolling digest [st]. *)

  val add_channel : state -> in_channel -> int -> unit
    (** If [len] is nonnegative, [add_channel st ic len] reads [len] bytes
        from channel [ic] and adds them to the rolling digest [st].
        It raises [End_of_file] if end-of-file is reached before
        [len] characters are read.
        If [len] is negative, [add_channel st ic len] reads all bytes
        from [ic] until end-of-file is reached and adds them to the
        rolling digest [st]. *)

end
   (** The signature for a hash function that produces digests of length
       [hash_size] from character strings, byte arrays, and files.
       @since 5.2 *)

(** {1 Specific hash functions} *)

module BLAKE128 : S
  (** [BLAKE128] is the BLAKE2b hash function producing
      128-bit (16-byte) digests.  It is cryptographically secure.
      However, the small size of the digests enables brute-force collision
      attacks in [2{^64}] attempts.
      @since 5.2 *)

module BLAKE256 : S
  (** [BLAKE256] is the BLAKE2b hash function producing
      256-bit (32-byte) digests.  It is cryptographically secure,
      and the digests are large enough to thwart brute-force attacks.
      @since 5.2 *)

module BLAKE512 : S
  (** [BLAKE512] is the BLAKE2b hash function producing
      512-bit (64-byte) digests.  It is cryptographically secure,
      and the digests are large enough to thwart brute-force attacks.
      @since 5.2 *)

module MD5 : S
  (** [MD5] is the MD5 hash function.  It produces 128-bit (16-byte) digests
      and is not cryptographically secure at all.
      @since 5.2 *)
