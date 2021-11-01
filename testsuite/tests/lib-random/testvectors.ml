(* TEST
*)

(* Check the numbers drawn from a known state against the numbers
   obtained from the reference Java implementation. *)

open Random

let _ =
  full_init [| 1; 2; 3; 4 |];
  for i = 0 to 49 do
    Printf.printf "%Ld\n" (bits64 ())
  done

let _ = exit 0
