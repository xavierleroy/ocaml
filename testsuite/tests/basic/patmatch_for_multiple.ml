(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/86 = 3 *match*/87 = 2 *match*/88 = 1)
  (catch
    (catch
      (catch (if (!= *match*/87 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/86 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/86 = 3 *match*/87 = 2 *match*/88 = 1)
  (catch (if (!= *match*/87 3) (if (!= *match*/86 1) 0 (exit 1)) (exit 1))
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/91 = 3 *match*/92 = 2 *match*/93 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/92 3) (exit 6)
          (let (x/95 =a (makeblock 0 *match*/91 *match*/92 *match*/93))
            (exit 4 x/95)))
       with (6)
        (if (!= *match*/91 1) (exit 5)
          (let (x/94 =a (makeblock 0 *match*/91 *match*/92 *match*/93))
            (exit 4 x/94))))
     with (5) 0)
   with (4 x/89) (seq (ignore x/89) 1)))
(let (*match*/91 = 3 *match*/92 = 2 *match*/93 = 1)
  (catch
    (if (!= *match*/92 3)
      (if (!= *match*/91 1) 0
        (exit 4 (makeblock 0 *match*/91 *match*/92 *match*/93)))
      (exit 4 (makeblock 0 *match*/91 *match*/92 *match*/93)))
   with (4 x/89) (seq (ignore x/89) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/96 b/97 0)
(function a/96 b/97 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/100 b/101 (let (p/102 =a (makeblock 0 a/100 b/101)) p/102))
(function a/100 b/101 (makeblock 0 a/100 b/101))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/104 b/105 (let (p/106 =a (makeblock 0 a/104 b/105)) p/106))
(function a/104 b/105 (makeblock 0 a/104 b/105))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/110 b/111
  (let (x/112 =a a/110 p/113 =a (makeblock 0 a/110 b/111))
    (makeblock 0 x/112 p/113)))
(function a/110 b/111 (makeblock 0 a/110 (makeblock 0 a/110 b/111)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/116 b/117
  (let (x/118 =a a/116 p/119 =a (makeblock 0 a/116 b/117))
    (makeblock 0 x/118 p/119)))
(function a/116 b/117 (makeblock 0 a/116 (makeblock 0 a/116 b/117)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/126 b/127
  (if a/126
    (let (x/128 =a a/126 p/129 =a (makeblock 0 a/126 b/127))
      (makeblock 0 x/128 p/129))
    (let (x/130 =a b/127 p/131 =a (makeblock 0 a/126 b/127))
      (makeblock 0 x/130 p/131))))
(function a/126 b/127
  (if a/126 (makeblock 0 a/126 (makeblock 0 a/126 b/127))
    (makeblock 0 b/127 (makeblock 0 a/126 b/127))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/132 b/133
  (catch
    (if a/132
      (let (x/140 =a a/132 p/141 =a (makeblock 0 a/132 b/133))
        (exit 10 x/140 p/141))
      (let (x/138 =a b/133 p/139 =a (makeblock 0 a/132 b/133))
        (exit 10 x/138 p/139)))
   with (10 x/134 p/135) (makeblock 0 x/134 p/135)))
(function a/132 b/133
  (catch
    (if a/132 (exit 10 a/132 (makeblock 0 a/132 b/133))
      (exit 10 b/133 (makeblock 0 a/132 b/133)))
   with (10 x/134 p/135) (makeblock 0 x/134 p/135)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/142 b/143
  (if a/142
    (let (x/144 =a a/142 _p/145 =a (makeblock 0 a/142 b/143))
      (makeblock 0 x/144 [0: 1 1]))
    (let (x/146 =a a/142 p/147 =a (makeblock 0 a/142 b/143))
      (makeblock 0 x/146 p/147))))
(function a/142 b/143
  (if a/142 (makeblock 0 a/142 [0: 1 1])
    (makeblock 0 a/142 (makeblock 0 a/142 b/143))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/148 b/149
  (let (x/150 =a a/148 p/151 =a (makeblock 0 a/148 b/149))
    (makeblock 0 x/150 p/151)))
(function a/148 b/149 (makeblock 0 a/148 (makeblock 0 a/148 b/149)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/161 b/162
  (catch
    (if a/161 (if b/162 (let (p/163 =a (field 0 b/162)) p/163) (exit 12))
      (exit 12))
   with (12) (let (p/164 =a (makeblock 0 a/161 b/162)) p/164)))
(function a/161 b/162
  (catch (if a/161 (if b/162 (field 0 b/162) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/161 b/162)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/165 b/166
  (catch
    (catch
      (if a/165
        (if b/166 (let (p/170 =a (field 0 b/166)) (exit 13 p/170)) (exit 14))
        (exit 14))
     with (14) (let (p/169 =a (makeblock 0 a/165 b/166)) (exit 13 p/169)))
   with (13 p/167) p/167))
(function a/165 b/166
  (catch
    (catch
      (if a/165 (if b/166 (exit 13 (field 0 b/166)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/165 b/166)))
   with (13 p/167) p/167))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
