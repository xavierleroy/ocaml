(* TEST *)

let () =
  let test f e =
    assert(Filename.extension f = e);
    assert(Filename.extension ("foo/" ^ f) = e);
    assert(f = Filename.remove_extension f ^ Filename.extension f)
  in
  test "" "";
  test "foo" "";
  test "foo.txt" ".txt";
  test "foo.txt.gz" ".gz";
  test ".foo" "";
  test "." "";
  test ".." "";
  test "foo..txt" ".txt"

let () =
  if Sys.os_type = "Win32" then begin
    let test f e p =
      assert (Filename.extension f = e);
      assert(Filename.extension ("foo/" ^ f) = e);
      assert(Filename.remove_extension f = p);
      assert(Filename.remove_extension ("foo/" ^ f) = "foo/" ^ p) in
    test "foo." "" "foo.";
    test "foo " "" "foo ";
    test "foo.txt." ".txt" "foo";
    test "foo.txt  " ".txt" "foo";
    test "foo.txt .gz " ".gz" "foo.txt "
  end

let () =
  let test ~suffix f r =
    assert (Filename.check_suffix f suffix = (r <> None));
    assert (Filename.chop_suffix_opt ~suffix f = r) in
  test ~suffix:".txt" "foo.txt" (Some "foo");
  test ~suffix:".txt" "f" None;
  test ~suffix:".txt" "foo.exe" None;
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then begin
    test ~suffix:".tXt" "foo.TxT" (Some "foo")
  end;
  if Sys.os_type = "Win32" then begin
    test ~suffix:".TXT" "foo.txt  " (Some "foo")
  end
