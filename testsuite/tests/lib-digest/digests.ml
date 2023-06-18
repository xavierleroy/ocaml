(* TEST
*)

module Test(H: Digest.S) = struct

  let test1 (msg, hh) =
    assert (H.(equal (string msg) (from_hex hh)))

  let test2 (msg, hh) =
    let st = H.create() in
    for i = 0 to String.length msg - 1 do
      H.add_substring st msg i 1
    done;
    assert (H.(equal (get_digest st) (from_hex hh)))

  let testfile wlen rlen =
    let data = String.init wlen Char.unsafe_chr in
    Out_channel.with_open_bin "data.tmp"
      (fun oc -> Out_channel.output_string oc data);
    let h1 = H.file "data.tmp" in
    assert (H.equal h1 (H.string data));
    let h2 =
      In_channel.with_open_bin "data.tmp"
        (fun ic -> H.channel ic rlen) in
    assert (H.equal h2 (H.substring data 0 rlen));
    Sys.remove "data.tmp"

  let run_tests vectors =
    List.iter test1 vectors;
    List.iter test2 vectors;
    testfile 100 99;
    testfile 100_000 10_000
end

module TestMD5 = Test(Digest.MD5)
let _ = TestMD5.run_tests
  ["", "D41D8CD98F00B204E9800998ECF8427E";
   "a", "0CC175B9C0F1B6A831C399E269772661";
   "abc", "900150983CD24FB0D6963F7D28E17F72";
   "message digest", "F96B697D7CB7938D525A2F31AAF161D0"]

module TestBLAKE512 = Test(Digest.BLAKE512)
let _ = TestBLAKE512.run_tests
  [("",
    "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419\
     d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce");
   ("a",
    "333fcb4ee1aa7c115355ec66ceac917c8bfd815bf7587d325aec1864edd24e34\
     d5abe2c6b1b5ee3face62fed78dbef802f2a85cb91d455a8f5249d330853cb3c");
   ("abc",
    "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1\
     7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923");
   ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno\
     ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
    "ce741ac5930fe346811175c5227bb7bfcd47f42612fae46c0809514f9e0e3a11\
     ee1773287147cdeaeedff50709aa716341fe65240f4ad6777d6bfaf9726e5e52")]

module TestBLAKE256 = Test(Digest.BLAKE256)
let _ = TestBLAKE256.run_tests
  [("", "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8");
   ("a", "8928aae63c84d87ea098564d1e03ad813f107add474e56aedd286349c0c03ea4");
   ("abc", "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319");
   ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno\
     ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
    "90a0bcf5e5a67ac1578c2754617994cfc248109275a809a0721feebd1e918738")]

module TestBLAKE128 = Test(Digest.BLAKE128)
let _ = TestBLAKE128.run_tests
  [("", "cae66941d9efbd404e4d88758ea67670");
   ("a", "27c35e6e9373877f29e562464e46497e");
   ("abc", "cf4ab791c62b8d2b2109c90275287816");
   ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno\
     ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
    "8fa81cd08c10a6e4dd94583e6fb48c2f")]
