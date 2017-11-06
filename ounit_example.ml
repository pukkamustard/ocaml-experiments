open OUnit2
open BatPervasives

let test1 test_ctxt = assert_equal "x" (identity "x")

let test2 test_ctxt = assert_equal 100 (identity 100)

let test3 test_ctxt = assert_equal 100 (identity 101)

let suite =
  "suite">:::
   ["test1">:: test1;
    "test2">:: test2;
    (* Ucomment next line to cause a test failure *)
    (*"test3">:: test3*)
   ]

let () =
  run_test_tt_main suite



