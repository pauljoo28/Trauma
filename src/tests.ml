open OUnit2
open Collection

let tests = "test suite for sum" >::: [
  "empty"  >:: (fun _ -> assert_equal 0 (2+2));
]

let _ = run_test_tt_main tests