open OUnit2
open Collection

let test_normal = empty |> insert ("hello", 2) |> insert ("hello", 3) |> get ("hellos")
let test_neg = empty |> insert ("hello", 2) |> insert ("hello", -6) |> get ("hellos")

let tests = "test suite for sum" >::: [
  "five"  >:: (fun _ -> assert_equal 5 (test_normal));
  "neg"  >:: (fun _ -> assert_equal -4 (test_neg));
]

let _ = run_test_tt_main tests