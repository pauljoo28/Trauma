open OUnit2
open Collection
open Trace

let test_collection_01 =
  Collection.empty |> Collection.insert ("This", 2) |> Collection.get ("This")

let test_collection_02 =
  Collection.empty |> Collection.insert ("This", 2) |> Collection.insert ("This", -2) |> Collection.get ("This")

let test_collection_03 =
  Collection.empty |> Collection.insert ("This", 2) |> Collection.get ("Thingy")

let col1 = Collection.empty |> Collection.insert ("This", 2)
let col2 = Collection.empty |> Collection.insert ("This", 3)
let col3 = Collection.empty |> Collection.insert ("This", 5)
let col4 = Collection.empty |> Collection.insert ("This", 7)

let test_trace_01 =
  Trace.init col1 |> Trace.add_dim |> Trace.get_diff_version [0] |> to_collection |> Collection.get "This"

let test_trace_01 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.get_diff_version [0] 
  |> to_collection
  |> Collection.get "This"

let test_trace_02 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.get_diff_version [1] 
  |> to_collection 
  |> Collection.get "This"

let test_trace_03 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.get_version [1] 
  |> to_collection
  |> Collection.get "This"

let test_trace_04 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.get_diff_version [0; 0]
  |> to_collection
  |> Collection.get "This"

let test_trace_05 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.get_diff_version [0; 1]
  |> to_collection
  |> Collection.get "This"

let test_trace_06 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.get_diff_version [1; 0]
  |> to_collection
  |> Collection.get "This"

let test_trace_07 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.get_diff_version [1; 1]
  |> to_collection
  |> Collection.get "This"

let test_trace_08 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.get_version [1; 1]
  |> to_collection
  |> Collection.get "This"

let test_trace_09 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.debug_empty_output
  |> Trace.get_version [1; 1]
  |> to_collection
  |> Collection.get "This"

let test_trace_10 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.swap [0;0] col4
  |> Trace.get_version [0; 0]
  |> to_collection
  |> Collection.get "This"

let test_trace_11 =
  Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.distinct
  |> Trace.get_version [2; 0]
  |> to_collection
  |> Collection.get "This"

let _ =
  let x = Trace.init col1 
  |> Trace.add_dim 
  |> Trace.add_diff [] col2
  |> Trace.add_diff [] col2
  |> Trace.add_dim
  |> Trace.add_diff [0] col3
  |> Trace.add_diff [1] col4
  |> Trace.add_diff [2] col4
  |> Trace.add_diff [2] col4
  |> Trace.distinct
  in Printf.printf "DEBUG_LIST: %s" (debug_iter_tostring x)

let benchmark_tests = "test suite for benchmark" >::: [
  "01"  >:: (fun _ -> assert_equal 2 (2));
]

let collection_tests = "test suite for collection" >::: [
  "01"  >:: (fun _ -> assert_equal 2 (test_collection_01));
  "02"  >:: (fun _ -> assert_equal 0 (test_collection_02));
  "03"  >:: (fun _ -> assert_equal 0 (test_collection_03));
]

let trace_tests = "test suite for trace" >::: [
  "01"  >:: (fun _ -> assert_equal 2 (test_trace_01) ~printer:string_of_int);
  "02"  >:: (fun _ -> assert_equal 3 (test_trace_02) ~printer:string_of_int);
  "03"  >:: (fun _ -> assert_equal 5 (test_trace_03) ~printer:string_of_int);
  "04"  >:: (fun _ -> assert_equal 2 (test_trace_04) ~printer:string_of_int);
  "05"  >:: (fun _ -> assert_equal 5 (test_trace_05) ~printer:string_of_int);
  "06"  >:: (fun _ -> assert_equal 3 (test_trace_06) ~printer:string_of_int); 
  "07"  >:: (fun _ -> assert_equal 7 (test_trace_07) ~printer:string_of_int);
  "08"  >:: (fun _ -> assert_equal 17 (test_trace_08) ~printer:string_of_int);
  "09"  >:: (fun _ -> assert_equal 0 (test_trace_09) ~printer:string_of_int);
  "10"  >:: (fun _ -> assert_equal 7 (test_trace_10) ~printer:string_of_int);
  "11"  >:: (fun _ -> assert_equal 1 (test_trace_11) ~printer:string_of_int);
]

let _ = run_test_tt_main benchmark_tests

let _ = run_test_tt_main collection_tests

let _ = run_test_tt_main trace_tests
