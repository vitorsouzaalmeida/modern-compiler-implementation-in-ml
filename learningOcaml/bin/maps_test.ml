open OUnit2
open Maps

let assoc_tests = [
  
]

let suite = "maps suite" >::: assoc_tests

let _ = run_test_tt_main suite