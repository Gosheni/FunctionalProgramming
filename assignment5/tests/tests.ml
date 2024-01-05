(* 
open Core;;
open OUnit2;;
*)

open OUnit2
open Monads

let test_stack_monad _ =
  assert_equal true (are_balanced_monadic "((()))");
  assert_equal true (are_balanced_monadic "(())");
  assert_equal true (are_balanced_monadic "");
  assert_equal false (are_balanced_monadic ")");
  assert_equal true (are_balanced_monadic "(a + b) * (c -d)")

let test_exception_stack _ =
  assert_equal true (are_balanced_more_monadic "((()))");
  assert_equal false (are_balanced_more_monadic "(()");
  assert_equal true (are_balanced_more_monadic "");
  assert_equal false (are_balanced_more_monadic "(");
  assert_equal true (are_balanced_more_monadic "((a + b) * (c - d))");
  assert_equal true (are_balanced_more_monadic "(a + b) * c - d")

let monad_tests = "monad_tests" >: test_list [
  "test_stack_monad" >:: test_stack_monad;
  "test_exception_stack" >:: test_exception_stack;
]

let series = "Assignment5 Tests" >::: [
  monad_tests;
]

let () =
  run_test_tt_main series
