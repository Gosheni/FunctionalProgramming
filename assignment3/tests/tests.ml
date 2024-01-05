(*
  Part II: Tests
 
  In this part, you will need to create and run your own tests.  Tests should
  cover both common cases and edge cases.  In previous assignments, we only
  asked for a specified number of additional tests, but in this assignment we
  will be grading based on code coverage.
 
  Aim for complete code coverage on all functions, and we will check 
  by running the bisect tool on your code.  For that reason, you need 
  to add the following line in the dune file for your library:
      
      (preprocess (pps bisect_ppx))
 
  or else your tests will not run in the autograder.

 Additionally, you will need to write a special suite of tests here which
 verifies some invariants.  See the assignment for details.
 
*)
open Core
open OUnit2
module D = Finite_group
module R = Ring
module P = Postfix_calc

let test_add_Z5 _ =
  match (D.Z5_add.of_int 3), (D.Z5_add.of_int 4) with
  | Some v1, Some v2 -> 
    assert_equal 2 @@ D.Z5_add.to_int (D.Z5_add.op v1 v2)
  | _, _ -> assert_bool "Failed test_add_Z5" false;

  match (D.Z5_add.of_int 2), (D.Z5_add.of_int 4) with
  | Some v1, Some v2 -> 
    assert_equal 1 @@ D.Z5_add.to_int (D.Z5_add.op v1 v2)
  | _, _ -> assert_bool "Failed test_add_Z5" false

let test_inverse_Z5 _ =
  match D.Z5_add.of_int 0 with
  | Some v ->
    assert_equal 0 @@ D.Z5_add.to_int (D.Z5_add.inverse v)
  | _ -> assert_bool "Failed test_inverse_Z5" false;

  match D.Z5_add.of_int 2 with
  | Some v ->
    assert_equal 3 @@ D.Z5_add.to_int (D.Z5_add.inverse v)
  | _ -> assert_bool "Failed test_inverse_Z5" false
  
let test_of_int_to_int _ =
  assert_equal (Some 3) @@ (D.Z5_add.of_int 3 |> Option.map ~f:D.Z5_add.to_int);
  assert_equal None @@ (D.Z5_add.of_int 6);
  assert_equal None @@ (D.Z5_add.of_int (-1))
  
let test_identity _ =
  assert_equal 0 @@ (D.Z5_add.to_int D.Z5_add.id)

let finite_tests = "finite tests" >: test_list [
  "Test addition in Z5"    >:: test_add_Z5;
  "Test inverse in Z5"     >:: test_inverse_Z5;
  "Test of_int and to_int conversions" >:: test_of_int_to_int;
  "Test identity element"  >:: test_identity;
]

let test_add_Z4 _ =
  match (R.Z4.of_string "1"), (R.Z4.of_string "1") with
  | Some v1, Some v2 -> 
    assert_equal "2" @@ R.Z4.to_string (R.Z4.(+) v1 v2)
  | _, _ -> 
    assert_bool "Failed test_add_Z4" false;

  match (R.Z4.of_string "2"), (R.Z4.of_string "1") with
  | Some v1, Some v2 -> 
    assert_equal "3" @@ R.Z4.to_string (R.Z4.(+) v1 v2)
  | _, _ -> 
    assert_bool "Failed test_add_Z4" false

let test_multiply_Z4 _ =
  match (R.Z4.of_string "2"), (R.Z4.of_string "0") with
  | Some v1, Some v2 -> 
    assert_equal "0" @@ R.Z4.to_string (R.Z4.( * ) v1 v2)
  | _, _ -> 
    assert_bool "Failed test_multiply_Z4" false;

  match (R.Z4.of_string "2"), (R.Z4.of_string "1") with
  | Some v1, Some v2 -> 
    assert_equal "2" @@ R.Z4.to_string (R.Z4.( * ) v1 v2)
  | _, _ -> 
    assert_bool "Failed test_multiply_Z4" false

let ring_tests = "ring tests" >::: [
  "Test addition in Z4"    >:: test_add_Z4;
  "Test multiplication in Z4" >:: test_multiply_Z4;
]

let test_make_eval _ =
  match (P.Int_data.of_string "7") with 
  | Some v -> 
    assert_equal (Ok v) (P.Int_eval.eval "3 4 +")
  | None ->
    assert_bool "Failed test_int_eval" false;
  match (P.Int_data.of_string "12") with 
  | Some v -> 
    assert_equal (Ok v) (P.Int_eval.eval "3 4 *")
  | None ->
    assert_bool "Failed test_int_eval" false;
  assert_equal (Error "Not enough operands") (P.Int_eval.eval "3 +");
  assert_equal (Error "unmatched") (P.Int_eval.eval "3 4 5 +");
  assert_equal (Error "Invalid operator") (P.Int_eval.eval "3 4 /");

  match (P.Rat_data.of_string "3/4") with 
  | Some v -> 
    assert_equal (Ok v) (P.Rat_eval.eval "1/2 1/4 +")
  | None ->
    assert_bool "Failed test_rat_eval" false;
  match (P.Rat_data.of_string "1/8") with 
  | Some v -> 
    assert_equal (Ok v) (P.Rat_eval.eval "1/2 1/4 *")
  | None ->
    assert_bool "Failed test_rat_eval" false;
  assert_equal (Error "Not enough operands") (P.Rat_eval.eval "1/2 +");
  assert_equal (Error "unmatched") (P.Rat_eval.eval "1/2 1/4 1/8 +");
  assert_equal (Error "Invalid operator") (P.Rat_eval.eval "1/2 1/4 /")

let test_make_data _ =
  match (P.Int_data.of_string "12") with 
  | Some v ->
    assert_equal (Some ("", v)) (P.Int_data.next "12")
  | None -> 
    assert_bool "Failed make_int_data" false;
  match (P.Int_data.of_string "12") with 
  | Some v ->
    assert_equal (Some ("@", v)) (P.Int_data.next "12@")
  | None -> 
    assert_bool "Failed make_int_data" false;
  match (P.Int_data.of_string "-5") with 
  | Some v ->
    assert_equal (Some (" 6 +", v)) (P.Int_data.next "-5 6 +")
  | None -> 
    assert_bool "Failed make_int_data" false;
  match (P.Rat_data.of_string "1/2") with 
  | Some v ->
    assert_equal (Some (" 3/4 *", v)) (P.Rat_data.next "1/2 3/4 *")
  | None -> 
    assert_bool "Failed make_rat_data" false;
  match (P.Rat_data.of_string "-3/4") with 
  | Some v ->
      assert_equal (Some ("", v)) (P.Rat_data.next " -3/4")
  | None -> 
    assert_bool "Failed make_rat_data" false;

  assert_equal (None) (P.Int_data.next "abc");
  assert_equal (None) (P.Rat_data.next "-/3");
  assert_equal (None) (P.Rat_data.next "1/0")

let post_test_addition _ =
  match (P.IntRing.of_string "2"), (P.IntRing.of_string "3") with
  | Some v1, Some v2 ->
    assert_equal "5" @@ P.IntRing.to_string (P.IntRing.( + ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_addition" false;
  match (P.IntRing.of_string "0"), (P.IntRing.of_string "0") with
  | Some v1, Some v2 ->
    assert_equal "0" @@ P.IntRing.to_string (P.IntRing.( + ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_addition" false;
  match (P.IntRing.of_string "-2"), (P.IntRing.of_string "1") with
  | Some v1, Some v2 ->
    assert_equal "-1" @@ P.IntRing.to_string (P.IntRing.( + ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_addition" false

let post_test_multiplication _ =
  match (P.IntRing.of_string "2"), (P.IntRing.of_string "3") with
  | Some v1, Some v2 ->
    assert_equal "6" @@ P.IntRing.to_string (P.IntRing.( * ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_multiplication" false;
  match (P.IntRing.of_string "0"), (P.IntRing.of_string "7") with
  | Some v1, Some v2 ->
    assert_equal "0" @@ P.IntRing.to_string (P.IntRing.( * ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_multiplication" false;
  match (P.IntRing.of_string "-2"), (P.IntRing.of_string "2") with
  | Some v1, Some v2 ->
    assert_equal "-4" @@ P.IntRing.to_string (P.IntRing.( * ) v1 v2)
  | _, _ -> 
    assert_bool "Failed post_test_multiplication" false

let test_reduce_rat _ =
  match (P.RatRing.of_string "2/5") with
  | Some v ->
    assert_equal "2/5" @@ P.RatRing.to_string v
  | _ -> 
    assert_bool "Failed post_test_addition" false;
  match (P.RatRing.of_string "3/6") with
  | Some v ->
    assert_equal "1/2" @@ P.RatRing.to_string v
  | _ -> 
    assert_bool "Failed post_test_addition" false;
  match (P.RatRing.of_string "6/14") with
  | Some v ->
    assert_equal "3/7" @@ P.RatRing.to_string v
  | _ -> 
    assert_bool "Failed post_test_addition" false;
  match (P.RatRing.of_string "0") with
  | Some v ->
    assert_equal "0" @@ P.RatRing.to_string v
  | _ -> 
    assert_bool "Failed post_test_addition" false

let test_add_rat _ =
  match (P.RatRing.of_string "3/5"), (P.RatRing.of_string "1/10") with 
  | Some v1, Some v2 ->
    assert_equal "7/10" @@ P.RatRing.to_string (P.RatRing.( + ) v1 v2)
  | _ ->
    assert_bool "Failed test_add_rat" false;

  match (P.RatRing.of_string "2/7"), (P.RatRing.of_string "3/7") with 
  | Some v1, Some v2 ->
    assert_equal "5/7" @@ P.RatRing.to_string (P.RatRing.( + ) v1 v2)
  | _ ->
    assert_bool "Failed test_add_rat" false;

  match (P.RatRing.of_string "-3/5"), (P.RatRing.of_string "-1/10") with 
  | Some v1, Some v2 ->
    assert_equal "-7/10" @@ P.RatRing.to_string (P.RatRing.( + ) v1 v2)
  | _ ->
    assert_bool "Failed test_add_rat" false;

  match (P.RatRing.of_string "1/4"), (P.RatRing.of_string "-3/4") with 
  | Some v1, Some v2 ->
    assert_equal "-1/2" @@ P.RatRing.to_string (P.RatRing.( + ) v1 v2)
  | _ ->
    assert_bool "Failed test_add_rat" false

let test_multiply_rat _ =
  match (P.RatRing.of_string "2/5"), (P.RatRing.of_string "3/7") with 
  | Some v1, Some v2 ->
    assert_equal "6/35" @@ P.RatRing.to_string (P.RatRing.( * ) v1 v2)
  | _ ->
    assert_bool "Failed test_multiply_rat" false;

  match (P.RatRing.of_string "1/3"), (P.RatRing.of_string "2/5") with 
  | Some v1, Some v2 ->
    assert_equal "2/15" @@ P.RatRing.to_string (P.RatRing.( * ) v1 v2)
  | _ ->
    assert_bool "Failed test_multiply_rat" false;

  match (P.RatRing.of_string "-2/5"), (P.RatRing.of_string "3/7") with 
  | Some v1, Some v2 ->
    assert_equal "-6/35" @@ P.RatRing.to_string (P.RatRing.( * ) v1 v2)
  | _ ->
    assert_bool "Failed test_multiply_rat" false;
    
  match (P.RatRing.of_string "-1/3"), (P.RatRing.of_string "-2/5") with 
  | Some v1, Some v2 ->
    assert_equal "2/15" @@ P.RatRing.to_string (P.RatRing.( * ) v1 v2)
  | _ ->
    assert_bool "Failed test_multiply_rat" false
  
let postfix_tests = "postfix_tests" >: test_list [
  "Test make_data"   >:: test_make_data;
  "Test make_eval"   >:: test_make_eval;
  "Test post_addition"    >:: post_test_addition;
  "Test post_multiplication" >:: post_test_multiplication;
  "Test post_reduce_fraction"  >:: test_reduce_rat;
  "Test post_add_rationals" >:: test_add_rat;
  "Test post_multiply_rationals" >:: test_multiply_rat;
]

let series = "Assignment3 Tests" >::: [
  finite_tests;
  ring_tests;
  postfix_tests;
]

let () = 
  run_test_tt_main series

