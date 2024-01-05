open OUnit2
open Core
open Lib

module type MyRandom = sig 
  val int : int -> int 
end

module MyRandomImpl : MyRandom = struct
  let int (size : int) =
    Random.init 0;
    Random.int size 
end

module L = MakeDistribution (Int) (MyRandomImpl)
module K = MakeDistribution (String) (MyRandomImpl)

let test_get_ngram _ =
  (* Test when n is less than or equal to 0 *)
  assert_equal [] (L.get_ngram [1; 2; 3] 0);
  assert_equal [] (L.get_ngram [1; 2; 3] (-1));

  (* Test when (n+1) is greater than the length of the sequence *)
  assert_equal [] (L.get_ngram [1; 2; 3] 3);
  
  (* Test for valid n-grams *)
  assert_equal [1] (L.get_ngram [1; 2; 3] 1);
  assert_equal [1; 2] (L.get_ngram [1; 2; 3] 2)

let test_make_distribution _ =
  (* Test making a distribution with n-grams *)
  let sequence = [1; 2; 3; 1; 2; 3; 2; 3] in
  let dist = L.make_distribution sequence ~n:2 in

  let ngram1 = [1] in
  let bag1 = Bag.of_list [2; 2] in
  let ngram2 = [2] in
  let bag2 = Bag.of_list [3; 3; 3] in
  let ngram3 = [3] in
  let bag3 = Bag.of_list [1; 2] in

  assert_equal (L.equal_test dist ngram1 bag1) true;
  assert_equal (L.equal_test dist ngram2 bag2) true;
  assert_equal (L.equal_test dist ngram3 bag3) true;

  (* Test making a distribution with n-grams where n is larger than the sequence *)
  let dist = L.make_distribution sequence ~n:10 in
  assert_equal (L.empty dist) true;

  (* Test making a distribution from an empty sequence *)
  let dist = L.make_distribution [] ~n:2 in
  assert_equal (L.empty dist) true

let test_sample_random_sequence _ =
  let sequence = [3; 4; 1; 2; 4; 2; 4; 1; 1; 3] in

  let dist = L.make_distribution sequence ~n:2 in
  let initial_ngram, k = [2], 4 in
  let random_sequence = L.sample_random_sequence dist initial_ngram ~n:2 ~k in
  assert_equal [2; 4; 1; 2] random_sequence;
  
  let empty_dist = L.make_distribution [] ~n:2 in
  let initial_ngram, k = [2], 4 in
  let random_sequence = L.sample_random_sequence empty_dist initial_ngram ~n:2 ~k in
  assert_equal [2] random_sequence
  
let test_to_list _ =
  let sequence = [1; 2; 3; 1; 2; 3; 2; 3] in
  let dist = L.make_distribution sequence ~n:2 in
  let result_list = L.to_list dist in

  let ngram1, ngram2, ngram3 = [3], [2], [1] in
  
  let expected_list = [
    (ngram1, Some (Bag.of_list [1; 2]));
    (ngram2, Some (Bag.of_list [3; 3; 3]));
    (ngram3, Some (Bag.of_list [2; 2]));
  ] in

  assert_equal (L.equal_list_test expected_list result_list) true

let test_equal_test _ =
  let sequence = [1; 2; 3; 1; 2; 3; 2; 3] in
  let dist = L.make_distribution sequence ~n:2 in
  
  let ngram1 = [3] in
  let bag1 = Bag.of_list [1; 2] in
  assert_equal (L.equal_test dist ngram1 bag1) true;
  
  let ngram2 = [2] in
  let bag2 = Bag.of_list [3; 3; 3] in
  assert_equal (L.equal_test dist ngram2 bag2) true;
  
  let ngram3 = [1] in
  let bag3 = Bag.of_list [2; 3] in
  assert_equal (L.equal_test dist ngram3 bag3) false;  (* Different bags *)
  
  let ngram4 = [4] in
  let bag4 = Bag.of_list [] in
  assert_equal (L.equal_test dist ngram4 bag4) true  (* Ngram not in the distribution *)
  
let test_equal_list_test _ =
  let sequence = [1; 2; 3; 1; 2; 3; 2; 3] in
  let dist = L.make_distribution sequence ~n:2 in
  
  let ngram1, ngram2, ngram3 = [3], [2], [1] in
  let bag1 = Some (Bag.of_list [1; 2]) in
  let bag2 = Some (Bag.of_list [3; 3; 3]) in
  let bag3 = Some (Bag.of_list [2; 2]) in
  
  let expected_list = [
    (ngram1, bag1);
    (ngram2, bag2);
    (ngram3, bag3);
  ] in

  assert_equal (L.equal_list_test expected_list (L.to_list dist)) true;
  
  let ngram4 = [4] in
  let bag4 = Some (Bag.of_list [1; 1]) in
  let ngram5 = [5] in
  let bag5 = None in
  
  let expected_list2 = [
    (ngram4, bag4);
    (ngram5, bag5);
  ] in
  
  assert_equal (L.equal_list_test expected_list2 (L.to_list dist)) false  (* Different bags *)

let test_empty _ =
  let sequence = [1; 2; 3; 1; 2; 3; 2; 3] in
  let dist = L.make_distribution sequence ~n:2 in
  assert_equal (L.empty dist) false;  (* The distribution is not empty *)
    
  let empty_dist = L.make_distribution [] ~n:2 in
  assert_equal (L.empty empty_dist) true;  (* The distribution is empty *)
    
  let dist2 = L.make_distribution sequence ~n:10 in
  assert_equal (L.empty dist2) true  

(* Sanity function taken directly from ngram.ml *)
let sanitize_word word =
  String.lowercase (String.filter word ~f:Char.is_alpha)

let is_valid_key (key, _) =
  List.for_all key ~f:(fun s ->
    String.for_all s ~f:(fun c -> Char.is_alpha c) &&
    String.for_all s ~f:(fun c -> Char.is_lowercase c) &&
    not (String.contains s ' ')
  )
  
let quickcheck_make_distribution _ =
  Quickcheck.test ~sexp_of:[%sexp_of: (string list * int)]
    (Quickcheck.Generator.tuple2
      (Quickcheck.Generator.list String.quickcheck_generator)
      (Int.gen_incl 2 10)
      |> Quickcheck.Generator.filter_map ~f:(fun (sequence, n) ->
        let non_empty_sequence = List.filter ~f:(fun s -> not (String.is_empty s)) sequence in
        if List.is_empty non_empty_sequence then None
        else Some (non_empty_sequence, n)
      )
    )
  ~f:(fun (sequence, n) ->
    let words = List.map ~f:sanitize_word sequence in
    let dist = K.make_distribution words ~n in
  
    let list_check = K.to_list dist |> List.for_all ~f:is_valid_key in
  
    assert(list_check)
  )

let lib_tests = "lib_tests" >: test_list [
  "test_get_ngram" >:: test_get_ngram;
  "test_make_distribution" >:: test_make_distribution;
  "test_sample_random_sequence" >:: test_sample_random_sequence;
  "test_to_list" >:: test_to_list;
  "test_equal_test" >:: test_equal_test;
  "test_equal_list_test" >:: test_equal_list_test;
  "test_empty" >:: test_empty;
  "quickcheck_make_distribution" >:: quickcheck_make_distribution;
]

let series = "Assignment4 Tests" >::: [
  lib_tests;
]

let () =
  run_test_tt_main series
