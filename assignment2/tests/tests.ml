open Core
open OUnit2
module D = Simpledict
module T = D.Tree

open D.Dict_item

let t1 = T.(Branch
  { item = "d"
  ; left = Branch {item = "a"; left = Leaf; right = Leaf}
  ; right = Branch {item = "e"; left = Leaf; right = Leaf}})

let t2 = T.(Branch
  { item = "f"
  ; left = Branch {item = "g"; left = Leaf; right = Leaf}
  ; right = Branch {item = "a"; left = Leaf; right = Leaf}})

let test_tree_size _ =
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size t1

let test_tree_height _ =
  assert_equal 1 @@ T.height t1;
  assert_equal 1 @@ T.height t2

let test_tree_is_balanced _ =
  assert_equal true @@ T.is_balanced t1;
  assert_equal true @@ T.is_balanced t2

let test_tree_to_list _ =
  assert_equal [] @@ T.to_list T.Leaf;
  assert_equal ["a";"d";"e"] @@ T.to_list t1

let test_tree_is_ordered _ = 
  assert_equal true @@ T.is_ordered t1 ~compare:String.compare;
  assert_equal false @@ T.is_ordered t2 ~compare:String.compare

let tree_tests = "Tree tests" >::: [
  "Tree.size"        >:: test_tree_size;
  "Tree.height"      >:: test_tree_height;
  "Tree.is_balanced" >:: test_tree_is_balanced;
  "Tree.to_list"     >:: test_tree_to_list;
  "Tree.is_ordered"  >:: test_tree_is_ordered;
]

let d1 = T.(Branch
  { item = {key="d"; value=0}
  ; left = Branch {item = {key="a"; value=1}; left = Leaf; right = Leaf}
  ; right = Branch {item = {key="e"; value=2}; left = Leaf; right = Leaf}})

let d2 = T.(Branch
  { item = {key="f"; value=2}
  ; left = Branch {item = {key="g"; value=4}; left = Leaf; right = Leaf}
  ; right = Branch {item = {key="a"; value=8}; left = Leaf; right = Leaf}})

let test_size _ = 
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size d1

let test_to_list _ =
  assert_equal [("a", 1); ("d", 0); ("e", 2)] @@ D.to_list d1;
  assert_equal [("g", 4); ("f", 2); ("a", 8)] @@ D.to_list d2

let test_lookup _ = 
  assert_equal (Some 1) @@ D.lookup d1 ~key:"a";
  assert_equal None @@ D.lookup d1 ~key:"b"

let test_insert _ =
  assert_equal T.(Branch {item={key="5";value=5}; left=Leaf; right=Leaf}) @@ D.insert T.Leaf ~key:"5" ~value:5;
  assert_equal T.(Branch {item={key="5";value=6}; left=Leaf; right=Leaf}) 
  @@ D.insert (D.insert T.Leaf ~key:"5" ~value:5) ~key:"5" ~value:6

let test_map _ = 
  let tr = D.map d1 ~f:(fun _ x -> x + 10) in
  assert_equal 11 @@ D.lookup_exn tr ~key:"a";
  assert_equal 12 @@ D.lookup_exn tr ~key:"e"

(* also should test of_list, of_list_multi *)

(* test_map_one requires working lookup_exn *)
let test_map_one _ =
  let d = D.map_one d1 ~key:"a" ~f:(fun _ x -> x + 10) in
  assert_equal 11 @@ D.lookup_exn d ~key:"a";
  assert_equal 2 @@ D.lookup_exn d ~key:"e"

let example_dict1 = T.(Branch
  { item = {key = "9"; value = 1}
  ; left = Branch
    { item = {key = "8"; value = 3}
    ; left = Branch { item = {key = "1"; value = 5}; left = Leaf; right = Leaf } 
    ; right = Leaf}
  ; right = Leaf}
)

let example_dict2 = T.(Branch
  { item = {key = "8"; value = 13}
  ; left = Branch
    { item = {key = "1"; value = 2}
    ; left = Leaf
    ; right = Leaf }
   ; right = Branch
    { item = {key = "99"; value = 2}
    ; left = Leaf
    ; right = Leaf }}
)

let example_dict3 = T.(Branch
  { item = {key = "1"; value = [1; 2; 3]}
  ; left = Leaf
  ; right = Leaf }
)

let example_dict4 = T.(Branch
  { item = {key = "1"; value = [1; 2]}
  ; left = Leaf
  ; right = Branch
  { item = {key = "2"; value = [3; 4]}
  ; left = Leaf
  ; right = Leaf }}
)

let example_dict5 = T.(Branch
  { item = {key = "1"; value = 1}
  ; left = Leaf
  ; right = Branch
  { item = {key = "2"; value = 4}
  ; left = Leaf
  ; right = Leaf }}
)

let test_of_list _ =
  assert_equal example_dict5 D.(of_list [("1", 1); ("2", 4)]);
  assert_equal example_dict2 D.(of_list [("8", 13); ("1", 2); ("99", 2)])

let test_of_list_multi _ =
  assert_equal example_dict3 D.(of_list_multi [("1", 1); ("1", 2); ("1", 3)]);
  assert_equal example_dict4 D.(of_list_multi [("1", 1); ("2", 3); ("1", 2); ("2", 4)])

let test_merge _ =
  assert_equal [("1", 2); ("8", 13); ("9", 1); ("99", 2)] D.(merge example_dict1 example_dict2 |> to_list)

let merge_fun l r =
  match l, r with 
  | None, None -> failwith "should not get here!"
  | Some _, None -> 0
  | None, Some _ -> 1
  | Some a, Some b -> a * b

(* test_merge_with requires working to_list *)
let test_merge_with _ =
  assert_equal [("1", 10); ("8", 39); ("9", 0); ("99", 1)] D.(merge_with ~merger:merge_fun example_dict1 example_dict2 |> to_list)

let dict_tests = "dict tests" >: test_list [
    "D.size"          >:: test_size;
    "D.list"          >:: test_to_list;
    "D.lookup"        >:: test_lookup;
    "D.insert"        >:: test_insert;
    "D.map"           >:: test_map;
    "D.map_one"       >:: test_map_one;
    "D.of_list"       >:: test_of_list;
    "D.of_list_multi" >:: test_of_list_multi;
    "D.merge"         >:: test_merge;
    "D.merge_with"    >:: test_merge_with;
  ]

let test_remove_non_chars _ =
  assert_equal "fun open" @@ (Utils.remove_non_characters "fun)open");
  assert_equal "if_if_else   d" @@ (Utils.remove_non_characters "if_if_else$% d")

let test_remove_strings _ =
  assert_equal "fuen" @@ (Utils.remove_literal_strings "fu\"n(op\"en");
  assert_equal "n(opn" @@ (Utils.remove_literal_strings "\"fu\"n(op\"e\"n")

let keyword_tests = "keyword tests" >: test_list [
    "KW.remove_non_chars"    >:: test_remove_non_chars;
    "KW.remove_strings"      >:: test_remove_strings
  ]

  (* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in utils.ml and headers in utils.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [
    tree_tests;
    dict_tests;
    keyword_tests;
  ]

let () = 
  run_test_tt_main series

