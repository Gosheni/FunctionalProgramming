open Core

(*

FPSE Assignment 2

Name                  : Jacob Choi
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

See file simpledict.mli for the specification of Part I of the assignment, and keywordcount.ml for Part II.  Recall from lecture that .mli files are module signatures aka module types and you will need to provide implementations of all the functions listed there in this file. 

Your Part I answers go here, and the Part II application should go in the keywordcount.ml file. No helper functions for Part II should exist in this file beyond what is required for Part I.

Hint: start by reading the code we've put here, and then copy over parts of the .mli file to make dummy headers and fill them with `unimplemented ()`. Note the changes in syntax between .mli and .ml files.

Note that .ml files need to include all `type` declarations in .mli files.

You must leave all `[@@deriving show]` annotations, or your autograder won't work. We use this to pretty-print your results.

*)

(*let unimplemented () = failwith "unimplemented"*)

module Tree = struct
  type 'a t =
    | Leaf
    | Branch of
        { item:  'a
        ; left:  'a t
        ; right: 'a t } [@@deriving show]

  (* You are expected to implement the functions in Tree *)

  let rec size (tree : 'a t) : int =
    match tree with
    | Leaf -> 0
    | Branch { left; right; _ } -> 1 + size left + size right

  let rec height (tree : 'a t) : int =
    match tree with
    | Leaf -> -1
    | Branch { left; right; _ } ->
      let left_height = height left in
      let right_height = height right in
      1 + max left_height right_height

  let rec is_balanced (tree : 'a t) : bool =
    match tree with
    | Leaf -> true
    | Branch { left; right; _ } ->
      let left_height = height left in
      let right_height = height right in
      abs (left_height - right_height) <= 1 && is_balanced left && is_balanced right

  let to_list (tree : 'a t) : 'a list =
    let rec helper (tree : 'a t) (ls : 'a list) : 'a list =
      match tree with
      | Leaf -> ls
      | Branch { item; left; right } ->
        let acc_right = helper right ls in
        let acc_current = item :: acc_right in
        helper left acc_current
    in
    helper tree []

  let rec is_ordered (tree : 'a t) ~compare:(compare: 'a -> 'a -> int) : bool =
    match tree with
    | Leaf -> true  (* An empty tree is ordered by definition *)
    | Branch { item; left; right } ->
      let left_ordered = is_ordered left ~compare in
      let right_ordered = is_ordered right ~compare in
      left_ordered && right_ordered && 
        (* Check if the item is greater than all elements in the left subtree and less than all elements in the right subtree *)
        (match left with
         | Leaf -> true  (* No left subtree, so it's ordered with respect to the right subtree *)
         | Branch { item = left_item; _ } -> compare left_item item < 0) &&
        (match right with
         | Leaf -> true  (* No right subtree, so it's ordered with respect to the left subtree *)
         | Branch { item = right_item; _ } -> compare item right_item < 0)


end (* module Tree *)

module Dict_item = struct
  type 'a t = { key: string ; value: 'a } [@@deriving show]

  (* We implement this for you.*)
  let compare (x : 'a t) (y : 'a t) : int =
    String.compare x.key y.key
end (* module Dict_item *)


type 'a t = 'a Dict_item.t Tree.t [@@deriving show]

(* 
   We provide this for you to demonstrate that the Tree module functions work on the dict
   since the dict is a Tree.t.
*)
let size = Tree.size

(*
    You will have to implement the rest of the functions in simpledict.mli. We have copied
    over `to_list` for you to show you the syntax, and you will need to copy over the rest.
    See `Tree.is_ordered` for the syntax with named arguments.
*)

let to_list (dict : 'a t) : (string * 'a) list =
  let rec helper (dt : 'a t) (ls : (string * 'a) list) : (string * 'a) list =
    match dt with
    | Leaf -> ls
    | Branch { item; left; right } ->
      let acc_right = helper right ls in
      let acc_current = (item.key, item.value) :: acc_right in
      helper left acc_current
  in
  helper dict []

let rec lookup (dict : 'a t) ~key:(key : string) : 'a option =
  match dict with
  | Leaf -> None
  | Branch { item; left; right } ->
    let comp_val = String.compare key item.key in
    if comp_val < 0 then lookup left ~key
    else if comp_val > 0 then lookup right ~key
    else Some item.value

exception KeyNotFound of string

let rec lookup_exn (dict : 'a t) ~key:(key : string) : 'a =
  match dict with
  | Leaf -> raise (KeyNotFound key)
  | Branch { item; left; right } ->
    let comp_val = String.compare key item.key in
    if comp_val < 0 then lookup_exn left ~key
    else if comp_val > 0 then lookup_exn right ~key
    else item.value

let rec insert (dict : 'a t) ~key:(key : string) ~value:(value : 'a) : 'a t = 
  match dict with
  | Leaf -> Branch { item = { key; value }; left = Leaf; right = Leaf }
  | Branch { item; left; right } ->
    let comp_val = String.compare key item.key in
    if comp_val = 0 then
      Branch { item = { key; value }; left; right }
    else if comp_val < 0 then
      Branch { item; left = insert left ~key ~value; right }
    else
      Branch { item; left; right = insert right ~key ~value }

let of_list (ls : (string * 'a) list) : 'a t =
  let rec helper (dict : 'a t) (pairs : (string * 'a) list) : 'a t =
    match pairs with
    | [] -> dict
    | (key, value) :: rest ->
      let updated_dict = insert dict ~key ~value in
      helper updated_dict rest
  in
  helper Leaf ls

let of_list_multi (ls : (string * 'a) list) : 'a list t =
  let rec append_to_end (lst : 'a list) (elem : 'a) : 'a list =
    match lst with
    | [] -> [elem]  (* If the list is empty, create a new list with the element. *)
    | head :: tail -> head :: append_to_end tail elem
  in
  let rec insert2 (dict : 'a list t) ~key:(key : string) ~value:(value : 'a) : 'a list t =
    match dict with
    | Leaf -> Branch { item = { key; value = [value] }; left = Leaf; right = Leaf }
    | Branch { item; left; right } ->
      let comp_val = String.compare key item.key in
      if comp_val = 0 then
        Branch { item = { key; value = append_to_end item.value value }; left; right }
      else if comp_val < 0 then
        Branch { item; left = insert2 left ~key ~value; right }
      else
        Branch { item; left; right = insert2 right ~key ~value }
  in
  let rec helper (dict : 'a list t) (pairs : (string * 'a) list) : 'a list t =
    match pairs with
    | [] -> dict
    | (key, value) :: rest ->
      let updated_dict = insert2 dict ~key ~value in
      helper updated_dict rest
  in
  helper Leaf ls

let rec map (dict : 'a t) ~f:(f : string -> 'a -> 'b) : 'b t =
  match dict with
  | Leaf -> Leaf
  | Branch { item = { key; value }; left; right } ->
    let left_tree = map left ~f in
    let right_tree = map right ~f in
    Branch { item = { key; value = f key value }; left = left_tree; right = right_tree }

let rec map_one (dict : 'a t) ~key:(key : string) ~f:(f : string -> 'a -> 'a) : 'a t =
  match dict with
  | Leaf -> Leaf
  | Branch { item; left; right } ->
    let comp_val = String.compare key item.key in
    if comp_val < 0 then 
      Branch { item; left = map_one left ~key ~f; right }
    else if comp_val > 0 then 
      Branch { item; left; right = map_one right ~key ~f }
    else 
      Branch { item = { key; value = (f item.key item.value) }; left; right }

let merge (dict1 : 'a t) (dict2 : 'a t) : 'a t =
  match dict1, dict2 with
  | Leaf, dict2 -> dict2  
  | dict1, Leaf -> dict1  
  | dict1, dict2->
    let rec helper (dict : 'a t) (pairs : (string * 'a) list) : 'a t =
      match pairs with
      | [] -> dict
      | (key, value) :: rest ->
        let updated_dict = insert dict ~key ~value in
        helper updated_dict rest
    in
    let list2 = to_list dict2 in
    helper dict1 list2
        

let merge_with (dict1 : 'a t) (dict2 : 'b t) ~merger:(merger : 'a option -> 'b option -> 'c) : 'c t =
  let list1 = to_list dict1 in
  let list2 = to_list dict2 in
  let rec helper (ans1 : 'c t) (ls1 : (string * 'b) list) : 'c t =
    match ls1 with
    | [] -> ans1
    | (key, value) :: rest ->
      let lkup = lookup dict1 ~key in
      let updated_value =
        match lkup with
        | None -> merger None (Some value)
        | Some v -> merger (Some v) (Some value)
      in
      let ans2 = insert ans1 ~key ~value:updated_value in
      helper ans2 rest
  in
  let rec helper2 (ans : 'c t) (ls : (string * 'a) list) : 'c t =
    match ls with
    | [] -> ans
    | (key, value) :: rest ->
      let lkup = lookup dict2 ~key in
      match lkup with
      | Some _ -> helper2 ans rest
      | None -> helper2 (insert ans ~key ~value:(merger (Some value) None)) rest
  in
  helper2 (helper Leaf list2) list1
