(* *********************************************************** *)
(* ********************** P A R T I ************************** *)
(* *********************************************************** *)

open Core

(*
  Your implementation goes here. We provide just a little bit of starter code to give you
  the syntax, but you're expected to do the rest without modifying the .mli files.

  See finite_group.mli for the details.
*)

(* We must copy our types over *)
module type S =
  sig
    type t [@@deriving compare, sexp]
    val id : t
    val op : t -> t -> t
    val inverse : t -> t
    val of_int : int -> t option
    val to_int : t -> int
  end

(* Copy the Param module type here... *)
module type Params =
  sig
    val op : int -> int -> int
    val n : int
  end

module Make (P : Params) : S =
  struct
    type t = int [@@deriving compare, sexp]
    let to_int = Fn.id
    (* Fill in the rest of the module to meet the functional requirements and the signature *)

    let op (x : t) (y : t) : t =
      (P.op x y) mod P.n

    let id =
      let rec find_candidate candidate : t =
        if candidate >= P.n then
          failwith "Identity not found within the specified range" 
        else if List.for_all ~f:(fun x -> op candidate x = x && op x candidate = x) (List.init P.n ~f:(fun i -> i)) then
          candidate
        else
          find_candidate (candidate + 1)
      in
      find_candidate 0
      

    let inverse (x : t) : t =
      let rec find_inverse_candidate (candidate : t) =
        if candidate < 0 then
          failwith "Inverse not found within the specified range"
        else if (P.op candidate x) = id && (P.op x candidate) = id then
          candidate
        else
          find_inverse_candidate (candidate - 1)
      in
      find_inverse_candidate (P.n-1)

    let of_int (k : int) : t option =
      if k >= 0 && k < P.n then Some k
      else None
  end

module Z5_params : Params =
  struct
    let op x y = (x + y) mod 5
    let n = 5
  end

module Z5_add : S = Make (Z5_params)

(* complete Memoize functor here... *)
module Memoize (G : S) : S with type t = G.t = 
  struct
    include G

    module InverseMap = Map.Make(G)

    let next m = (G.to_int m) + 1 

    let rec helper m x =
      let updated_table = Map.add_exn m ~key:x ~data:(G.inverse x) in
      match G.of_int (next x) with
      | Some v -> 
        helper updated_table v
      | None -> 
        updated_table

    let inverse (x : t) : t =
      match G.of_int 0 with
      | Some v -> 
        let inverse_table = helper InverseMap.empty v in
        Map.find_exn inverse_table x
      | None ->
        failwith "0 conversion fail"
  end
  