(* Intentionally left empty until your implementation *)
open Core

module type MyRandom = sig 
  val int : int -> int 
end

module type Distribution = sig
  type t
  type item

  val get_ngram : item list -> int -> item list
  val make_distribution : item list -> n:int -> t
  val sample_random_sequence : t -> item list -> n:int -> k:int -> item list
  val equal_test : t -> item list -> item Bag.t -> bool
  val to_list : t -> (item list * item Bag.t option) list
  val equal_list_test : (item list * item Bag.t option) list -> (item list * item Bag.t option) list -> bool
  val empty : t -> bool
end

module MakeDistribution (Item : Map.Key) (R : MyRandom) : Distribution with type item = Item.t [@@ocaml.warning "-67"]

