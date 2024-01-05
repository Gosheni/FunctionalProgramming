(* Intentionally left empty until your implementation *)
module type S = sig
  type t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Make_modular_ring (Params : sig val n : int end) : S = struct
  type t = int
  let ( + ) a b = (a + b) mod Params.n
  let ( * ) a b = (a * b) mod Params.n
  let of_string s =
    try
      let x = int_of_string s in
      if x >= Params.n then None else Some x
    with _ -> None
  let to_string x = string_of_int x
end

module Z4 : S = Make_modular_ring (struct let n = 4 end)
