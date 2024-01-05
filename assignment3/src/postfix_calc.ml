(* Intentionally left empty until your implementation *)
open Core 

[@@coverage off]

module type Data =
  sig
    include Ring.S (* Has everything that ring has, and more! *)
    val next : string -> (string * t) option
  end

module type Eval =
  sig
    type t
    val eval : string -> (t, string) result
  end

[@@coverage on]

module Make_eval (Data : Data) : Eval with type t = Data.t = struct
  type t = Data.t
  
  let eval expression =
    let stack = Stack.create () in
  
    let handle_operator (operator : string) =
      match Stack.pop stack, Stack.pop stack with
      | Some x, Some y -> (
          match operator with
          | "+" -> Stack.push stack (Data.(+) x y)
          | "*" -> Stack.push stack (Data.( * ) x y)
          | _ -> raise (Invalid_argument "Invalid operator")
        )
      | _ -> raise (Invalid_argument "Not enough operands")
    in
    let rec process_tokens tokens =
      match tokens with
      | [] -> (
          match Stack.pop stack with
          | Some result -> Ok result
          | None -> Error "unmatched"
        )
      | token :: rest_tokens -> (
          match Data.next token with
          | Some (_, value) -> 
            Stack.push stack value; process_tokens rest_tokens
          | None -> (
            match token with
            | "+" | "*" -> handle_operator token; process_tokens rest_tokens
            | _ -> Error "illegal character"
          )
        )
    in
    process_tokens (Str.split (Str.regexp " ") expression)
end

module Make_data (Ring : Ring.S) : Data = struct
  include Ring

  let is_digit c = Char.(c >= '0' && c <= '9')

  let next input =
    let input = String.strip input in
    let len = String.length input in

    let rec read_digits i acc =
      if i > 0 && is_digit input.[len - i] then
        read_digits (i - 1) (acc ^ String.make 1 input.[len - i])
      else if i > 1 && Char.(input.[len - i] = '/') then
        if i < len && is_digit input.[len - i - (-1)] then
          read_digits (i - 1) (acc ^ String.make 1 input.[len - i])
        else (i, acc)
      else (i, acc)
    in

    match len with
    | 0 -> None
    | _ -> 
      match input.[0] with
      | '-' -> (
        let i, digits = read_digits (len-1) "" in
        if i = (len-1) then None
        else 
          match of_string ("-" ^ digits) with
          | Some n -> Some (String.sub input ~pos:(len-i) ~len:i, n)
          | None -> None
        )
      | _ -> (
        let i, digits = read_digits len "" in
        if i = len then None
        else 
          match of_string digits with
          | Some n -> Some (String.sub input ~pos:(len-i) ~len:i, n)
          | None -> None
      )
end

module IntRing : Ring.S = struct
  type t = int
  let ( + ) a b = a + b
  let ( * ) a b = a * b
  let of_string s =
    try
      let x = int_of_string s in
      Some x
    with _ -> None
  let to_string x = string_of_int x
end

module RatRing : Ring.S = struct
  type t = string

  let gcd a b =
    let rec gcd' a b =
      if b = 0 then a
      else gcd' b (a mod b)
    in
    gcd' (abs a) (abs b)

  let reduce s =
      match String.split_on_chars s ~on:['/'] with
      | [num_str; den_str] ->
        let num, den = int_of_string num_str, int_of_string den_str in
        let common_div = gcd num den in
        let simp_num, simp_den = num / common_div, den / common_div in
        (string_of_int simp_num) ^ "/" ^ (string_of_int simp_den)
      | _ -> s  (* Return as is if the input is not a valid fraction *)

  let ( + ) a b = 
    match (String.split_on_chars a ~on:['/'], String.split_on_chars b ~on:['/']) with
    | ([num1_str; den1_str], [num2_str; den2_str]) ->
      let num1, den1, num2, den2 = int_of_string num1_str, int_of_string den1_str, 
        int_of_string num2_str, int_of_string den2_str in
      let common_den = den1 * den2 in
      let new_num = (num1 * den2) + (num2 * den1) in
      reduce ((string_of_int new_num) ^ "/" ^ (string_of_int common_den))
    | _ -> a  
  let ( * ) a b =
    match (String.split_on_chars a ~on:['/'], String.split_on_chars b ~on:['/']) with
    | ([num1_str; den1_str], [num2_str; den2_str]) ->
      let num1, den1, num2, den2 = int_of_string num1_str, int_of_string den1_str, 
        int_of_string num2_str, int_of_string den2_str in
      let new_num, new_den = num1 * num2, den1 * den2 in
      reduce ((string_of_int new_num) ^ "/" ^ (string_of_int new_den))
    | _ -> a  

  let of_string s = Some (reduce s)
  let to_string x = reduce x  (* Convert the fraction to a string using the identity function *)
end

module Z4_data = Make_data (Ring.Z4)
module Int_data = Make_data (IntRing)
module Rat_data = Make_data (RatRing)

module Z4_eval = Make_eval (Z4_data)
module Int_eval = Make_eval (Int_data)
module Rat_eval = Make_eval (Rat_data)