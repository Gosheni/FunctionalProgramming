
(*

FPSE Assignment 1
 
Name                  : Jacob Choi
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make it recursive. In some cases, you will find it helpful to define auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these questions (which we have not taught yet in any case): no arrays, for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* 
	You are required to use the Core libraries, don't remove the following line. If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"

(* 
	This homework is structured into modules. You are to complete module Part1 and all sections within it by the first due date.

	These modules act as containers for your functions.

	You will not need to modify any lines with 'module', 'end', or 'include'. You WILL need to modify the functions within the modules.
*)

module Part1 = struct
	(*
		Part I Section 1: simple numeric recursions.
		
		All functions must be total for the specified domain;	overflow is excluded from this restriction but should be avoided.
		
	*)
	module Section1 = struct

		(*
			Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
			(don't use the closed-form solution, do the actual addition).
		*)
		let rec summate (n: int): int =
			if n = 0 then 0
			else n + summate (n-1)

		(*
			Given non-negative integers `n` and `m`, compute their least common multiple.
		*)
		let rec gcd (n: int) (m: int): int =
			if m = 0 then n
			else gcd m (n mod m)

		let lcm (n: int) (m: int): int =
			if n = 0 || m = 0 then 0
			else (n * m)/(gcd n m)

		(*
			Given a non-negative integer `n`, compute the n-th fibonacci number.	Give an implementation that does not take exponential time; the naive version from lecture is exponential	since it has two recursive calls for each call.
		*)
		let fibonacci (n: int): int =
			let rec fibonacci_helper (n: int) (a: int) (b: int): int =
				if n = 0 then a
				else fibonacci_helper (n-1) b (a+b)  
			in
			fibonacci_helper n 0 1
			

	end (* module Section1 *)

	include Section1

	module Section2 = struct
		(*
			Part I Section 2: building and modifying lists. The List. module functions may NOT be used (yet).
		*)
			
		(*
			Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
		*)
		let rec iota1 (n: int): int list =
			if n = 0 then []
			else n :: iota1 (n-1)
			
		(*
			Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],	without taking O(n^2) time.
		*)
		let iota2 (n: int): int list =
			let rec generate (acc: int list) (n: int): int list =
				if n = 0 then acc
				else generate (n :: acc) (n-1)
			in
			generate [] n
			
		(*
			Given a positive integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
		*)
		let factors (n: int): int list =
			let rec generate (acc: int list) (a: int): int list =
				if a = 0 then acc
				else if n mod a = 0 then generate (a :: acc) (a-1)
				else generate acc (a-1)
			in
			generate [] n

		(* 
			 Reverse a list. Your solution must be in O(n) time. Note: the solution in lecture is O(n^2).
		*)
		let reverse (ls : 'a list) : 'a list =
			let rec reverse_list (acc: 'a list) (newA: 'a list): 'a list =
				match acc with
				| [] -> newA
				| head :: tail -> reverse_list tail (head :: newA)
			in
			reverse_list ls []

		(* 
			Rotate a list to the right. That is, move index i to index i + k, and wrap the list around
			where necessary.
			e.g. rotate_list [1;2;3;4;5] 3 evaluates to [3;4;5;1;2]
			`k` is non-negative with no further constraints.
		*)
		let rotate_list (ls : 'a list) (k : int) : 'a list =
			let rec count (ls : 'a list) (k : int) : int =
				match ls with
				| [] -> (-1*k)
				| _ :: tail -> count tail (k-1)
			in
			let rec rotate (ls : 'a list) (sec : 'a list) (k : int) : 'a list =
				if k = 0 then ls @ (reverse sec)
				else match ls with
				| [] -> reverse sec
				| head :: tail -> rotate tail (head :: sec) (k-1)
			in
			rotate ls [] (count ls k)
		
	end (* module Section2 *)

	include Section2

	module Section3 = struct
		(*
			Part I Section 3: strings, lists, and sorting.  The List module functions cannot be used.	String comparisons operations such as String.(<=) can be used, but no other String module functions.
		*)

		(*
			Given a list of strings, check to see if it is ordered, i.e. whether earlier elements are less than or equal to later elements.
		*)
		let is_ordered (ls: string list): bool =
			let rec compare (lis: string list) (prev: string): bool =
				match lis with
				| [] -> true
				| head :: tail -> if String.(<=) prev head then compare tail head else false
			in
			match ls with
			| [] -> true
			| head :: tail -> compare tail head

		(*
			Define a function to remove the lexicographically maximum string in a list of strings.
			Return
			Error("empty list") if the input list is empty (and has no max)
			Ok(s,s_list) for s the maximum string and s_list the list with s removed, 
				if the list is not empty.

			If there are more than one max string, remove the max string that occurs last in the list.
			Relative order of the list must be maintained except for the single removed element.
		*)
		let rec remove_max (l: string list): (string * string list, string) result =
			match l with
  		| [] -> Error("empty list")
  		| [x] -> Ok(x, [])
  		| head :: tail ->
    		match remove_max tail with
				| Error e -> Error e
    		| Ok (max_str, remaining) ->
      		if String.(>=) head max_str then Ok(head, tail)
					else Ok (max_str, head :: remaining)
		(*
			Write a sort routine by repeated invocations of remove_max to pull out the largest
			elements one-by-one.  You should never need to invoke `remove_max` on an empty
			list, and you can thus `assert false` (an invariant failure) if the `Error`
			case is ever returned from `remove_max`.  
			This problem shows how we can manually encode the exceptional condition in `
			remove_max` with `Ok`/`Error` but convert it to an actual side effect here
			(the `assert false` will raise an exception if hit).

			Max sort on an empty list should return an empty list, not throw an exception.
			The sorted list should be sorted from smallest to largest string lexicographically
		*)
		let max_sort (l: string list): string list =
			let rec generate (ls: string list) (aux: string list): string list =
				match remove_max ls with
				| Error e -> aux
				| Ok(max_str, remaining) -> generate remaining (max_str :: aux)
			in
			generate l []

		(* 
			Split a list `ls` into two pieces, the first of which is the first `n` elements of `ls`,
			and the second is all remaining elements.
			e.g. split_list [1;2;3;4;5] 3 evaluates to ([1;2;3], [4;5])	 
			Note that this function returns a tuple. Here is an example of a tuple.
			```
			let f x = (x, 10)
			```
			Assume `n` is non-negative.
		*)
		let split_list (ls : 'a list) (n : int) : 'a list * 'a list =
			let rec split (ls : 'a list) (sec: 'a list) (n: int) : 'a list * 'a list =
				if n = 0 then (reverse sec, ls)
				else match ls with
				| [] -> (sec, ls)
				| head :: tail -> split tail (head :: sec) (n-1)
			in
			split ls [] n

		(* 
			 Sort an int list using merge sort. Your solution must be O(n log n).
		*)
		let rec merge_sort (ls : int list) = 
			let rec split (lst : int list) (left : int list) (right : int list) : 'a list * 'a list =
				match lst with
				| [] -> (left, right)
				| x::xs -> split xs (x::right) left
			in
			let rec merge (left : int list) (right : int list) =
				match (left, right) with
				| ([], r) -> r
				| (l, []) -> l
				| (x::xs, y::ys) ->
						if x < y then x :: merge xs (y::ys)
						else y :: merge (x::xs) ys
			in
			match ls with
			| [] -> []
			| [x] -> [x]
			| _ ->
					let (left, right) = split ls [] [] in
					let sorted_left = merge_sort left in
					let sorted_right = merge_sort right in
					merge sorted_left sorted_right
			
		
	end (* module Section3 *)

	include Section3

end (* module Part1 *)

include Part1

(* *************
    END PART I
   ************* *)		

(* ***************
		BEGIN PART II
	 *************** *)

module Part2 = struct

	module Section1 = struct
		(*
			Part II Section 1: for selected functions in Part I, provide a reimplementation of your previous code by refactoring the definition to use combinators provided by the List module.
			
			Care should be taken to use a concise, elegant combination of these provided functions to best express the task.
			
			These new implementations should not be explicitly recursive.	Note that the autograder is not aware if you cheated and used recursion; we will manually inspect your code and give you negative points if 
			you used recursion.
		*)
		let iota1' (n: int): int list = 
			if n = 0 then []
			else List.init n ~f:(fun i -> n-i)

		let iota2' (n: int): int list =
			if n = 0 then []
			else List.init n ~f:(fun i -> i+1)

		let factors' (n: int): int list =
			List.init n ~f:(fun i -> i+1) |> List.filter ~f:(fun x -> n mod x = 0)

		let split_list' (ls : 'a list) (n : int) : 'a list * 'a list =
			List.split_n ls n

	end (* module Section1 *)

	include Section1

	module Section2 = struct
		(*
			Part II Section 2: primes. In this section we focus on numeric recursive functions.
		*)

		(* 
			 Check if positive integer `n` is prime. This should be able to quickly handle numbers up to 2^32.
		*)
			let is_prime (n : int) : bool =
				let rec check_prime (x : int) : bool =
					if x*x > n then true 
					else if n mod x = 0 || n mod (x+2) = 0 then false
					else check_prime (x+6)
				in
				if n <= 1 then false
				else if n = 2 || n = 3 then true
				else if n % 2 = 0 || n % 3 = 0 then false 
				else check_prime 5

		(* 
			Given a positive integer `n`, return the prime factor of `n` that has the greatest multiplicity.
			e.g. if n = 120, then its prime factors are 2, 2, 2, 3, 5, so the factor with the greatest
				multiplicity is 2.
			If two prime factors have the same multiplicity, return the largest factor.
		*)
		let prime_factor_with_greatest_multiplicity (n : int) : int =
			let rec factors (d : int) (n : int) (count : int) (m : int) (ans : int) : int =
				if n = 1 then 
					if count >= m then d
					else ans
				else if n mod d = 0 then factors d (n / d) (count+1) m ans
				else if count >= m then factors (d + 1) n 0 count d
				else factors (d + 1) n 0 m ans
			in
			factors 2 n 0 1 1
	end (* module Section2 *)

	include Section2

	module Section3 = struct
		(*
		Part II Section 3: Checking validity of a Towers game solution

		Towers is a simple puzzle game, for rules and to play the game see
		https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html

		Here is a description taken from that site:

		Your task is to build a tower on every square, in such a way that:
		* Each row contains every possible height of tower once
		* Each column contains every possible height of tower once
		* Each numeric clue describes the number of towers that can be seen if you look into the square from that direction, assuming that shorter towers are hidden behind taller ones. For example, in a 5×5 grid, a clue marked ‘5’ indicates that the five tower heights must appear in increasing order (otherwise you would not be able to see all five towers), whereas a clue marked ‘1’ indicates that the tallest tower (the one marked 5) must come first.
			
		Since this assignment is an exercise to become familiar with functional programming, we are going to give appropriate auxiliary functions which will decompose the problem into smaller pieces.  You should be able to compose these short functions to accomplish the ultimate goal of verifying whether a completely-filled grid is a solution based on the clues around the edges.  We will only be verifying the "easy" games which don't leave out any of the clues around the grid edges.
			
		Use the List combinators, pipelining, etc. when possible and whenever it improves the readability of the solution.  All but the last function are directly code-able with the List combinators and no `rec`.
		As in Part I feel free to write your own helper functions if needed.

		If you are unsure on what the requirements of the functions are, look at the test cases we provide.

		*)

		(* We can represent the tower board itself as a list of lists of ints.  
			See `tower_board_example` in `tests.ml` for an example.

		The first task is to determine if a list of lists of integers is "square", i.e. each list is the same length and there are the same number of lists as there are elements in any of the lists. If it is, return Ok(the dimension of the array).  If not, return Error "not square". *)
		let square_size (grid: int list list) : (int, string) result =
			let size = List.length grid in
			let square = List.for_all ~f:(fun row -> List.length row = size) grid in
			if square then Ok(size)
			else Error("not square")
		
		(* Given a list of integers of length n, determine if the list has exactly one occurrence
			of each number in 1 .. n in it. Return false if not *)	
		let elements_span_range (l : int list) : bool = 
			let sorted = merge_sort l in
			let list2 = List.init (List.length l) ~f:(fun i -> i+1) in
			match List.for_all2 ~f:(fun a b -> a = b) sorted list2 with
				| Ok true -> true (* Lists have equal lengths and all elements are equal *)
				| Ok false -> false (* Lists have equal lengths but some elements are not equal *)
				| Unequal_lengths -> false (* Lists have unequal lengths *)
		
		(* Check to see if a towers grid is well-formed, namely
			1) it is square as per above,
			2) it is at least 1x1 in size (no 0x0 degenerates are allowed)
			2) each row and column spans the range as per above *)
		let well_formed_grid (grid : int list list) : bool = 
			let n = List.length grid in
			let columns = List.init n ~f:(fun i -> List.map ~f:(fun row -> 
				match List.nth row i with
				| Some value -> value  (* Use the value if it exists *)
				| None -> 0)  (* Provide a default value, e.g., 0, when the index is out of bounds *)
			grid) in
			if n = 0 then false
			else if Result.is_error(square_size grid) then false
			else if not (List.for_all ~f:(fun row -> elements_span_range row) grid) then false
			else if not (List.for_all ~f:(fun col -> elements_span_range col) columns) then false
			else true
		
		(* The next six auxiliary functions should only be called on well-formed grids, or rows from well-formed
			grids, and so you don't need to deal with ill-formed cases there such as 0x0 or non-spanning grid rows.  *)	
		
		(* The lowest level of the validity check for towers requires finding the number of
		local maxima going down a given row of the grid (i.e. down a list of integers).  Define
		a function local_max_count to find that value.  *)
		
		let local_max_count (row : int list) : int =  
			let rec count_local_max (lst : int list) (prev : int) (count : int) : int =
				match lst with
				| [] -> count
				| [x] -> if x > prev then count+1 else count
				| x :: xs ->
					if x > prev then count_local_max xs x (count+1)
					else count_local_max xs prev count
			in
			count_local_max row 0 0
		
		(* Now we need to apply the above function to each row/column around the grid.  There
		are many reasonable ways to solve that task, but here we ask you to first write a
		function verify_left_clues to check the "left side grid" clues only are correct.  For
		this function the `edge_clues` list is only the left-side clues. *)
		let verify_left_clues (grid : int list list) (edge_clues : int list) : bool =
			let local_max_counts = List.map ~f:(local_max_count) grid in
  		match List.for_all2 ~f:(fun x1 x2 -> x1 = x2) local_max_counts edge_clues with
			| Ok true -> true (* Lists have equal lengths and all elements are equal *)
			| Ok false -> false (* Lists have equal lengths but some elements are not equal *)
			| Unequal_lengths -> false (* Lists have unequal lengths *)
		
		(* In order to check the clues on all four edges, we will rotate the grid counter-clockwise and call the above function at each rotation. 
		There many ways we can rotate our grid.  Here we suggest using a combination of transpose (like for a matrix: flip rows and columns), and reflect.  Note you can assume the grid is well-formed.  *)	
		let transpose (grid : int list list) : int list list =
			let columns = List.init (List.length grid) ~f:(fun i -> List.map ~f:(fun row -> 
				match List.nth row i with
				| Some value -> value  (* Use the value if it exists *)
				| None -> 0)  (* Provide a default value, e.g., 0, when the index is out of bounds *)
				grid) in
			columns
			
		let reflect_vertical_axis (grid : int list list) : int list list =
			List.map ~f:(List.rev) grid
		
		(* Now it should not be difficult to define a function to rotate the grid counterclockwise *)
		let rotate_ccw (grid : int list list) : int list list = 
			transpose (reflect_vertical_axis grid)
		
		(* Finally, write a function verify_towers_solution which given a grid and the clues all around the grid, verifies that the solution is correct with respect to the clues: the grid is well-formed as per above, and the clues are all satisfied by the solution.  
		The clues are a list of lists, the first element of the list is the edge_clues for the original board orientation, and the subsequent lists correspond to clues for successive rotations of the grid. 
		If either the grid is ill-formed or the clues are not all satisfied, return false.  *)
		let verify_towers_solution  (grid : int list list) (four_edge_clues : int list list) : bool = 
			let rec check (g : int list list) (clues : int list list) : bool =
				match clues with
				| [] -> true
				| x :: xs -> 
					if verify_left_clues g x then check (rotate_ccw g) xs
					else false
			in
			if not (well_formed_grid grid) then false 
			else check grid four_edge_clues
		
	end (* module Section3 *)

	include Section3

end (* module Part2 *)

include Part2