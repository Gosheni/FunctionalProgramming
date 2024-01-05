(*

Put all helper functions needed for keywordcount here. This should contain no functions
that help only your implementation of simpledict. Helper functions should be tested, and
they go in this library so that they are testable. If they are only in keywordcount.ml,
which compiles to an executable, then they are not accessible from a test file.

Feel free to remove `placeholder` when you add your actual functions.
   
*)

type keyword_count = {
  keyword : string;
  count : int;
} and 
stat = keyword_count list
[@@deriving sexp]

val remove_non_characters : string -> string

val compare_keyword_counts : keyword_count -> keyword_count -> int

val convert : int Simpledict.t -> stat -> stat

val edit : string list -> int Simpledict.t -> int Simpledict.t

val count_keywords_in_directory : string -> stat

val remove_comments : int -> int Simpledict.t -> string -> int * int Simpledict.t

val count_keywords_in_file : string -> int Simpledict.t -> int Simpledict.t

val remove_literal_strings : string -> string