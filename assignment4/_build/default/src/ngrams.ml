(*
  Part II:

  Implement an executable `ngrams.exe` which can use n-gram models in several ways.   It should expect to be called with the following arguments, with bracketed ones optional:

    $ ngrams.exe N CORPUS-FILE [--sample SAMPLE-LENGTH [INITIAL-WORDS...]] [--most-frequent N-MOST-FREQUENT]

  
  Functionality should be as follows:

  - Load the file specified by `CORPUS-FILE` and split its contents into a sequence of strings based on whitespace. Treat newlines and spaces, etc. equally.

  - Sanitize each of the strings in this sequence by sending to lowercase and removing non-alphanumeric characters. Say a "sanitized" word is lowercase and uses only alphanumeric characters.

  - Initialize an n-gram distribution using `N` and the sanitized sequence of words. The `N` is the length of the n-grams used when building the distribution, so N = 3 means two items of context because the last item is used for sampling, and the first two are used for context of the sampled element.

  
  If the option `--sample SAMPLE-LENGTH` is provided:

    To stdout, output a sequence of `SAMPLE-LENGTH` words randomly sampled from the n-gram model.  Print them out separated by single spaces. 
    
    To begin the sequence, use the `INITIAL-WORDS` arguments provided after `--sample` to seed the sequence, or if none are provided, choose a random starting n-gram to begin. You may assume that the words provided as `INITIAL-WORDS` are already sanitized, and that there are at least `N - 1` of them.

  If the option `--most-frequent N-MOST-FREQUENT` is provided:
  
    To stdout, output a sorted sexp-formatted list of length `N-MOST-FREQUENT` containing information about the most common n-grams seen in the `CORPUS-FILE`, like so:

      (((ngram(hello world goodbye))(frequency 5))...)

    Where the ["hello"; "world"; "goodbye"] n-gram showed up 5 times, and "..." is for the remaining, less-frequent n-grams. In this example, `N` = 3.

    Higher frequency n-grams should come first, and frequency ties should be broken by n-gram alphabetical order. 

  You may assume that only one of `--sample` or `--most-frequent` will be supplied at a time, and that at least one will be given.

  To parse command line arguments, we recommend looking into the `Core.Command` module.  See Real World OCaml Chapter 14 https://dev.realworldocaml.org/command-line-parsing.html for some examples of Core.Command in action.
  If these feels cumbersome, you can parse them manually, which is not a bad option for an executable of this scale.

  We will reveal only one test for each option to help you get the right output format, but you are expected to thoroughly test your own code. Testing is an important part of software development.
*)

open Core
open Lib

module Ngram = MakeDistribution (String) (Random)

let sanitize_word word =
  String.lowercase (String.filter word ~f:Char.is_alphanum)

let split_string_into_words text =
  String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ] text |> 
  List.filter ~f:(fun x -> not (String.is_empty (String.filter x ~f:Char.is_alphanum)))

let update_entry (ngram_str_with_elem : string list) (acc_elem : (string list * int) list) =
  let updated_list =
    let found, updated_acc = List.fold_left acc_elem ~init:(false, []) ~f:(fun (found, acc) (ngram_str', freq) ->
      if List.compare String.compare ngram_str' ngram_str_with_elem = 0 then
        (true, (ngram_str', freq + 1) :: acc)
      else
        (found, (ngram_str', freq) :: acc)
    ) in
    if found then updated_acc
    else (ngram_str_with_elem, 1) :: updated_acc
  in
  updated_list

let compare_ngram_freq (ngram1, freq1) (ngram2, freq2) =
  let freq_compare = Int.compare freq2 freq1 in
  if freq_compare <> 0 then freq_compare
  else List.compare String.compare ngram1 ngram2

type result_item = {
  ngram: string list;
  frequency: int;
} and result = result_item list [@@deriving sexp] 

(* Function to find the most frequent n-grams *)
let find_most_frequent_ngrams (distribution : Ngram.t) = 
  let ngram_freq_list =
    List.fold_left (Ngram.to_list distribution) ~init:[] ~f:(fun acc (ngram, bag) ->
      let frequencies =
        match bag with
        | Some v ->
          Bag.fold v ~init:acc ~f:(fun acc_elem bag_elem ->
            let new_list = List.append ngram [bag_elem] in
            update_entry new_list acc_elem
          )
        | None -> acc
      in
      frequencies)
  in
  let sorted_ngrams = List.sort ~compare:compare_ngram_freq ngram_freq_list in

  let result_list =
    List.map sorted_ngrams ~f:(fun (ngram, frequency) ->
      { ngram = ngram; frequency = frequency })
  in
  let result = sexp_of_result result_list in
  Sexp.to_string result

let () =
  Command.basic
    ~summary:"ngrams.exe - Generate n-grams from a corpus"
    (let%map_open.Command n =
      anon ("N" %: int)
    and corpus_file =
      anon ("CORPUS-FILE" %: string)
    and sample_length =
      flag "--sample" (optional_with_default 0 int) ~doc:"SAMPLE LENGTH"
    and initial_words =
      anon (maybe (sequence ("sample" %: string)))
    and n_most_frequent =
      flag "--most-frequent" (optional_with_default 0 int) ~doc:"N-MOST-FREQUENT" in
    fun () ->
      let corpus_text = In_channel.read_all corpus_file in
      let words = List.map ~f:sanitize_word (split_string_into_words corpus_text) in

      let distribution = Ngram.make_distribution words ~n in
      let initial_words = Option.value initial_words ~default:[] in

      if not (List.is_empty initial_words) then  
        let sampled_sequence = Ngram.sample_random_sequence distribution initial_words ~n ~k:sample_length in
        Stdio.print_endline (String.concat ~sep:" " sampled_sequence);

      else if n_most_frequent > 0 then
        let frequent_ngrams = find_most_frequent_ngrams distribution in
        Stdio.print_endline frequent_ngrams;
      else
        printf "No operation specified (use --sample or --most-frequent)\n")
  |> Command_unix.run
