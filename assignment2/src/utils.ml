(*
   
See utils.mli. This file contains implementations for your utils.mli functions.

*)

open Core

type keyword_count = {
  keyword : string;
  count : int;
}  and
stat = keyword_count list
[@@deriving sexp]

let keywords = ["and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with"]

let keyword_set = String.Set.of_list keywords

let remove_non_characters (str : string) : string =
  let is_valid_char (c : char) : bool = 
    (Char.(>=) c 'a' && Char.(<=) c 'z') || (Char.(>=) c 'A' && Char.(<=) c 'Z') || 
    (Char.(>=) c '0' && Char.(<=) c '9') || Char.(=) c '_' 
  in
  String.map ~f:(fun c -> if is_valid_char c then c else ' ') str 

let compare_keyword_counts (kc1 : keyword_count) (kc2 : keyword_count) : int =
  if kc1.count = kc2.count then
    String.compare kc1.keyword kc2.keyword
  else 
    compare kc2.count kc1.count

let rec convert (dt : int Simpledict.t) (ls : stat) : stat =
  match dt with
  | Leaf -> ls
  | Branch { item; left; right } ->
    let acc_right = convert right ls in
    let acc_current = { keyword = item.key; count = item.value } :: acc_right in
    convert left acc_current

let rec remove_literal_strings (s : string) : string =
  match String.index s '"' with
  | None -> s
  | Some start ->
    match String.index_from s (start + 1) '"' with
    | None -> s
    | Some end_ ->
      let prefix = String.sub s ~pos:0 ~len:start in
      let suffix = remove_literal_strings (String.sub s ~pos:(end_ + 1) ~len:(String.length s - end_ - 1)) in
      prefix ^ suffix

let rec edit (words : string list) (dt : int Simpledict.t) : int Simpledict.t =
  let rec edit2 (words2 : string list) (dt2 : int Simpledict.t) : int Simpledict.t =
    match words2 with
    | [] -> dt2
    | hd2 :: tl2 ->
    if Set.mem keyword_set hd2 then
      match Simpledict.lookup dt2 ~key:hd2 with
      | Some v -> edit2 tl2 (Simpledict.insert dt2 ~key:hd2 ~value:(v + 1))
      | None -> edit2 tl2 (Simpledict.insert dt2 ~key:hd2 ~value:1)
    else
      edit2 tl2 dt2
  in
  match words with
  | [] -> dt
  | hd :: tail ->
    let split = String.split (remove_non_characters hd) ~on:' ' in
    edit tail (edit2 split dt)

let remove_comments (count : int) (keyword_count : int Simpledict.t) (s : string) : int * int Simpledict.t =
  let rec process_tokens (tokens : string list) (comment_depth : int) (kw_count : int Simpledict.t) =
    match tokens with
    | [] -> (comment_depth, kw_count)
    | token :: rest ->
      if comment_depth = 0 then
        match String.substr_index ~pattern:"(*" token with
        | None -> process_tokens rest comment_depth (edit [token] kw_count)
        | Some start_idx ->
          let pre_comment = String.sub token ~pos:0 ~len:start_idx in
          let post_comment = String.sub token ~pos:(start_idx + 2) ~len:(String.length token - start_idx - 2) in
          process_tokens (post_comment :: rest) (comment_depth + 1) (edit [pre_comment] kw_count)
      else
        match String.substr_index ~pattern:"(*" token, String.substr_index ~pattern:"*)" token with
        | None, None -> process_tokens rest comment_depth kw_count
        | Some st_idx, None ->
          let post_comment = String.sub token ~pos:(st_idx + 2) ~len:(String.length token - st_idx - 2) in
          process_tokens (post_comment :: rest) (comment_depth + 1) kw_count
        | None, Some end_idx ->
          let post_comment = String.sub token ~pos:(end_idx + 2) ~len:(String.length token - end_idx - 2) in
          process_tokens (post_comment :: rest) (comment_depth - 1) kw_count
        | Some st_idx, Some end_idx ->
          if end_idx < st_idx then
            let post_comment = String.sub token ~pos:(st_idx + 2) ~len:(String.length token - st_idx - 2) in
            if comment_depth = 1 then
              let mid_comment = String.sub token ~pos:(end_idx + 2) ~len:(st_idx - end_idx - 2) in
              process_tokens (post_comment :: rest) comment_depth (edit [mid_comment] kw_count)
            else
              process_tokens (post_comment :: rest) comment_depth kw_count
          else
            let post_comment = (String.sub token ~pos:(end_idx + 2) ~len:(String.length token - end_idx - 2)) in
            process_tokens (post_comment :: rest) comment_depth kw_count     
  in
  process_tokens (String.split s ~on:' ') count keyword_count
    

let count_keywords_in_file (filename : string) (keyword_counts1 : int Simpledict.t) : int Simpledict.t =
  let cmd = Printf.sprintf "cat %s" filename in
  let ic = Core_unix.open_process_in cmd in
  let rec count_lines (count : int) (keyword_counts2 : int Simpledict.t) : int Simpledict.t =
    match In_channel.input_line ic with
    | None ->
      In_channel.close ic;
      keyword_counts2
    | Some line ->
      let c, updated = remove_comments count keyword_counts2 (remove_literal_strings line) in
      count_lines c updated
  in
  count_lines 0 keyword_counts1

let count_keywords_in_directory (dir : string) : stat =
  let keyword_counts = Simpledict.of_list [] in
  let rec traverse_dir (path : string) (keyword_counts : int Simpledict.t) : int Simpledict.t =
    let entries = Sys_unix.ls_dir path in
    let rec process_entries (acc : int Simpledict.t) (entries : string list) : int Simpledict.t =
      match entries with
      | [] -> acc
      | entry :: rest ->
        let entry_path = Filename.concat path entry in
        match Sys_unix.is_directory entry_path with
        | `Yes ->
          let updated_with_nested = traverse_dir entry_path acc in
          process_entries updated_with_nested rest
        | _ ->
          if Filename.check_suffix entry ".ml" || Filename.check_suffix entry ".mli" then
            let updated = count_keywords_in_file entry_path acc in
            process_entries updated rest
          else
            process_entries acc rest
    in
    process_entries keyword_counts entries
  in
  let updated_keyword_counts = traverse_dir dir keyword_counts in
  List.sort ~compare:(compare_keyword_counts) (convert updated_keyword_counts [])
