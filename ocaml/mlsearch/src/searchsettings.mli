open Core.Std
(* module Regex = Re2.Regex *)

type t = {
  archives_only : bool;
  colorize : bool;
  debug : bool;
  exclude_hidden : bool;
  first_match : bool;
  in_archive_extensions : string list;
  in_archive_file_patterns : Re2.Regex.t list;
  in_dir_patterns : Re2.Regex.t list;
  in_extensions : string list;
  in_file_patterns : Re2.Regex.t list;
  in_file_types : string list;
  in_lines_after_patterns : Re2.Regex.t list;
  in_lines_before_patterns : Re2.Regex.t list;
  lines_after : int;
  lines_aftertopatterns : Re2.Regex.t list;
  lines_afteruntilpatterns : Re2.Regex.t list;
  lines_before : int;
  list_dirs : bool;
  list_files : bool;
  list_lines : bool;
  max_line_length : int;
  multi_line_search : bool;
  out_archive_extensions : string list;
  out_archive_file_patterns : Re2.Regex.t list;
  out_dir_patterns : Re2.Regex.t list;
  out_extensions : string list;
  out_file_patterns : Re2.Regex.t list;
  out_file_types : string list;
  out_lines_after_patterns : Re2.Regex.t list;
  out_lines_before_patterns : Re2.Regex.t list;
  print_results : bool;
  print_usage : bool;
  print_version : bool;
  recursive : bool;
  search_archive : bool;
  search_patterns : Re2.Regex.t list;
  startpath : string;
  unique_lines : bool;
  verbose : bool
}

val default_settings : t

val add_extensions : string -> string list -> string list

val add_file_types : string -> string list -> string list

val to_string : t -> string
