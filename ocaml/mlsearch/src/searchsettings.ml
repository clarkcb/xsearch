open Core.Std
open Common

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
  text_file_encoding : string;
  unique_lines : bool;
  verbose : bool
}

let default_settings = {
  archives_only = false;
  colorize = true;
  debug = false;
  exclude_hidden = true;
  first_match = false;
  in_archive_extensions = [];
  in_archive_file_patterns = [];
  in_dir_patterns = [];
  in_extensions = [];
  in_file_patterns = [];
  in_file_types = [];
  in_lines_after_patterns = [];
  in_lines_before_patterns = [];
  lines_after = 0;
  lines_aftertopatterns = [];
  lines_afteruntilpatterns = [];
  lines_before = 0;
  list_dirs = false;
  list_files = false;
  list_lines = false;
  max_line_length = 200;
  multi_line_search = false;
  out_archive_extensions = [];
  out_archive_file_patterns = [];
  out_dir_patterns = [];
  out_extensions = [];
  out_file_patterns = [];
  out_file_types = [];
  out_lines_after_patterns = [];
  out_lines_before_patterns = [];
  print_results = true;
  print_usage = false;
  print_version = false;
  recursive = true;
  search_archive = false;
  search_patterns = [];
  startpath = "";
  text_file_encoding = "UTF-8";
  unique_lines = false;
  verbose = false
};;

let add_extensions (ext_string : string) (extensions : string list) = 
  let exts = String.split ext_string ~on:(char_of_int 44) in
  List.append extensions exts

let add_file_types (ft_string : string) (file_types : string list) = 
  let fts = String.split ft_string ~on:(char_of_int 44) in
  List.append file_types fts

let set_archives_only (ss : SearchSettings.t) (archives_only: bool) (ss : SearchSettings.t) =
  let search_archive = if archives_only then archives_only else ss.search_archive
  { ss with archives_only=archives_only; search_archive=search_archive }

let set_debug (ss : SearchSettings.t) (debug: bool) (ss : SearchSettings.t) =
  let verbose = if debug then debug else ss.verbose
  { ss with debug=debug; verbose=verbose }

let to_string s = 
  String.concat [
    sprintf "{archives_only=%b" s.archives_only;
    sprintf "; colorize=%b" s.colorize;
    sprintf "; debug=%b" s.debug;
    sprintf "; exclude_hidden=%b" s.exclude_hidden;
    sprintf "; first_match=%b" s.first_match;
    sprintf "; in_archive_extensions=%s" (list_to_string s.in_archive_extensions);
    sprintf "; in_archive_file_patterns=%s" (regexp_list_to_string s.in_archive_file_patterns);
    sprintf "; in_dir_patterns=%s" (regexp_list_to_string s.in_dir_patterns);
    sprintf "; in_extensions=%s" (list_to_string s.in_extensions);
    sprintf "; in_file_patterns=%s" (regexp_list_to_string s.in_file_patterns);
    sprintf "; in_file_types=%s" (list_to_string s.in_file_types);
    sprintf "; in_lines_after_patterns=%s" (regexp_list_to_string s.in_lines_after_patterns);
    sprintf "; in_lines_before_patterns=%s" (regexp_list_to_string s.in_lines_before_patterns);
    sprintf "; lines_after=%d" s.lines_after;
    sprintf "; lines_aftertopatterns=%s" (regexp_list_to_string s.lines_aftertopatterns);
    sprintf "; lines_afteruntilpatterns=%s" (regexp_list_to_string s.lines_afteruntilpatterns);
    sprintf "; lines_before=%d" s.lines_before;
    sprintf "; list_dirs=%b" s.list_dirs;
    sprintf "; list_files=%b" s.list_files;
    sprintf "; list_lines=%b" s.list_lines;
    sprintf "; max_line_length=%d" s.max_line_length;
    sprintf "; multi_line_search=%b" s.multi_line_search;
    sprintf "; out_archive_extensions=%s" (list_to_string s.out_archive_extensions);
    sprintf "; out_archive_file_patterns=%s" (regexp_list_to_string s.out_archive_file_patterns);
    sprintf "; out_dir_patterns=%s" (regexp_list_to_string s.out_dir_patterns);
    sprintf "; out_extensions=%s" (list_to_string s.out_extensions);
    sprintf "; out_file_patterns=%s" (regexp_list_to_string s.out_file_patterns);
    sprintf "; out_file_types=%s" (list_to_string s.out_file_types);
    sprintf "; out_lines_after_patterns=%s" (regexp_list_to_string s.out_lines_after_patterns);
    sprintf "; out_lines_before_patterns=%s" (regexp_list_to_string s.out_lines_before_patterns);
    sprintf "; print_results=%b" s.print_results;
    sprintf "; print_version=%b" s.print_version;
    sprintf "; recursive=%b" s.recursive;
    sprintf "; search_archive=%b" s.search_archive;
    sprintf "; search_patterns=%s" (regexp_list_to_string s.search_patterns);
    sprintf "; startpath=\"%s\"" s.startpath;
    sprintf "; text_file_encoding=\"%s\"" s.text_file_encoding;
    sprintf "; unique_lines=%b" s.unique_lines;
    sprintf "; verbose=%b}" s.verbose];;
