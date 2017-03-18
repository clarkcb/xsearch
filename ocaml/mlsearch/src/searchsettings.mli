open Core.Std
(* module Regex = Re2.Regex *)

type t = {
  archivesonly : bool;
  debug : bool;
  excludehidden : bool;
  firstmatch : bool;
  in_archiveextensions : string list;
  in_archivefilepatterns : Re2.Regex.t list;
  in_dirpatterns : Re2.Regex.t list;
  in_extensions : string list;
  in_filepatterns : Re2.Regex.t list;
  in_filetypes : string list;
  in_linesafterpatterns : Re2.Regex.t list;
  in_linesbeforepatterns : Re2.Regex.t list;
  linesafter : int;
  linesaftertopatterns : Re2.Regex.t list;
  linesafteruntilpatterns : Re2.Regex.t list;
  linesbefore : int;
  listdirs : bool;
  listfiles : bool;
  listlines : bool;
  maxlinelength : int;
  multilinesearch : bool;
  out_archiveextensions : string list;
  out_archivefilepatterns : Re2.Regex.t list;
  out_dirpatterns : Re2.Regex.t list;
  out_extensions : string list;
  out_filepatterns : Re2.Regex.t list;
  out_filetypes : string list;
  out_linesafterpatterns : Re2.Regex.t list;
  out_linesbeforepatterns : Re2.Regex.t list;
  printresults : bool;
  printusage : bool;
  printversion : bool;
  recursive : bool;
  searcharchives : bool;
  searchpatterns : Re2.Regex.t list;
  startpath : string;
  uniquelines : bool;
  verbose : bool
}

val default_settings : t

val add_extensions : string -> string list -> string list

val add_filetypes : string -> string list -> string list

val to_string : t -> string
