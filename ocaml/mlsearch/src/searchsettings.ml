open Core.Std
open Common

type t = {
  archivesonly : bool;
  colorize : bool;
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
  textfileencoding : string;
  uniquelines : bool;
  verbose : bool
}

let default_settings = {
  archivesonly = false;
  colorize = true;
  debug = false;
  excludehidden = true;
  firstmatch = false;
  in_archiveextensions = [];
  in_archivefilepatterns = [];
  in_dirpatterns = [];
  in_extensions = [];
  in_filepatterns = [];
  in_filetypes = [];
  in_linesafterpatterns = [];
  in_linesbeforepatterns = [];
  linesafter = 0;
  linesaftertopatterns = [];
  linesafteruntilpatterns = [];
  linesbefore = 0;
  listdirs = false;
  listfiles = false;
  listlines = false;
  maxlinelength = 200;
  multilinesearch = false;
  out_archiveextensions = [];
  out_archivefilepatterns = [];
  out_dirpatterns = [];
  out_extensions = [];
  out_filepatterns = [];
  out_filetypes = [];
  out_linesafterpatterns = [];
  out_linesbeforepatterns = [];
  printresults = true;
  printusage = false;
  printversion = false;
  recursive = true;
  searcharchives = false;
  searchpatterns = [];
  startpath = "";
  textfileencoding = "UTF-8";
  uniquelines = false;
  verbose = false
};;

let add_extensions (ext_string : string) (extensions : string list) = 
  let exts = String.split ext_string ~on:(char_of_int 44) in
  List.append extensions exts

let add_filetypes (ft_string : string) (filetypes : string list) = 
  let fts = String.split ft_string ~on:(char_of_int 44) in
  List.append filetypes fts

let set_archivesonly (ss : SearchSettings.t) (archivesonly: bool) (ss : SearchSettings.t) =
  let searcharchives = if archivesonly then archivesonly else ss.searcharchives
  { ss with archivesonly=archivesonly; searcharchives=searcharchives }

let set_debug (ss : SearchSettings.t) (debug: bool) (ss : SearchSettings.t) =
  let verbose = if debug then debug else ss.verbose
  { ss with debug=debug; verbose=verbose }

let to_string s = 
  String.concat [
    sprintf "{archivesonly=%b" s.archivesonly;
    sprintf "; colorize=%b" s.colorize;
    sprintf "; debug=%b" s.debug;
    sprintf "; excludehidden=%b" s.excludehidden;
    sprintf "; firstmatch=%b" s.firstmatch;
    sprintf "; in_archiveextensions=%s" (list_to_string s.in_archiveextensions);
    sprintf "; in_archivefilepatterns=%s" (regexp_list_to_string s.in_archivefilepatterns);
    sprintf "; in_dirpatterns=%s" (regexp_list_to_string s.in_dirpatterns);
    sprintf "; in_extensions=%s" (list_to_string s.in_extensions);
    sprintf "; in_filepatterns=%s" (regexp_list_to_string s.in_filepatterns);
    sprintf "; in_filetypes=%s" (list_to_string s.in_filetypes);
    sprintf "; in_linesafterpatterns=%s" (regexp_list_to_string s.in_linesafterpatterns);
    sprintf "; in_linesbeforepatterns=%s" (regexp_list_to_string s.in_linesbeforepatterns);
    sprintf "; linesafter=%d" s.linesafter;
    sprintf "; linesaftertopatterns=%s" (regexp_list_to_string s.linesaftertopatterns);
    sprintf "; linesafteruntilpatterns=%s" (regexp_list_to_string s.linesafteruntilpatterns);
    sprintf "; linesbefore=%d" s.linesbefore;
    sprintf "; listdirs=%b" s.listdirs;
    sprintf "; listfiles=%b" s.listfiles;
    sprintf "; listlines=%b" s.listlines;
    sprintf "; maxlinelength=%d" s.maxlinelength;
    sprintf "; multilinesearch=%b" s.multilinesearch;
    sprintf "; out_archiveextensions=%s" (list_to_string s.out_archiveextensions);
    sprintf "; out_archivefilepatterns=%s" (regexp_list_to_string s.out_archivefilepatterns);
    sprintf "; out_dirpatterns=%s" (regexp_list_to_string s.out_dirpatterns);
    sprintf "; out_extensions=%s" (list_to_string s.out_extensions);
    sprintf "; out_filepatterns=%s" (regexp_list_to_string s.out_filepatterns);
    sprintf "; out_filetypes=%s" (list_to_string s.out_filetypes);
    sprintf "; out_linesafterpatterns=%s" (regexp_list_to_string s.out_linesafterpatterns);
    sprintf "; out_linesbeforepatterns=%s" (regexp_list_to_string s.out_linesbeforepatterns);
    sprintf "; printresults=%b" s.printresults;
    sprintf "; printversion=%b" s.printversion;
    sprintf "; recursive=%b" s.recursive;
    sprintf "; searcharchives=%b" s.searcharchives;
    sprintf "; searchpatterns=%s" (regexp_list_to_string s.searchpatterns);
    sprintf "; startpath=\"%s\"" s.startpath;
    sprintf "; textfileencoding=\"%s\"" s.textfileencoding;
    sprintf "; uniquelines=%b" s.uniquelines;
    sprintf "; verbose=%b}" s.verbose];;
