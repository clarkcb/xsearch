open Core.Std

type searchOption = { long : string; short : string; desc : string }

val get_searchoptions : searchOption list

val get_usage : searchOption list -> string

val settings_from_args : searchOption list -> string list -> (Searchsettings.t, string) Result.t
