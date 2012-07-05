namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

type SearchOption(shortArg : string, longArg : string, description : string) =
    let _shortArg = shortArg
    let _longArg = longArg
    let _description = description

    // read-only member properties
    member this.ShortArg = _shortArg
    member this.LongArg = _longArg
    member this.Description = _description
    member this.SortArg = 
        if _shortArg <> null && _shortArg <> "" then _shortArg
        else _longArg


//type SearchArgOption(shortArg : string, longArg : string, description : string, action : Action<string, SearchSettings>) =
type SearchArgOption(shortArg : string, longArg : string, description : string, action : Action<string, SearchSettings>) =
    inherit SearchOption(shortArg, longArg, description)
    let _action = action
    member this.Action = _action

type SearchFlagOption(shortArg : string, longArg : string, description : string, action : Func<SearchSettings, bool>) =
    inherit SearchOption(shortArg, longArg, description)
    let _action = action
    member this.Action = _action
    ;;
