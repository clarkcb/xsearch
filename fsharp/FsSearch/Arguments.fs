namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Arguments =   

    let SettingsFromArgs (args : string[]) =
        let settings = new SearchSettings()

        let actionMap =
            Map.empty.
                Add("d", settings.AddInDirPattern).
                Add("D", settings.AddOutDirPattern).
                Add("f", settings.AddInFilePattern).
                Add("F", settings.AddOutFilePattern).
                Add("s", settings.AddSearchPattern).
                Add("x", settings.AddInExtension).
                Add("X", settings.AddOutExtension)

        let flagMap =
            Map.empty.
                Add("listfiles", settings.SetListFiles).
                Add("t", settings.SetDoTiming).
                Add("v", settings.SetVerbose)

        let argRegex = new Regex("^(?:-{1,2}|\/)(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec loop (argList:string list) =
            match argList with
            | [] -> ()
            | head::tail ->
                match head with
                | IsOption option ->
                    if (actionMap.ContainsKey(option)) then
                        match tail with
                        | [] -> printf "Error: missing value for %s arg" head
                        | aHead::aTail -> 
                            actionMap.[option] aHead
                            loop aTail
                    elif (flagMap.ContainsKey(option)) then
                        flagMap.[option] true
                        loop tail
                    else
                        printfn "Error: invalid arg: %s" head
                | _ -> settings.StartPath <- head
        loop (Array.toList args)
        settings
