namespace FsSearch

open System
open System.Collections.Generic
open System.Text

module Utils = 
    let ExtensionSet (s : string) =
        let exts =
            s.Split()
            |> Array.map (fun x -> "." + x)

        let extensionSet = 
            Array.fold
                (fun (acc: Set<string>) (ext : string) -> Set.add ext acc)
                Set.empty
                exts

        extensionSet

    let PrintElapsed (name : string) (ts : TimeSpan) =
        let elapsedTime =
            String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10)
        printfn "Elapsed time for %s: %s" name elapsedTime
;;
