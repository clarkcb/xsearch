namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Xml.Linq

module Utils = 
    let PrintElapsed (name : string) (ts : TimeSpan) =
        let elapsedTime =
            String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10)
        printfn "Elapsed time for %s: %s" name elapsedTime

    let PrintNames (names : string list) =
        for name in names do
            printfn "Name: %s" name

;;
