namespace FsSearchLib

open System

module Common = 
    let Log (msg : string) : unit =
        printfn $"%s{msg}"

    let PrintElapsed (name : string) (ts : TimeSpan) : unit =
        let elapsedTime =
            String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10)
        printfn $"Elapsed time for %s{name}: %s{elapsedTime}"

    let PrintNames (names : string list) : unit =
        for name in names do
            printfn $"Name: %s{name}"

    let list_to_string (lst : 'a list) : string = 
        let rec rec_list_to_string (acc : string) (lst : 'a list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (rec_list_to_string (acc + " \"" + a.ToString() + "\"") [])
            | h :: t -> (rec_list_to_string (acc + " \"" + h.ToString() + "\";") t) in
        sprintf "[%s]" (rec_list_to_string "" lst)

    let ListToString<'a> (name : string, ls : string list) : string = 
        $"%s{name} (%d{ls.Length}): %s{list_to_string ls}"

;;
