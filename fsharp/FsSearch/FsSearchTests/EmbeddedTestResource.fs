namespace FsSearchTests

open System.IO
open System.Reflection
open FsSearch

type EmbeddedTestResource =
    static member GetResourceFileContents (namespaceAndFileName : string) : string =
        let contents =
            try
                //use sr = new StreamReader (namespaceAndFileName)
                let assem = Assembly.GetExecutingAssembly()
//                assem.GetTypes()
//                let assem = typeof<EmbeddedTestResource>.GetType().Assembly
                use stream = assem.GetManifestResourceStream(namespaceAndFileName)
                use sr = new StreamReader (stream)
                sr.ReadToEnd()
            with
//            | :? IOException as e -> raise (SearchException("Failed to read Embedded Resource " + namespaceAndFileName)) 
            | :? IOException as e -> raise e 
        contents


//module TestResource =
//    let GetTestResource () =
////        use stream = typeof<EmbeddedTestResource>.GetType
//        let assembly = Assembly.GetExecutingAssembly()
//        let functions =
//            match assembly.GetTypes() |> Array.tryFind (fun t -> t.Name = "EmbeddedTestResource") with
//            | Some moduleType -> moduleType.GetMethods()
//            | None -> [||]
