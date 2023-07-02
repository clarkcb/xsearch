namespace FsSearchTests

open System.IO
open System.Reflection

module EmbeddedTestResource = 

    let GetResourceFileContents (namespaceAndFileName : string) : string = 
        let contents =
            try
                use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(namespaceAndFileName)
                use reader = new StreamReader(stream)
                reader.ReadToEnd()
            with
            | :? IOException as e -> printfn $"%s{e.Message}"; ""
        contents
