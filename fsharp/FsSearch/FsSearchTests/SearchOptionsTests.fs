namespace FsSearchTests

open NUnit.Framework
open FsSearchLib

[<TestFixture>]
type SearchOptionsTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SettingsFromArgs_NoArgs_HasDefaultValues () =
        let args : string[] = [||]
        let settings, _ = SearchOptions.SettingsFromArgs(args)
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsTrue(settings.Colorize)
        Assert.IsFalse(settings.Debug)
        Assert.IsTrue(settings.ExcludeHidden)
        Assert.IsFalse(settings.FirstMatch)
        Assert.AreEqual(0, settings.LinesAfter)
        Assert.AreEqual(0, settings.LinesBefore)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.IsFalse(settings.ListLines)
        Assert.AreEqual(150, settings.MaxLineLength)
        Assert.IsFalse(settings.MultiLineSearch)
        Assert.IsEmpty(settings.Paths)
        Assert.IsTrue(settings.PrintResults)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.SearchArchives)
        Assert.AreEqual("utf-8", settings.TextFileEncoding)
        Assert.IsFalse(settings.UniqueLines)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "." |]
        let settings, _ = SearchOptions.SettingsFromArgs(args)
        //let startFile = FileInfo(".")
        //Assert.AreEqual(settings.StartPath, startFile.FullName)
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
        Assert.AreEqual(1, settings.SearchPatterns.Length)
        Assert.AreEqual("Search", settings.SearchPatterns.Head.ToString())
        Assert.AreEqual(1, settings.Paths.Length)
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsSearchException () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "."; "-Q" |]
        let _, err = SearchOptions.SettingsFromArgs(args)
        Assert.AreEqual("Invalid option: Q", err)
        ()
