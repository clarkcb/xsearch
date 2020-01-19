namespace FsSearchTests

open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open FsSearch

[<TestFixture>]
type SearchOptionsTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SettingsFromArgs_NoArgs_HasDefaultValues () =
        let args : string[] = [||]
        let settings, err = SearchOptions.SettingsFromArgs(args)
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsFalse(settings.Debug)
        Assert.IsTrue(settings.ExcludeHidden)
        Assert.IsFalse(settings.FirstMatch)
        Assert.AreEqual(settings.LinesAfter, 0)
        Assert.AreEqual(settings.LinesBefore, 0)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.IsFalse(settings.ListLines)
        Assert.AreEqual(settings.MaxLineLength, 150)
        Assert.IsFalse(settings.MultiLineSearch)
        Assert.IsTrue(settings.PrintResults)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.SearchArchives)
        Assert.AreEqual(settings.StartPath, "")
        Assert.AreEqual(settings.TextFileEncoding, "utf-8")
        Assert.IsFalse(settings.UniqueLines)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "." |]
        let settings, err = SearchOptions.SettingsFromArgs(args)
        let startFile = FileInfo(".")
        //Assert.AreEqual(settings.StartPath, startFile.FullName)
        Assert.AreEqual(settings.InExtensions.Length, 1)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
        Assert.AreEqual(settings.SearchPatterns.Length, 1)
        Assert.AreEqual(settings.SearchPatterns.Head.ToString(), "Search")
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsSearchException () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "."; "-Q" |]
        let settings, err = SearchOptions.SettingsFromArgs(args)
        Assert.AreEqual(err, "Invalid option: Q")
        ()
