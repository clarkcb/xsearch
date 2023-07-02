namespace FsSearchTests

open FsFind
open NUnit.Framework
open FsSearchLib

[<TestFixture>]
type SearchSettingTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetNewSearchSettings_NoModifications_HasDefaultValues () =
        let settings = SearchSettings()
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsTrue(settings.Colorize)
        Assert.IsFalse(settings.Debug)
        Assert.IsTrue(settings.ExcludeHidden)
        Assert.IsFalse(settings.FirstMatch)
        Assert.IsFalse(settings.IncludeArchives)
        Assert.AreEqual(0, settings.LinesAfter)
        Assert.AreEqual(0, settings.LinesBefore)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.IsFalse(settings.ListLines)
        Assert.AreEqual(None, settings.MaxLastMod)
        Assert.AreEqual(150, settings.MaxLineLength)
        Assert.AreEqual(0, settings.MaxSize)
        Assert.AreEqual(None, settings.MinLastMod)
        Assert.AreEqual(0, settings.MinSize)
        Assert.IsFalse(settings.MultiLineSearch)
        Assert.IsEmpty(settings.Paths)
        Assert.IsFalse(settings.PrintResults)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.SearchArchives)
        Assert.AreEqual(SortBy.FilePath, settings.SortBy)
        Assert.IsFalse(settings.SortCaseInsensitive)
        Assert.IsFalse(settings.SortDescending)
        Assert.IsFalse(settings.UniqueLines)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SearchSettings_AddExtensions_HasExtensions () =
        let settings = SearchSettings()
        settings.InExtensions <- settings.AddExtensions "cs" settings.InExtensions
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.AreEqual(".cs", settings.InExtensions.Head)
        settings.InExtensions <- settings.AddExtensions "java,scala" settings.InExtensions
        Assert.AreEqual(3, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.SearchSettings_AddPatterns_HasPatterns () =
        let settings = SearchSettings()
        settings.SearchPatterns <- settings.AddPattern "Search" settings.SearchPatterns
        Assert.AreEqual(1, settings.SearchPatterns.Length)
        Assert.AreEqual("Search", settings.SearchPatterns.Head.ToString())
        ()

    [<Test>]
    member this.SearchSettings_SetArchivesOnly_HasSearchArchives () =
        let settings = SearchSettings()
        settings.ArchivesOnly <- true
        Assert.IsTrue(settings.ArchivesOnly)
        Assert.IsTrue(settings.IncludeArchives)
        Assert.IsTrue(settings.SearchArchives)
        ()

    [<Test>]
    member this.SearchSettings_SetDebug_HasVerbose () =
        let settings = SearchSettings()
        settings.Debug <- true
        Assert.IsTrue(settings.Debug)
        Assert.IsTrue(settings.Verbose)
        ()
