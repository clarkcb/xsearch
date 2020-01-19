namespace FsSearchTests

open NUnit.Framework
open FsSearch

[<TestFixture>]
type SearchSettingTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetNewSearchSettings_NoModifications_HasDefaultValues () =
        let settings = SearchSettings.DefaultSettings
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
        Assert.IsFalse(settings.PrintResults)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.SearchArchives)
        Assert.IsFalse(settings.UniqueLines)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SearchSettings_AddExtensions_HasExtensions () =
        let settings = SearchSettings.DefaultSettings
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "cs" settings.InExtensions }
        Assert.AreEqual(settings.InExtensions.Length, 1)
        Assert.AreEqual(settings.InExtensions.Head, ".cs")
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "java,scala" settings.InExtensions }
        Assert.AreEqual(settings.InExtensions.Length, 3)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.SearchSettings_AddPatterns_HasPatterns () =
        let settings = SearchSettings.DefaultSettings
        let settings = { settings with SearchPatterns = SearchSettings.AddPattern "Search" settings.SearchPatterns }
        Assert.AreEqual(settings.SearchPatterns.Length, 1)
        Assert.AreEqual(settings.SearchPatterns.Head.ToString(), "Search")
        ()

    [<Test>]
    member this.SearchSettings_SetArchivesOnly_HasSearchArchives () =
        let settings = SearchSettings.SetArchivesOnly true SearchSettings.DefaultSettings 
        Assert.IsTrue(settings.ArchivesOnly)
        Assert.IsTrue(settings.SearchArchives)
        ()

    [<Test>]
    member this.SearchSettings_SetDebug_HasVerbose () =
        let settings = SearchSettings.SetDebug true SearchSettings.DefaultSettings 
        Assert.IsTrue(settings.Debug)
        Assert.IsTrue(settings.Verbose)
        ()
