namespace FsSearchTests

open NUnit.Framework
open FsSearchLib

[<TestFixture>]
type SearchSettingTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetNewSearchSettings_NoModifications_HasDefaultValues () =
        let settings = SearchSettings.DefaultSettings
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsTrue(settings.Colorize)
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
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.AreEqual(".cs", settings.InExtensions.Head)
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "java,scala" settings.InExtensions }
        Assert.AreEqual(3, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.SearchSettings_AddPatterns_HasPatterns () =
        let settings = SearchSettings.DefaultSettings
        let settings = { settings with SearchPatterns = SearchSettings.AddPattern "Search" settings.SearchPatterns }
        Assert.AreEqual(1, settings.SearchPatterns.Length)
        Assert.AreEqual("Search", settings.SearchPatterns.Head.ToString())
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
