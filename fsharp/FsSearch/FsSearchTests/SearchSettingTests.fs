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
        Assert.That(settings.ArchivesOnly, Is.False)
        Assert.That(settings.Colorize)
        Assert.That(settings.Debug, Is.False)
        Assert.That(settings.FirstMatch, Is.False)
        Assert.That(settings.IncludeArchives, Is.False)
        Assert.That(settings.IncludeHidden, Is.False)
        Assert.That(settings.LinesAfter, Is.EqualTo(0))
        Assert.That(settings.LinesBefore, Is.EqualTo(0))
        Assert.That(settings.PrintDirs, Is.False)
        Assert.That(settings.PrintFiles, Is.False)
        Assert.That(settings.PrintLines, Is.False)
        Assert.That(settings.MaxLastMod, Is.EqualTo(None))
        Assert.That(settings.MaxLineLength, Is.EqualTo(150))
        Assert.That(settings.MaxSize, Is.EqualTo(0))
        Assert.That(settings.MinLastMod, Is.EqualTo(None))
        Assert.That(settings.MinSize, Is.EqualTo(0))
        Assert.That(settings.MultiLineSearch, Is.False)
        Assert.That(settings.Paths.IsEmpty)
        Assert.That(settings.PrintResults, Is.False)
        Assert.That(settings.PrintUsage, Is.False)
        Assert.That(settings.PrintVersion, Is.False)
        Assert.That(settings.Recursive)
        Assert.That(settings.SearchArchives, Is.False)
        Assert.That(settings.SortBy, Is.EqualTo(SortBy.FilePath))
        Assert.That(settings.SortCaseInsensitive, Is.False)
        Assert.That(settings.SortDescending, Is.False)
        Assert.That(settings.UniqueLines, Is.False)
        Assert.That(settings.Verbose, Is.False)
        ()

    [<Test>]
    member this.SearchSettings_AddExtensions_HasExtensions () =
        let settings = SearchSettings()
        settings.InExtensions <- settings.AddExtensions "cs" settings.InExtensions
        Assert.That(settings.InExtensions.Length, Is.EqualTo(1))
        Assert.That(settings.InExtensions.Head, Is.EqualTo(".cs"))
        settings.InExtensions <- settings.AddExtensions "java,scala" settings.InExtensions
        Assert.That(settings.InExtensions.Length, Is.EqualTo(3))
        Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.SearchSettings_AddPatterns_HasPatterns () =
        let settings = SearchSettings()
        settings.SearchPatterns <- settings.AddPattern "Search" settings.SearchPatterns
        Assert.That(settings.SearchPatterns.Length, Is.EqualTo(1))
        Assert.That(settings.SearchPatterns.Head.ToString(), Is.EqualTo("Search"))
        ()

    [<Test>]
    member this.SearchSettings_SetArchivesOnly_HasSearchArchives () =
        let settings = SearchSettings()
        settings.ArchivesOnly <- true
        Assert.That(settings.ArchivesOnly)
        Assert.That(settings.IncludeArchives)
        Assert.That(settings.SearchArchives)
        ()

    [<Test>]
    member this.SearchSettings_SetDebug_HasVerbose () =
        let settings = SearchSettings()
        settings.Debug <- true
        Assert.That(settings.Debug)
        Assert.That(settings.Verbose)
        ()
