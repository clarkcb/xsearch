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
        match SearchOptions.SettingsFromArgs(args) with
        | Ok settings ->
            Assert.That(settings.ArchivesOnly, Is.False)
            Assert.That(settings.Colorize)
            Assert.That(settings.Debug, Is.False)
            Assert.That(settings.FirstMatch, Is.False)
            Assert.That(settings.IncludeHidden, Is.False)
            Assert.That(settings.LinesAfter, Is.EqualTo(0))
            Assert.That(settings.LinesBefore, Is.EqualTo(0))
            Assert.That(settings.MaxLineLength, Is.EqualTo(150))
            Assert.That(settings.MultiLineSearch, Is.False)
            Assert.That(settings.Paths.IsEmpty)
            Assert.That(settings.PrintDirs, Is.False)
            Assert.That(settings.PrintFiles, Is.False)
            Assert.That(settings.PrintLines, Is.False)
            Assert.That(settings.PrintResults)
            Assert.That(settings.PrintUsage, Is.False)
            Assert.That(settings.PrintVersion, Is.False)
            Assert.That(settings.Recursive)
            Assert.That(settings.SearchArchives, Is.False)
            Assert.That(settings.TextFileEncoding, Is.EqualTo("utf-8"))
            Assert.That(settings.UniqueLines, Is.False)
            Assert.That(settings.Verbose, Is.False)
        | Error _ ->
            Assert.That(true, Is.False)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "." |]
        match SearchOptions.SettingsFromArgs(args) with
        | Ok settings ->
            Assert.That(settings.InExtensions.Length, Is.EqualTo(1))
            Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
            Assert.That(settings.SearchPatterns.Length, Is.EqualTo(1))
            Assert.That(settings.SearchPatterns.Head.ToString(), Is.EqualTo("Search"))
            Assert.That(settings.Paths.Length, Is.EqualTo(1))
        | Error _ ->
            Assert.That(true, Is.False)
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsSearchException () =
        let args = [| "-x"; "cs"; "-s"; "Search"; "."; "-Q" |]
        match SearchOptions.SettingsFromArgs(args) with
        | Ok _ ->
            Assert.That(true, Is.False)
        | Error e->
            Assert.That(e, Is.EqualTo("Invalid option: Q"))
        ()
