namespace FsSearchTests

open System.IO
open NUnit.Framework
open FsSearchLib
open FsFind

[<TestFixture>]
type SearcherTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    member this.GetTestFileContent () : string =
        try
            EmbeddedTestResource.GetResourceFileContents "FsSearchTests.Resources.testFile2.txt"
        with
        | :? IOException as e -> raise e
        | :? SearchException as e -> raise e

    member this.GetSettings () : SearchSettings =
        let settings = SearchSettings()
        settings.Paths <- ["."]
        settings.SearchPatterns <- settings.AddPattern "Searcher" settings.SearchPatterns
        settings
    

    //////////////////////////////////////////////////////////////
    // SearchTextReaderLines test
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestSearchTextReaderLines () =
        let settings = this.GetSettings()
        let searcher = Searcher(settings)
        let enumerableLines = this.GetTestFileContent().Split([|'\n'; '\r'|]) |> Array.toList
        let results = searcher.SearchLines(enumerableLines)

        Assert.That(results.Length, Is.EqualTo(2))

        let firstResult = results[0]
        let expectedFirstLineNum = 30
        Assert.That(firstResult.LineNum, Is.EqualTo(expectedFirstLineNum))

        let expectedFirstMatchStartIndex = 3
        Assert.That(firstResult.MatchStartIndex, Is.EqualTo(expectedFirstMatchStartIndex))

        let expectedFirstMatchEndIndex = 11
        Assert.That(firstResult.MatchEndIndex, Is.EqualTo(expectedFirstMatchEndIndex))

        let secondResult = results[1]
        let expectedSecondLineNum = 36
        Assert.That(secondResult.LineNum, Is.EqualTo(expectedSecondLineNum))
        let expectedSecondMatchStartIndex = 24
        Assert.That(secondResult.MatchStartIndex, Is.EqualTo(expectedSecondMatchStartIndex))
        let expectedSecondMatchEndIndex = 32
        Assert.That(secondResult.MatchEndIndex, Is.EqualTo(expectedSecondMatchEndIndex))
        ()

    //////////////////////////////////////////////////////////////
    // SearchMultiLineString test
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestSearchMultiLineString () =
        let settings = this.GetSettings()
        let searcher = Searcher(settings)
        let contents = this.GetTestFileContent()
        let results = searcher.SearchContents(contents)

        Assert.That(results.Length, Is.EqualTo(2))

        let firstResult = results[0]
        let expectedFirstLineNum = 30
        Assert.That(firstResult.LineNum, Is.EqualTo(expectedFirstLineNum))

        let expectedFirstMatchStartIndex = 3
        Assert.That(firstResult.MatchStartIndex, Is.EqualTo(expectedFirstMatchStartIndex))

        let expectedFirstMatchEndIndex = 11
        Assert.That(firstResult.MatchEndIndex, Is.EqualTo(expectedFirstMatchEndIndex))

        let secondResult = results[1]
        let expectedSecondLineNum = 36
        Assert.That(secondResult.LineNum, Is.EqualTo(expectedSecondLineNum))

        let expectedSecondMatchStartIndex = 24
        Assert.That(secondResult.MatchStartIndex, Is.EqualTo(expectedSecondMatchStartIndex))

        let expectedSecondMatchEndIndex = 32
        Assert.That(secondResult.MatchEndIndex, Is.EqualTo(expectedSecondMatchEndIndex))
        ()
