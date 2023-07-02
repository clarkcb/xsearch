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

        Assert.AreEqual(results.Length, 2)

        let firstResult = results[0]
        let expectedFirstLineNum = 29
        Assert.AreEqual(firstResult.LineNum, expectedFirstLineNum)

        let expectedFirstMatchStartIndex = 3
        Assert.AreEqual(firstResult.MatchStartIndex, expectedFirstMatchStartIndex)

        let expectedFirstMatchEndIndex = 11
        Assert.AreEqual(firstResult.MatchEndIndex, expectedFirstMatchEndIndex)

        let secondResult = results[1]
        let expectedSecondLineNum = 35
        Assert.AreEqual(secondResult.LineNum, expectedSecondLineNum)
        let expectedSecondMatchStartIndex = 24
        Assert.AreEqual(secondResult.MatchStartIndex, expectedSecondMatchStartIndex)
        let expectedSecondMatchEndIndex = 32
        Assert.AreEqual(secondResult.MatchEndIndex, expectedSecondMatchEndIndex)
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

        Assert.AreEqual(results.Length, 2)

        let firstResult = results[0]
        let expectedFirstLineNum = 29
        Assert.AreEqual(expectedFirstLineNum, firstResult.LineNum)

        let expectedFirstMatchStartIndex = 3
        Assert.AreEqual(expectedFirstMatchStartIndex, firstResult.MatchStartIndex)

        let expectedFirstMatchEndIndex = 11
        Assert.AreEqual(expectedFirstMatchEndIndex, firstResult.MatchEndIndex)

        let secondResult = results[1]
        let expectedSecondLineNum = 35
        Assert.AreEqual(expectedSecondLineNum, secondResult.LineNum)

        let expectedSecondMatchStartIndex = 24
        Assert.AreEqual(expectedSecondMatchStartIndex, secondResult.MatchStartIndex)

        let expectedSecondMatchEndIndex = 32
        Assert.AreEqual(expectedSecondMatchEndIndex, secondResult.MatchEndIndex)
        ()
