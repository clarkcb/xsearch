namespace FsSearchTests

open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open FsSearchLib
open FsFindLib

[<TestFixture>]
type SearchResultTests () =

    member this.CsSearchPath = "/Users/cary/src/xsearch/csharp/CsSearch/CsSearch"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SearchResultSingleLine_ToString_EqualsExpected () =
        let pattern = Regex("Search")
        let file = FileInfo(Path.Join(this.CsSearchPath, "Searcher.cs"))
        let fileResult = FileResult.Create file FileType.Code
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
        let line = "\tpublic class Searcher\n"
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line [] []
        let searchResult = { searchResult with File=fileResult }

        Assert.That(searchResult.SearchPattern.ToString(), Is.EqualTo(pattern.ToString()))
        Assert.That(searchResult.LineNum, Is.EqualTo(lineNum))
        Assert.That(searchResult.MatchStartIndex, Is.EqualTo(matchStartIndex))
        Assert.That(searchResult.MatchEndIndex, Is.EqualTo(matchEndIndex))
        Assert.That(searchResult.Line, Is.EqualTo(line))
        ()

    [<Test>]
    member this.SearchResultMultiLine_ToString_EqualsExpected () =
        let pattern = Regex("Search")
        let file = FileInfo(Path.Join(this.CsSearchPath, "Searcher.cs"))
        let fileResult = FileResult.Create file FileType.Code
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
        let line = "\tpublic class Searcher"
        let linesBefore = [ "namespace CsSearch"; "{" ]
        let linesAfter = [ "\t{"; "\t\tprivate readonly FileTypes _fileTypes" ]
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line linesBefore linesAfter
        let searchResult = { searchResult with File=fileResult }

        Assert.That(searchResult.SearchPattern.ToString(), Is.EqualTo(pattern.ToString()))
        Assert.That(searchResult.LineNum, Is.EqualTo(lineNum))
        Assert.That(searchResult.MatchStartIndex, Is.EqualTo(matchStartIndex))
        Assert.That(searchResult.MatchEndIndex, Is.EqualTo(matchEndIndex))
        Assert.That(searchResult.Line, Is.EqualTo(line))
        ()
