namespace FsSearchTests

open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open FsSearchLib

[<TestFixture>]
type SearchResultTests () =

    // member this.FileTypes = new FileTypes()
    member this.CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SearchResultSingleLine_ToString_EqualsExpected () =
        let settings = { SearchSettings.DefaultSettings with Colorize = false; Paths = ["~"] }
        let formatter = SearchResultFormatter(settings)
        let pattern = Regex("Search")
        let file = FileInfo(FileUtil.JoinPath (FileUtil.ExpandPath this.CsSearchPath) "Searcher.cs")
        let searchFile = SearchFile.Create file FileType.Code
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
        let line = "\tpublic class Searcher\n"
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line [] []
        let searchResult = { searchResult with File=searchFile }
        let expectedPath = this.CsSearchPath + "/Searcher.cs"
        let expectedOutput = $"%s{expectedPath}: %d{lineNum}: [%d{matchStartIndex}:%d{matchEndIndex}]: %s{line.Trim()}"
        let output = formatter.Format searchResult
        Assert.AreEqual(expectedOutput, output)
        ()

    [<Test>]
    member this.SearchResultSingleLineLongerThanMaxLineLength_ToString_EqualsExpected () =
        let settings = { SearchSettings.DefaultSettings with Colorize=false; MaxLineLength=100; Paths=["."] }
        let formatter = SearchResultFormatter(settings)
        let pattern = Regex("maxlen")
        let searchFile = SearchFile.Create (FileInfo("./maxlen.txt")) FileType.Code
        let lineNum = 1
        let matchStartIndex = 53
        let matchEndIndex = 59
        let line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line [] []
        let searchResult = { searchResult with File=searchFile }
        let expectedPath = "./maxlen.txt"
        let expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
        let expectedOutput = $"%s{expectedPath}: %d{lineNum}: [%d{matchStartIndex}:%d{matchEndIndex}]: %s{expectedLine}"
        let output = formatter.Format searchResult
        Assert.AreEqual(expectedOutput, output)
        ()

    [<Test>]
    member this.SearchResultMultiLine_ToString_EqualsExpected () =
        let settings = { SearchSettings.DefaultSettings with Colorize=false; Paths=["~"] }
        let formatter = SearchResultFormatter(settings)
        let pattern = Regex("Search")
        let file = FileInfo(FileUtil.JoinPath (FileUtil.ExpandPath this.CsSearchPath) "Searcher.cs")
        let searchFile = SearchFile.Create file FileType.Code
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
        let line = "\tpublic class Searcher"
        let linesBefore = [ "namespace CsSearch"; "{" ]
        let linesAfter = [ "\t{"; "\t\tprivate readonly FileTypes _fileTypes" ]
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line linesBefore linesAfter
        let searchResult = { searchResult with File=searchFile }
        let expectedPath = this.CsSearchPath + "/Searcher.cs"
        let expectedLines = [
                "================================================================================";
                $"%s{expectedPath}: %d{lineNum}: [%d{matchStartIndex}:%d{matchEndIndex}]";
                "--------------------------------------------------------------------------------";
                "   8 | namespace CsSearch";
                "   9 | {";
                "> 10 | \tpublic class Searcher";
                "  11 | \t{";
                "  12 | \t\tprivate readonly FileTypes _fileTypes\n"
            ]
        let expectedOutput = String.concat "\n" expectedLines
        let output = formatter.Format searchResult
        Assert.AreEqual(expectedOutput, output)
        ()

    [<Test>]
    member this.SearchResultBinaryFile_ToString_EqualsExpected () =
        let settings = { SearchSettings.DefaultSettings with Paths=["~"] }
        let formatter = SearchResultFormatter(settings)
        let pattern = Regex("Search")
        let file = FileInfo(FileUtil.JoinPath (FileUtil.ExpandPath this.CsSearchPath) "Searcher.exe")
        let searchFile = SearchFile.Create file FileType.Binary
        let lineNum = 0
        let matchStartIndex = 0
        let matchEndIndex = 0
        let line : string = null
        let searchResult = SearchResult.Create pattern lineNum matchStartIndex matchEndIndex line [] []
        let searchResult = { searchResult with File=searchFile }
        let expectedPath = this.CsSearchPath + "/Searcher.exe"
        let expectedOutput = $"%s{expectedPath} matches at [0:0]"
        let output = formatter.Format searchResult
        Assert.AreEqual(expectedOutput, output)
        ()
