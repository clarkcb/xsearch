namespace FsSearchTests

open System.IO
open NUnit.Framework
open FsSearch

[<TestFixture>]
type SearchFileTests () =

    member this.CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch"
    member this.WinCsSearchPath = @"C:\src\xsearch\csharp\CsSearch\CsSearch"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SearchFile_ToString_EqualsExpected () =
        let searchFile = SearchFile.Create (FileInfo(FileUtil.JoinPath this.CsSearchPath "Searcher.cs")) FileType.Code
        Assert.AreEqual(this.CsSearchPath + "/Searcher.cs", SearchFile.ToString(searchFile))
        ()

    [<Test>]
    member this.SearchFileTrailingSlash_ToString_EqualsExpected () =
        let searchFile = SearchFile.Create (FileInfo(FileUtil.JoinPath this.CsSearchPath "Searcher.cs")) FileType.Code
        Assert.AreEqual(this.CsSearchPath + "/Searcher.cs", SearchFile.ToString(searchFile))
        ()

    [<Test>]
    member this.SearchFileBackSlashes_ToString_EqualsExpected () =
        let searchFile = SearchFile.Create (FileInfo(FileUtil.JoinPath this.WinCsSearchPath "Searcher.cs")) FileType.Code
        Assert.AreEqual(this.WinCsSearchPath + @"\Searcher.cs", SearchFile.ToString(searchFile))
        ()

    [<Test>]
    member this.SearchFileBackSlashesTrailingSlash_ToString_EqualsExpected () =
        let searchFile = SearchFile.Create (FileInfo(FileUtil.JoinPath this.WinCsSearchPath "Searcher.cs")) FileType.Code
        Assert.AreEqual(this.WinCsSearchPath + @"\Searcher.cs", SearchFile.ToString(searchFile))
        ()
