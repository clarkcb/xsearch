namespace FsSearchTests

open System.IO
open NUnit.Framework
open FsSearch

[<TestFixture>]
type SearcherTests () =

    member this.FileTypes = new FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    member this.GetSettings () : SearchSettings.t =
        let settings = { SearchSettings.DefaultSettings with StartPath = "." }
        let settings = { settings with SearchPatterns = SearchSettings.AddPattern "Searcher" settings.SearchPatterns }
        settings
    
    // member this.GetTestFileContent () : string =
    //     try
    //         EmbeddedTestResource.GetResourceFileContents "FsSearchTests.Resources.testFile2.txt"
    //     with
    //     | :? IOException as e -> raise e
    //     | :? SearchException as e -> raise e

    
    //////////////////////////////////////////////////////////////
    // IsSearchDirectory tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsSearchDirectory_SingleDot_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        Assert.True(searcher.IsSearchDir(new DirectoryInfo(".")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_DoubleDot_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        Assert.True(searcher.IsSearchDir(new DirectoryInfo("..")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_IsHidden_False () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        Assert.False(searcher.IsSearchDir(new DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_IsHiddenIncludeHidden_True () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let searcher = new Searcher(settings)
        Assert.True(searcher.IsSearchDir(new DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_NoPatterns_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        Assert.True(searcher.IsSearchDir(new DirectoryInfo("/Users")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = SearchSettings.AddPattern "Search" settings.InDirPatterns }
        let searcher = new Searcher(settings)
        Assert.True(searcher.IsSearchDir(new DirectoryInfo("CsSearch")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = SearchSettings.AddPattern "Search" settings.OutDirPatterns }
        let searcher = new Searcher(settings)
        Assert.False(searcher.IsSearchDir(new DirectoryInfo("CsSearch")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = SearchSettings.AddPattern "SearchFiles" settings.InDirPatterns }
        let searcher = new Searcher(settings)
        Assert.False(searcher.IsSearchDir(new DirectoryInfo("CsSearch")))
        ()

    [<Test>]
    member this.TestIsSearchDirectory_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = SearchSettings.AddPattern "SearchFiles" settings.OutDirPatterns }
        let searcher = new Searcher(settings)
        let dir = new DirectoryInfo("CsSearch")
        Assert.True(searcher.IsSearchDir(dir))
        ()


    //////////////////////////////////////////////////////////////
    // IsSearchFile tests
    //////////////////////////////////////////////////////////////

    [<Test>]
    member this.TestIsSearchFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "cs" settings.InExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "java" settings.InExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsSearchFile(sf))
        ()


    [<Test>]
    member this.TestIsSearchFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = SearchSettings.AddExtensions "cs" settings.OutExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = SearchSettings.AddExtensions "java" settings.OutExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = SearchSettings.AddPattern "Search" settings.InFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("Searcher.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = SearchSettings.AddPattern "Search" settings.InFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = SearchSettings.AddPattern "Search" settings.OutFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("Searcher.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsSearchFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = SearchSettings.AddPattern "Search" settings.OutFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsSearchFile(sf))
        ()


    //////////////////////////////////////////////////////////////
    // IsArchiveSearchFile tests
    //////////////////////////////////////////////////////////////

    [<Test>]
    member this.TestIsArchiveSearchFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = SearchSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = SearchSettings.AddExtensions "gz" settings.InArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsArchiveSearchFile(sf))
        ()


    [<Test>]
    member this.TestIsArchiveSearchFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = SearchSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = SearchSettings.AddExtensions "gz" settings.OutArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = SearchSettings.AddPattern "arch" settings.InArchiveFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = SearchSettings.AddPattern "archives" settings.InArchiveFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = SearchSettings.AddPattern "arch" settings.OutArchiveFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.IsArchiveSearchFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveSearchFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = SearchSettings.AddPattern "archives" settings.OutArchiveFilePatterns }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.IsArchiveSearchFile(sf))
        ()

    //////////////////////////////////////////////////////////////
    // FilterFile tests
    //////////////////////////////////////////////////////////////

    [<Test>]
    member this.TestFilterFile_IsHidden_False () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        let file = new FileInfo(".gitignore")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsHiddenIncludeHidden_True () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let searcher = new Searcher(settings)
        let file = new FileInfo(".gitignore")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveNoSearchArchives_False () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveSearchArchives_True () =
        let settings = { this.GetSettings() with SearchArchives = true }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsArchiveSearchFile_True () =
        let settings = { this.GetSettings() with SearchArchives = true }
        let settings = { settings with InArchiveExtensions = SearchSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NotIsArchiveSearchFile_False () =
        let settings = { this.GetSettings() with SearchArchives = true }
        let settings = { settings with OutArchiveExtensions = SearchSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveFileArchivesOnly_True () =
        let settings = { this.GetSettings() with ArchivesOnly = true }
        let searcher = new Searcher(settings)
        let file = new FileInfo("archive.zip")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()


    [<Test>]
    member this.TestFilterFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsSearchFile_True () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = SearchSettings.AddExtensions "cs" settings.InExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NotIsSearchFile_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = SearchSettings.AddExtensions "cs" settings.OutExtensions }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NonArchiveFileArchivesOnly_False () =
        let settings = { this.GetSettings() with ArchivesOnly = true }
        let searcher = new Searcher(settings)
        let file = new FileInfo("FileUtil.cs")
        let sf = SearchFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(searcher.FilterFile(sf))
        ()


    // //////////////////////////////////////////////////////////////
    // // SearchTextReaderLines test
    // //////////////////////////////////////////////////////////////
    // [<Test>]
    // member this.TestSearchTextReaderLines () =
    //     let settings = this.GetSettings()
    //     let searcher = new Searcher(settings)
    //     let enumerableLines = this.GetTestFileContent().Split([|'\n'; '\r'|]) |> Array.toList
    //     let results = searcher.SearchLines(enumerableLines)

    //     Assert.AreEqual(results.Length, 2)

    //     let firstResult = results.[0]
    //     let expectedFirstLineNum = 23
    //     Assert.AreEqual(firstResult.LineNum, expectedFirstLineNum)

    //     let expectedFirstMatchStartIndex = 3
    //     Assert.AreEqual(firstResult.MatchStartIndex, expectedFirstMatchStartIndex)

    //     let expectedFirstMatchEndIndex = 11
    //     Assert.AreEqual(firstResult.MatchEndIndex, expectedFirstMatchEndIndex)

    //     let secondResult = results.[1]
    //     let expectedSecondLineNum = 29
    //     Assert.AreEqual(secondResult.LineNum, expectedSecondLineNum)
    //     let expectedSecondMatchStartIndex = 24
    //     Assert.AreEqual(secondResult.MatchStartIndex, expectedSecondMatchStartIndex)
    //     let expectedSecondMatchEndIndex = 32
    //     Assert.AreEqual(secondResult.MatchEndIndex, expectedSecondMatchEndIndex)
    //     ()

    // //////////////////////////////////////////////////////////////
    // // SearchMultiLineString test
    // //////////////////////////////////////////////////////////////
    // [<Test>]
    // member this.TestSearchMultiLineString () =
    //     let settings = this.GetSettings()
    //     let searcher = new Searcher(settings)
    //     //let testFile = GetTestFile()
    //     //let contents = FileUtil.GetFileContents(testFile.FullName)
    //     let contents = this.GetTestFileContent()
    //     let results = searcher.SearchContents(contents)

    //     Assert.AreEqual(results.Count, 2)

    //     let firstResult = results.[0]
    //     let expectedFirstLineNum = 23
    //     Assert.AreEqual(firstResult.LineNum, expectedFirstLineNum)

    //     let expectedFirstMatchStartIndex = 3
    //     Assert.AreEqual(firstResult.MatchStartIndex, expectedFirstMatchStartIndex)

    //     let expectedFirstMatchEndIndex = 11
    //     Assert.AreEqual(firstResult.MatchEndIndex, expectedFirstMatchEndIndex)

    //     let secondResult = results.[1]
    //     let expectedSecondLineNum = 29
    //     Assert.AreEqual(secondResult.LineNum, expectedSecondLineNum)

    //     let expectedSecondMatchStartIndex = 24
    //     Assert.AreEqual(secondResult.MatchStartIndex, expectedSecondMatchStartIndex)

    //     let expectedSecondMatchEndIndex = 32
    //     Assert.AreEqual(secondResult.MatchEndIndex, expectedSecondMatchEndIndex)
    //     ()
