namespace FsSearchTests

open System
open System.IO
open NUnit.Framework
open FsSearch

[<TestFixture>]
type FileUtilTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    //////////////////////////////////////////////////////////////
    // GetRelativePath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.GetRelativePath_PathWithCurrentDirectory_RelativePath () =
        let path = Environment.CurrentDirectory + "/rest/of/path/"
        Assert.AreEqual("./rest/of/path/", FileUtil.GetRelativePath path ".")
        ()

    [<Test>]
    member this.GetRelativePath_PathWithoutCurrentDirectory_FullPath () =
        let path = "/a/full/path/by/itself/"
        Assert.AreEqual(path, FileUtil.GetRelativePath path "/a/full/path")
        ()

    [<Test>]
    member this.GetRelativePath_RelativePath_Unchanged () =
        let path = "./a/relative/path/"
        Assert.AreEqual(path, FileUtil.GetRelativePath path ".")
        ()

    //////////////////////////////////////////////////////////////
    // IsDotDir tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.IsDotDir_IsSingleDot_IsDotDir () =
        let dotDir = "."
        Assert.IsTrue(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsSingleDotWithTrailingSlash_IsDotDir () =
        let dotDir = "./"
        Assert.IsTrue(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsDoubleDot_IsDotDir () =
        let dotDir = ".."
        Assert.IsTrue(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsDoubleDotWithTrailingSlash_IsDotDir () =
        let dotDir = "../"
        Assert.IsTrue(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsNotDotDir_IsNotDotDir () =
        let nonDotDir = "~/path"
        Assert.IsFalse(FileUtil.IsDotDir(nonDotDir))
        ()

    //////////////////////////////////////////////////////////////
    // IsHidden tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.IsHidden_StartsWithDot_IsHidden () =
        let hiddenFile = FileInfo(".FileUtilTests.cs")
        Assert.IsTrue(FileUtil.IsHiddenFile(hiddenFile))
        ()

    [<Test>]
    member this.IsHidden_NotStartsWithDot_NotIsHidden () =
        let hiddenFile = FileInfo("FileUtilTests.cs")
        Assert.IsFalse(FileUtil.IsHiddenFile(hiddenFile))
        ()

    [<Test>]
    member this.IsHidden_SingleDot_NotIsHidden () =
        let dotDir = DirectoryInfo(".")
        Assert.IsFalse(FileUtil.IsHiddenFile(dotDir))
        ()

    [<Test>]
    member this.IsHidden_DoubleDot_NotIsHidden () =
        let dotDir = DirectoryInfo("..")
        Assert.IsFalse(FileUtil.IsHiddenFile(dotDir))
        ()

    //////////////////////////////////////////////////////////////
    // ExpandPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.ExpandPath_WithTilde_ExpandHome () =
        let path = "~/src/git/xsearch"
        let expected = FileUtil.JoinPath (FileUtil.GetHomePath()) (path.Substring(1))
        let actual = FileUtil.ExpandPath(path)
        Assert.AreEqual(expected, actual)
        ()

    [<Test>]
    member this.ExpandPath_NoTilde_UnchangedPath () =
        let path = "/a/full/path/"
        Assert.AreEqual(path, FileUtil.ExpandPath(path))
        ()

    [<Test>]
    member this.ExpandPath_WithBackSlashes_UnchangedPath () =
        let path = @"C:\src\git\xsearch\"
        Assert.AreEqual(path, FileUtil.ExpandPath(path))
        ()

    //////////////////////////////////////////////////////////////
    // NormalizePath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.NormalizePath_NoTrailingSlash_UnchangedPath () =
        let path = "~/src/git/xsearch"
        Assert.AreEqual(path, FileUtil.NormalizePath(path))
        ()

    [<Test>]
    member this.NormalizePath_TrailingSlash_TrimmedPath () =
        let path = "~/src/git/xsearch/"
        Assert.AreEqual("~/src/git/xsearch", FileUtil.NormalizePath(path))
        ()

    [<Test>]
    member this.NormalizePath_TrailingBackSlash_TrimmedPath () =
        let path = @"C:\src\git\xsearch\"
        Assert.AreEqual(@"C:\src\git\xsearch", FileUtil.NormalizePath(path))
        ()

    //////////////////////////////////////////////////////////////
    // JoinPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.JoinPath_NoTrailingSlash_EqualsExpected () =
        let path = "~/src/git/xsearch/csharp/CsSearch/CsSearchTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.AreEqual(pathAndFile, FileUtil.JoinPath path filename)
        ()

    [<Test>]
    member this.JoinPath_TrailingSlash_EqualsExpected () =
        let path = "~/src/git/xsearch/csharp/CsSearch/CsSearchTests/"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + filename
        Assert.AreEqual(pathAndFile, FileUtil.JoinPath path filename)
        ()

    [<Test>]
    member this.JoinPath_NoTrailingBackSlash_EqualsExpected () =
        let path = @"C:\src\git\xsearch"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "\\" + filename
        Assert.AreEqual(pathAndFile, FileUtil.JoinPath path filename)
        ()

    [<Test>]
    member this.JoinPath_TrailingBackSlash_EqualsExpected () =
        let path = @"C:\src\git\xsearch\"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + filename
        Assert.AreEqual(pathAndFile, FileUtil.JoinPath path filename)
        ()

    [<Test>]
    member this.JoinPath_NoSlashes_EqualsExpected () =
        let path = "CsSearchTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.AreEqual(pathAndFile, FileUtil.JoinPath path filename)
        ()
