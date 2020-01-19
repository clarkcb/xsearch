namespace FsSearchTests

open System.IO
open NUnit.Framework
open FsSearch

[<TestFixture>]
type FileTypesTests () =

    member this.FileTypes = new FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetFileType_ArchiveFile_FileTypeArchive () =
        printfn "GetFileType_ArchiveFile_FileTypeArchive"
        let archiveFile = new FileInfo("archive.zip")
        Assert.AreEqual(this.FileTypes.GetFileType(archiveFile), FileType.Archive)
        ()

    [<Test>]
    member this.GetFileType_BinaryFile_FileTypeBinary () =
        printfn "GetFileType_BinaryFile_FileTypeBinary"
        let binaryFile = new FileInfo("binary.exe")
        Assert.AreEqual(this.FileTypes.GetFileType(binaryFile), FileType.Binary)
        ()

    [<Test>]
    member this.GetFileType_CodeFile_FileTypeCode () =
        printfn "GetFileType_CodeFile_FileTypeCode"
        let codeFile = new FileInfo("code.cs")
        Assert.AreEqual(this.FileTypes.GetFileType(codeFile), FileType.Code)
        ()

    [<Test>]
    member this.GetFileType_TextFile_FileTypeText () =
        printfn "GetFileType_TextFile_FileTypeText"
        let textFile = new FileInfo("text.txt")
        Assert.AreEqual(this.FileTypes.GetFileType(textFile), FileType.Text)
        ()

    [<Test>]
    member this.GetFileType_XmlFile_FileTypeXml () =
        printfn "GetFileType_XmlFile_FileTypeXml"
        let xmlFile = new FileInfo("markup.xml")
        Assert.AreEqual(this.FileTypes.GetFileType(xmlFile), FileType.Xml)
        ()

    [<Test>]
    member this.GetFileType_UnknownFile_FileTypeUnknown () =
        printfn "GetFileType_UnknownFile_FileTypeUnknown"
        let unknownFile = new FileInfo("unknown.xyz")
        Assert.AreEqual(this.FileTypes.GetFileType(unknownFile), FileType.Unknown)
        ()

    [<Test>]
    member this.IsArchiveFile_ArchiveFile_True () =
        printfn "IsArchiveFile_ArchiveFile_True"
        let archiveFile = new FileInfo("archive.zip")
        Assert.IsTrue(this.FileTypes.IsArchiveFile(archiveFile))
        ()

    [<Test>]
    member this.IsBinaryFile_BinaryFile_True () =
        printfn "IsBinaryFile_BinaryFile_True"
        let binaryFile = new FileInfo("binary.exe")
        Assert.IsTrue(this.FileTypes.IsBinaryFile(binaryFile))
        ()

    [<Test>]
    member this.IsCodeFile_CodeFile_True () =
        printfn "IsCodeFile_CodeFile_True"
        let codeFile = new FileInfo("code.cs")
        Assert.IsTrue(this.FileTypes.IsCodeFile(codeFile))
        ()

    [<Test>]
    member this.IsTextFile_TextFile_True () =
        printfn "IsTextFile_TextFile_True"
        let textFile = new FileInfo("text.txt")
        Assert.IsTrue(this.FileTypes.IsTextFile(textFile))
        ()

    [<Test>]
    member this.IsXmlFile_XmlFile_True () =
        printfn "IsXmlFile_XmlFile_True"
        let xmlFile = new FileInfo("markup.xml")
        Assert.IsTrue(this.FileTypes.IsXmlFile(xmlFile))
        ()

    [<Test>]
    member this.IsSearchableFile_XmlFile_True () =
        printfn "IsSearchableFile_XmlFile_True"
        let xmlFile = new FileInfo("markup.xml")
        Assert.IsTrue(this.FileTypes.IsSearchableFile(xmlFile))
        ()
