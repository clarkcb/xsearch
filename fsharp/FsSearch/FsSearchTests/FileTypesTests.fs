namespace FsSearchTests

open System.IO
open NUnit.Framework
open FsSearchLib

[<TestFixture>]
type FileTypesTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetFileType_ArchiveFile_FileTypeArchive () =
        printfn "GetFileType_ArchiveFile_FileTypeArchive"
        let archiveFile = FileInfo("archive.zip")
        Assert.AreEqual(FileType.Archive, this.FileTypes.GetFileType(archiveFile))
        ()

    [<Test>]
    member this.GetFileType_BinaryFile_FileTypeBinary () =
        printfn "GetFileType_BinaryFile_FileTypeBinary"
        let binaryFile = FileInfo("binary.exe")
        Assert.AreEqual(FileType.Binary, this.FileTypes.GetFileType(binaryFile))
        ()

    [<Test>]
    member this.GetFileType_CodeFile_FileTypeCode () =
        printfn "GetFileType_CodeFile_FileTypeCode"
        let codeFile = FileInfo("code.cs")
        Assert.AreEqual(FileType.Code, this.FileTypes.GetFileType(codeFile))
        ()

    [<Test>]
    member this.GetFileType_TextFile_FileTypeText () =
        printfn "GetFileType_TextFile_FileTypeText"
        let textFile = FileInfo("text.txt")
        Assert.AreEqual(FileType.Text, this.FileTypes.GetFileType(textFile))
        ()

    [<Test>]
    member this.GetFileType_XmlFile_FileTypeXml () =
        printfn "GetFileType_XmlFile_FileTypeXml"
        let xmlFile = FileInfo("markup.xml")
        Assert.AreEqual(FileType.Xml, this.FileTypes.GetFileType(xmlFile))
        ()

    [<Test>]
    member this.GetFileType_UnknownFile_FileTypeUnknown () =
        printfn "GetFileType_UnknownFile_FileTypeUnknown"
        let unknownFile = FileInfo("unknown.xyz")
        Assert.AreEqual(FileType.Unknown, this.FileTypes.GetFileType(unknownFile))
        ()

    [<Test>]
    member this.IsArchiveFile_ArchiveFile_True () =
        printfn "IsArchiveFile_ArchiveFile_True"
        let archiveFile = FileInfo("archive.zip")
        Assert.IsTrue(this.FileTypes.IsArchiveFile(archiveFile))
        ()

    [<Test>]
    member this.IsBinaryFile_BinaryFile_True () =
        printfn "IsBinaryFile_BinaryFile_True"
        let binaryFile = FileInfo("binary.exe")
        Assert.IsTrue(this.FileTypes.IsBinaryFile(binaryFile))
        ()

    [<Test>]
    member this.IsCodeFile_CodeFile_True () =
        printfn "IsCodeFile_CodeFile_True"
        let codeFile = FileInfo("code.cs")
        Assert.IsTrue(this.FileTypes.IsCodeFile(codeFile))
        ()

    [<Test>]
    member this.IsTextFile_TextFile_True () =
        printfn "IsTextFile_TextFile_True"
        let textFile = FileInfo("text.txt")
        Assert.IsTrue(this.FileTypes.IsTextFile(textFile))
        ()

    [<Test>]
    member this.IsXmlFile_XmlFile_True () =
        printfn "IsXmlFile_XmlFile_True"
        let xmlFile = FileInfo("markup.xml")
        Assert.IsTrue(this.FileTypes.IsXmlFile(xmlFile))
        ()

    [<Test>]
    member this.IsSearchableFile_XmlFile_True () =
        printfn "IsSearchableFile_XmlFile_True"
        let xmlFile = FileInfo("markup.xml")
        Assert.IsTrue(this.FileTypes.IsSearchableFile(xmlFile))
        ()
