namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type FileUtil() =
    let _binaryExtensions = 
        Utils.ExtensionSet "ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe
                            hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys vsd xls xlsx xlt"
    let _compressedExtensions =
        Utils.ExtensionSet "bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz war zip Z"
    let _noSearchExtensions =
        Utils.ExtensionSet "aif aifc aiff au avi bmp cab dat db dmg eps gif ico idlk ief iso jpe jpeg jpg
                            m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu ogg pdf pict png ps qt ra ram rm rpm
                            scc snd suo tif tiff wav"
    let _textExtensions =
        Utils.ExtensionSet "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                            am app as asc ascx asm asp aspx bash bat bdsproj bsh
                            c cc cfg clj cls cmd cnt conf config cpp cs csh cshtml csproj css csv ctl
                            dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp dsw dtd
                            env etx exp fls fs fsproj h hpp htm html ics iml in inc ini ipr iws
                            java js jsp layout log mak map master mht mxml
                            pas php php3 pl plist pm po properties py rb rc rc2 rdf resx rex rtf rtx
                            scc sgm sgml sh sln smi smil spec sqc sql st str strings suml svg sxw
                            t tcl tld tmx tsv txt url user vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml
                            wbxml webinfo wml wmls wrl wsd wsdd wsdl xlf xml xsd xsl xslt"
    let _unknownExtensions =
        Utils.ExtensionSet "adm aps cli clw def df2 ncb nt nt2 orig pc plg roff sun t tex texinfo tr xwd"

    let _searchableExtensions = Set.union _binaryExtensions (Set.union _compressedExtensions _textExtensions)

    // read-only member properties
    member this.BinaryExtensions = _binaryExtensions
    member this.CompressedExtensions = _compressedExtensions
    member this.NoSearchExtensions = _noSearchExtensions
    member this.SearchableExtensions = _searchableExtensions
    member this.TextExtensions = _textExtensions
    member this.UnknownExtensions = _unknownExtensions

    member this.IsBinaryFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _binaryExtensions

    member this.IsCompressedFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _compressedExtensions

    member this.IsSearchableFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _searchableExtensions

    member this.IsTextFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _textExtensions

    member this.IsUnknownFile (f : FileInfo) =
        (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _unknownExtensions) ||
        (not (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _searchableExtensions) &&
         not (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _noSearchExtensions))

    ;;
