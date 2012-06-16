namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type SearchSettings() =
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
                            pas php php3 pl plist pm po properties py rc rc2 rdf resx rex rtf rtx
                            scc sgm sgml sh sln smi smil spec sqc sql st str strings suml svg sxw
                            t tcl tld tmx tsv txt url user vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml
                            wbxml webinfo wml wmls wrl wsd wsdd wsdl xlf xml xsd xsl xslt"
    let _unknownExtensions =
        Utils.ExtensionSet "adm aps cli clw def df2 ncb nt nt2 orig pc plg roff sun t tex texinfo tr xwd"

    let _inExtensions = new HashSet<string>()
    let _outExtensions = new HashSet<string>()

    let _inDirPatterns = new HashSet<Regex>()
    let _outDirPatterns = new HashSet<Regex>()
    let _inFilePatterns = new HashSet<Regex>()
    let _outFilePatterns = new HashSet<Regex>()
    let _searchPatterns = new HashSet<Regex>()

    let mutable _startPath = ""

    let mutable _doTiming = false
    let mutable _listFiles = false
    let mutable _verbose = false

    // read-only member properties
    member this.BinaryExtensions = _binaryExtensions
    member this.CompressedExtensions = _compressedExtensions
    member this.NoSearchExtensions = _noSearchExtensions
    member this.TextExtensions = _textExtensions
    member this.UnknownExtensions = _unknownExtensions

    member this.InExtensions = _inExtensions
    member this.OutExtensions = _outExtensions
    member this.InDirPatterns = _inDirPatterns
    member this.OutDirPatterns = _outDirPatterns
    member this.InFilePatterns = _inFilePatterns
    member this.OutFilePatterns = _outFilePatterns
    member this.SearchPatterns = _searchPatterns

    member this.HasExtensions = _inExtensions.Count > 0 || _outExtensions.Count > 0
    member this.HasDirPatterns = _inDirPatterns.Count > 0 || _outDirPatterns.Count > 0
    member this.HasFilePatterns = _inFilePatterns.Count > 0 || _outFilePatterns.Count > 0


    // read-write member properties
    member this.StartPath
        with get () = _startPath
        and set startPath = _startPath <- startPath
    member this.DoTiming
        with get() = _doTiming
        and set doTiming = _doTiming <- doTiming
    member this.ListFiles
        with get () = _listFiles
        and set listFiles = _listFiles <- listFiles
    member this.Verbose
        with get() = _verbose
        and set verbose = _verbose <- verbose

    // member methods
    member this.AddExtension(set : HashSet<string>, ext : string) =
        let ext =
            if ext.StartsWith(".") then ext
            else "." + ext
        let success = set.Add(ext)
        ()

    member this.AddInExtension(ext : string) =
        this.AddExtension(_inExtensions, ext)

    member this.AddOutExtension(ext : string) =
        this.AddExtension(_outExtensions, ext)

    member this.AddPattern(set : HashSet<Regex>, pattern : string) =
        let success = set.Add(new Regex(pattern))
        ()

    member this.AddInDirPattern(pattern : string) =
        this.AddPattern(_inDirPatterns, pattern)

    member this.AddOutDirPattern(pattern : string) =
        this.AddPattern(_outDirPatterns, pattern)

    member this.AddInFilePattern(pattern : string) =
        this.AddPattern(_inFilePatterns, pattern)

    member this.AddOutFilePattern(pattern : string) =
        this.AddPattern(_outFilePatterns, pattern)

    member this.AddSearchPattern(pattern : string) =
        this.AddPattern(_searchPatterns, pattern)

    member this.SetDoTiming(flag : bool) =
        _doTiming <- flag

    member this.SetListFiles(flag : bool) =
        _listFiles <- flag

    member this.SetVerbose(flag : bool) =
        _verbose <- flag

    override this.ToString() =
        "SearchSettings(" +
        String.Format("StartPath: \"{0}\"", _startPath) +
        String.Format(", InExtensions: {0}",  "[\"" + String.Join("\", \"", _inExtensions) + "\"]") +
        String.Format(", OutExtensions: {0}", "[\"" + String.Join("\", \"", _outExtensions) + "\"]") +
        String.Format(", InDirPatterns: {0}",  "[\"" + String.Join("\", \"", _inDirPatterns) + "\"]") +
        String.Format(", OutDirPatterns: {0}", "[\"" + String.Join("\", \"", _outDirPatterns) + "\"]") +
        String.Format(", InFilePatterns: {0}",  "[\"" + String.Join("\", \"", _inFilePatterns) + "\"]") +
        String.Format(", OutFilePatterns: {0}", "[\"" + String.Join("\", \"", _outFilePatterns) + "\"]") +
        String.Format(", SearchPatterns: {0}", "[\"" + String.Join("\", \"", _searchPatterns) + "\"]") +
        ")"
    ;;
