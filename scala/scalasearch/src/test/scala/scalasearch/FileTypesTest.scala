package scalasearch

import java.io.File

import org.scalatest.funsuite.AnyFunSuite

class FileTypesTest extends AnyFunSuite {

  test("test archive extensions") {
    """7z arj bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz war zip zipx Z""".
      split("\\s+").foreach { ext =>
        val archiveFile = new File("archive."+ext)
        println("archiveFile: "+archiveFile)
        assert(!FileTypes.isBinaryFile(archiveFile))
        assert(!FileTypes.isCodeFile(archiveFile))
        assert(FileTypes.isArchiveFile(archiveFile))
        assert(FileTypes.isSearchableFile(archiveFile))
        assert(!FileTypes.isTextFile(archiveFile))
        assert(!FileTypes.isUnknownFile(archiveFile))
        val fileType = FileTypes.getFileType(archiveFile)
        assert(fileType == FileType.Archive)
    }
  }

  test("test binary extensions") {
    """a ai beam bin chm class com dat dbmdl dcr dir dll dms doc dot dxr dylib
      |epub exe fm hi hlp indd lib lnk mdb mo mobi mpp nib o obj odm odt ott
      |pages pdb ppt psd pub pyc pyo qxd rpt so swf sys vsd wpd wps wpt wri
      |xls xlt""".stripMargin.split("\\s+").foreach { ext =>
        val binFile = new File("binfile."+ext)
        println("binFile: "+binFile)
        assert(FileTypes.isBinaryFile(binFile))
        assert(!FileTypes.isCodeFile(binFile))
        assert(!FileTypes.isArchiveFile(binFile))
        assert(FileTypes.isSearchableFile(binFile))
        assert(!FileTypes.isTextFile(binFile))
        assert(!FileTypes.isUnknownFile(binFile))
        val fileType = FileTypes.getFileType(binFile)
        assert(fileType == FileType.Binary)
    }
  }

  test("test code extensions") {
    """ada adb ads applejs as asm au3 bas bash bat boo bsh c c++ cbl cc cfm cgi
      |clj cls cmd cob coffee cpp cs csh cxx d e el elm erl es ex exs frm fs fth
      |fx go groovy h h++ hh hpp hs java js js2 jsf json jsp jspf kt lhs lisp
      |lua m ml pas php php3 php4 php5 pl pm ps1 psc1 psd1 psm1 pxd pxi py pyw
      |pyx r rb rkt rs s sass sbt sc scm scss scala sh swift tcl ts vb vbs""".
      stripMargin.split("\\s+").foreach { ext =>
        val codeFile = new File("codefile."+ext)
        println("codeFile: "+codeFile)
        assert(!FileTypes.isBinaryFile(codeFile))
        assert(FileTypes.isCodeFile(codeFile))
        assert(!FileTypes.isArchiveFile(codeFile))
        assert(FileTypes.isSearchableFile(codeFile))
        assert(FileTypes.isTextFile(codeFile))
        assert(!FileTypes.isUnknownFile(codeFile))
        val fileType = FileTypes.getFileType(codeFile)
        assert(fileType == FileType.Code)
    }
  }

  test("test nosearch extensions") {
    """aif aifc aiff au avi bmp cab cur db dib dmg eot gif icns ico
      |idlk ief iso jpe jpeg jpg m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu
      |ogg otf pdf pict png qt ra ram rm rpm snd suo tif tiff tte ttf wav
      |woff""".
      stripMargin.split("\\s+").foreach { ext =>
        val nosearchFile = new File("nosearch."+ext)
        println("nosearchFile: "+nosearchFile)
        assert(!FileTypes.isBinaryFile(nosearchFile))
        assert(!FileTypes.isArchiveFile(nosearchFile))
        assert(!FileTypes.isSearchableFile(nosearchFile))
        assert(!FileTypes.isTextFile(nosearchFile))
        assert(FileTypes.isUnknownFile(nosearchFile))
        val fileType = FileTypes.getFileType(nosearchFile)
        assert(fileType == FileType.Unknown)
    }
  }

  test("test text extensions") {
    """1 10 11 12 13 14 15 16 17 18 19 2 20 3 323 4 5 6 7 8 9 am app
      |asc ascx asm asmx asp aspx bib brf cabal cfg cls cmake cmd cnt conf css
      |csv ctl d dbml dbschema ddl dep dfm diff disco dlg dof dpr drl dsp dsw
      |dtd elt ent env eps etx exp feature fls gcd hql hs htc htm html hxx ics
      |icz iml in inc ini ipr iws jad jam jql layout lhs log ltx mak mako
      |manifest map markdown master md mf mht mml moc mod mxml p patch plist pm
      |po pot properties ps pt rc rc2 rdf rex rtf rtx scc sct sfv sgm sgml sht
      |shtm shtml sln smi smil spec sqc sql st str strings sty suml sxw t tex
      |text tk tld tm tmx tsv txt ui uls uml url user vbs vcf vcs vm vrml vssscc
      |vxml wbxml webinfo wml wmls wrl wsc wsd wsdd xlf xsp yaml
      |yml""".stripMargin.split("\\s+").foreach { ext =>
        val textFile = new File("textFile."+ext)
        println("textFile: "+textFile)
        assert(!FileTypes.isBinaryFile(textFile))
        assert(!FileTypes.isArchiveFile(textFile))
        assert(FileTypes.isSearchableFile(textFile))
        assert(FileTypes.isTextFile(textFile))
        assert(!FileTypes.isUnknownFile(textFile))
        val fileType = FileTypes.getFileType(textFile)
        assert(Set(FileType.Code, FileType.Text, FileType.Xml).contains(fileType))
    }
  }

  test("test unknown extensions") {
    """adm aps cli clw def df2 ncb nt nt2 orig pc plg roff sun texinfo tr xwd""".
      split("\\s+").foreach { ext =>
        val unknownFile = new File("unknown."+ext)
        println("unknownFile: "+unknownFile)
        assert(!FileTypes.isBinaryFile(unknownFile))
        assert(!FileTypes.isArchiveFile(unknownFile))
        assert(!FileTypes.isSearchableFile(unknownFile))
        assert(!FileTypes.isTextFile(unknownFile))
        assert(FileTypes.isUnknownFile(unknownFile))
        val fileType = FileTypes.getFileType(unknownFile)
        assert(fileType == FileType.Unknown)
    }
  }

  test("test xml extensions") {
    """atom atomcat atomsrv bdsproj config csproj davmount dbproj docx dotx
      |fsproj fxml jhm jnlp kml mm pom potx ppsx pptx qrc rdf resx rng rss
      |settings sldx stc std sti stw svg svgz sxc sxd sxg sxi stw sxm sxw tld
      |vbproj vcproj vdproj wadl wsdd wsdl x3d xaml xhtml xht xjb xlsx xltx xml
      |xps xsd xsl xslt xspf xul""".stripMargin.split("\\s+").foreach { ext =>
        val xmlFile = new File("xmlfile."+ext)
        println("xmlFile: "+xmlFile)
        assert(!FileTypes.isBinaryFile(xmlFile))
        assert(!FileTypes.isCodeFile(xmlFile))
        assert(!FileTypes.isArchiveFile(xmlFile))
        assert(FileTypes.isSearchableFile(xmlFile))
        assert(FileTypes.isTextFile(xmlFile))
        assert(!FileTypes.isUnknownFile(xmlFile))
        val fileType = FileTypes.getFileType(xmlFile)
        assert(fileType == FileType.Xml)
    }
  }
}
