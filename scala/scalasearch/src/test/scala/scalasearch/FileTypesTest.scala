package scalasearch

import java.io.File
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileTypesTest extends FunSuite {

  test("test archive extensions") {
    """bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz war zip zipx Z""".
      split("\\s+").foreach {
      ext =>
        val archiveFile = new File("archive."+ext)
        println("archilveFile: "+archiveFile)
        assert(!FileTypes.isBinaryFile(archiveFile))
        assert(FileTypes.isArchiveFile(archiveFile))
        assert(FileTypes.isSearchableFile(archiveFile))
        assert(!FileTypes.isTextFile(archiveFile))
        assert(!FileTypes.isUnknownFile(archiveFile))
        val fileType = FileTypes.getFileType(archiveFile)
        assert(fileType == FileType.Archive)
    }
  }

  test("test binary extensions") {
    """a ai beam bin chm class com dat dbmdl dcr dir dll dxr dms doc dot exe hi
      |hlp indd lnk mdb mo nib o obj odt pdb ppt psd pyc pyo qxd so swf sys vsd
      |xls xlt""".stripMargin.split("\\s+").foreach {
      ext =>
        val binFile = new File("binfile."+ext)
        println("binFile: "+binFile)
        assert(FileTypes.isBinaryFile(binFile))
        assert(!FileTypes.isArchiveFile(binFile))
        assert(FileTypes.isSearchableFile(binFile))
        assert(!FileTypes.isTextFile(binFile))
        assert(!FileTypes.isUnknownFile(binFile))
        val fileType = FileTypes.getFileType(binFile)
        assert(fileType == FileType.Binary)
    }
  }

  test("test code extensions") {
    """applejs as bash bat boo bsh c c++ cc cgi clj coffee cpp cs csh cxx el
      |erl es fs fx go groovy h h++ hh hpp hs java js js2 jsf json jsp jspf m
      |pas php php3 php4 php5 pl pm pxd pxi py pyw pyx rb rs sbt sc scala sh
      |tcl vb""".stripMargin.split("\\s+").foreach {
      ext =>
        val codeFile = new File("codefile."+ext)
        println("codeFile: "+codeFile)
        assert(!FileTypes.isBinaryFile(codeFile))
        assert(!FileTypes.isArchiveFile(codeFile))
        assert(FileTypes.isSearchableFile(codeFile))
        assert(FileTypes.isTextFile(codeFile))
        assert(!FileTypes.isUnknownFile(codeFile))
        val fileType = FileTypes.getFileType(codeFile)
        assert(fileType == FileType.Text)
    }
  }

  test("test nosearch extensions") {
    """aif aifc aiff au avi bmp cab db dmg eps gif icns ico idlk ief iso jpe
      |jpeg jpg m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu ogg pdf pict
      |png ps qt ra ram rm rpm snd suo tif tiff wav""".
      stripMargin.split("\\s+").foreach {
      ext =>
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
    """1 10 11 12 13 14 15 16 17 18 19 2 20 3 323 4 5 6 7 8 9 am app asc ascx
      |asm asp aspx bib brf cfg cls cmd cnt conf css csv ctl d dbml dbschema
      |ddl dep dfm diff disco dlg dof dpr drl dsp dsw dtd elt ent env etx exp
      |feature fls gcd hql hs htc htm html hxx ics icz iml in inc ini ipr iws
      |jad jam jql layout lhs log ltx mak mako manifest map master md mf mht
      |mml moc mod mxml p patch plist pm po pot properties pt rc rc2 rdf rex
      |rtf rtx scc sct sfv sgm sgml sht shtm shtml sln smi smil spec sqc sql st
      |str strings sty suml sxw t tex text tk tld tm tmx tsv txt ui uls uml url
      |user vbs vcf vcs vm vrml vssscc vxml wbxml webinfo wml wmls wrl wsc wsd
      |wsdd xlf yaml yml""".stripMargin.split("\\s+").foreach {
      ext =>
        val textFile = new File("textFile."+ext)
        println("textFile: "+textFile)
        assert(!FileTypes.isBinaryFile(textFile))
        assert(!FileTypes.isArchiveFile(textFile))
        assert(FileTypes.isSearchableFile(textFile))
        assert(FileTypes.isTextFile(textFile))
        assert(!FileTypes.isUnknownFile(textFile))
        val fileType = FileTypes.getFileType(textFile)
        assert(fileType == FileType.Text)
    }
  }

  test("test unknown extensions") {
    """adm aps cli clw def df2 ncb nt nt2 orig pc plg roff sun texinfo tr xwd""".
      split("\\s+").foreach {
      ext =>
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
      |fsproj fxml jhm jnlp kml pom potx ppsx pptx qrc rdf resx rng rss
      |settings sldx stc std sti stw svg svgz sxc sxd sxg sxi sxm sxw tld
      |vbproj vcproj vdproj wadl wsdd wsdl x3d xaml xhtml xht xjb xlsx xltx
      |xml xsd xsl xslt xspf xul""".stripMargin.split("\\s+").foreach {
      ext =>
        val xmlFile = new File("xmlfile."+ext)
        println("xmlFile: "+xmlFile)
        assert(!FileTypes.isBinaryFile(xmlFile))
        assert(!FileTypes.isArchiveFile(xmlFile))
        assert(FileTypes.isSearchableFile(xmlFile))
        assert(FileTypes.isTextFile(xmlFile))
        assert(!FileTypes.isUnknownFile(xmlFile))
        val fileType = FileTypes.getFileType(xmlFile)
        assert(fileType == FileType.Text)
    }
  }
}
