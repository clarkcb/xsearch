################################################################################
#
# fileutil.rb
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################

class FileUtil
  def initialize
    set_exts
  end

  def set_exts
    @NOSEARCH_EXTS   = 'aif aifc aiff au avi bmp cab dmg eps gif
                        ico idlk ief iso jpe jpeg jpg
                        m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu
                        ogg pdf pict png ps qt ra ram rm rpm
                        scc snd suo tif tiff wav'.split(/\s+/)
    @BINARY_EXTS     = 'ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe
                        hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys
                        vsd xls xlsx xlt'.split(/\s+/)
    @COMPRESSED_EXTS = 'bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz
                        war zip Z'.split(/\s+/)
    @TEXT_EXTS       = '1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                        am app as asc ascx asm asp aspx bash bat bdsproj bsh
                        c cc cfg clj cls cmd cnt conf config cpp cs csh csproj css csv ctl
                        dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp dsw dtd
                        env etx exp fls fs fsproj h hpp htm html ics iml in inc ini ipr iws
                        java js jsp layout log mak map master mht mxml
                        pas php php3 pl plist pm po properties py
                        rb rc rc2 rdf resx rex rtf rtx
                        scala scc sgm sgml sh sln smi smil spec sqc sql st str strings
                        suml svg sxw
                        t tcl tld tmx tsv txt url user
                        vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml
                        wbxml webinfo wml wmls wrl wsd wsdd wsdl
                        xlf xml xsd xsl xslt'.split(/\s+/)
    @UNKNOWN_EXTS    = 'adm aps cli clw dat db def df2 ncb nt nt2 orig
                        pc plg roff sun t tex texinfo tr xwd'.split(/\s+/)
    @SEARCHABLE_EXTS = []
    @SEARCHABLE_EXTS.concat(@BINARY_EXTS)
    @SEARCHABLE_EXTS.concat(@COMPRESSED_EXTS)
    @SEARCHABLE_EXTS.concat(@TEXT_EXTS)
  end

  def get_extension(filename)
    #puts "get_extension(\"#{filename}\") called"
    ext = ''
    index = filename.rindex('.')
    if index and index < (filename.length - 1)
      ext = filename[index+1..filename.length].downcase
    end
    #puts "ext: #{ext}"
    ext
  end

  def is_binary_file(f)
    @BINARY_EXTS.include?(get_extension(f))
  end

  def is_compressed_file(f)
    @COMPRESSED_EXTS.include?(get_extension(f))
  end

  def is_searchable_file(f)
    @SEARCHABLE_EXTS.include?(get_extension(f))
  end

  def is_text_file(f)
    @TEXT_EXTS.include?(get_extension(f))
  end

end
