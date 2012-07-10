################################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################
import os

class FileUtil:
    '''a file helper class'''

    # file types
    # for more info see /etc/httpd/mime.types
    NOSEARCH_EXTS   = '''aif aifc aiff au avi bmp cab dmg eps gif
                         ico idlk ief iso jpe jpeg jpg
                         m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu
                         ogg pdf pict png ps qt ra ram rm rpm
                         scc snd suo tif tiff wav'''.split()
    BINARY_EXTS     = '''ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe
                         hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys
                         vsd xls xlsx xlt'''.split()
    COMPRESSED_EXTS = '''bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz
                         war zip Z'''.split()
    TEXT_EXTS       = '''1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
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
                         xlf xml xsd xsl xslt'''.split()
    UNKNOWN_EXTS    = '''adm aps cli clw dat db def df2 ncb nt nt2 orig
                         pc plg roff sun t tex texinfo tr xwd'''.split()
    SEARCHABLE_EXTS = BINARY_EXTS + COMPRESSED_EXTS + TEXT_EXTS

    def get_extension(self, filename):
        '''Returns the extension for a given filename, if any, else empty string'''
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()

    def is_binary_file(self, f):
        '''Return true if file is of a (known) searchable binary file type'''
        return (self.get_extension(f) in self.BINARY_EXTS)

    def is_compressed_file(self, f):
        '''Return true if file is of a (known) compressed file type'''
        return (self.get_extension(f) in self.COMPRESSED_EXTS)

    def is_searchable_file(self, f):
        '''Return true if file is of a (known) searchable type'''
        return (self.get_extension(f) in self.SEARCHABLE_EXTS)

    def is_text_file(self, f):
        '''Return true if file is of a (known) text file type'''
        return (self.get_extension(f) in self.TEXT_EXTS)

