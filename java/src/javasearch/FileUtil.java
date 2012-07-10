/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class FileUtil {

	private static Set<String> _binaryExtensions =
		new HashSet<String>(
			Arrays.asList(
				("ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot " +
				 "exe hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys " +
				 "vsd xls xlsx xlt").split("\\s+")
			)
		);
	private static Set<String> _compressedExtensions =
		new HashSet<String>(
			Arrays.asList(
				("bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz war " +
				 "zip Z").split("\\s+")
			)
		);
	private static Set<String> _noSearchExtensions =
		new HashSet<String>(
			Arrays.asList(
				("aif aifc aiff au avi bmp cab dat db dmg eps gif ico idlk ief " +
				 "iso jpe jpeg jpg m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg " +
				 "mxu ogg pdf pict png ps qt ra ram rm rpm scc snd suo tif tiff " +
				 "wav").split("\\s+")
			)
		);
	private static Set<String> _textExtensions =
		new HashSet<String>(
			Arrays.asList(
				("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 " +
				 "am app as asc ascx asm asp aspx bash bat bdsproj bsh c cc cfg " +
				 "clj cls cmd cnt conf config cpp cs csh cshtml csproj css csv " +
				 "ctl dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp " +
				 "dsw dtd env etx exp fls fs fsproj h hpp htm html ics iml in " +
				 "inc ini ipr iws java js jsp layout log mak map master mht mxml " +
				 "pas php php3 pl plist pm po properties py rb rc rc2 rdf resx " +
				 "rex rtf rtx scala scc sgm sgml sh sln smi smil spec sqc sql st " +
				 "str strings suml svg sxw t tcl tld tmx tsv txt url user vb " +
				 "vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml wbxml webinfo " +
				 "wml wmls wrl wsd wsdd wsdl xlf xml xsd xsl xslt").split("\\s+")
			)
		);
	private static Set<String> _unknownExtensions =
		new HashSet<String>(
			Arrays.asList(
				("adm aps cli clw def df2 ncb nt nt2 orig pc plg roff sun t tex " +
				 "texinfo tr xwd").split("\\s+")
			)
		);

	public FileUtil() {}

	public String getExtension(File f) {
		String ext = "";
		String fileName = f.getName();
		int lastIndex = fileName.lastIndexOf(".");
		if (lastIndex > 0 && fileName.length() > lastIndex)
			ext = fileName.substring(lastIndex + 1);
		return ext;
	}

	public boolean isBinaryFile(File f) {
		return this._binaryExtensions.contains(this.getExtension(f));
	}

	public boolean isCompressedFile(File f) {
		return this._compressedExtensions.contains(this.getExtension(f));
	}

	public boolean isTextFile(File f) {
		return this._textExtensions.contains(this.getExtension(f));
	}

	public boolean isUnknownFile(File f) {
		return this._unknownExtensions.contains(this.getExtension(f));
	}
}
