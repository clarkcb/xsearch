/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

public class FileUtil {

	// TODO: move to config file
	private static final String fileTypePath = "/Users/cary/src/git/xsearch/shared/filetypes.xml";
    private Map<String, Set<String>> fileTypeMap;

    private Map<String,Set<String>> getFileTypeMap() {
        Map fileTypeMap = new HashMap<String, Set<String>>();

        File fileTypeFile = new File(fileTypePath);
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(fileTypeFile);
            doc.getDocumentElement().normalize();
            NodeList filetypeNodes = doc.getElementsByTagName("filetype");
            for (int i = 0; i < filetypeNodes.getLength(); i++) {
                Node fileTypeNode = filetypeNodes.item(i);
                String name = fileTypeNode.getAttributes().getNamedItem("name").getNodeValue();
                Node extNode = ((Element)fileTypeNode).getElementsByTagName("extensions").item(0);
                String extensions = extNode.getChildNodes().item(0).getNodeValue();
                Set<String> extSet = new HashSet<String>(Arrays.asList(extensions.split("\\s+")));
                fileTypeMap.put(name, extSet);
            }
            Set<String> allText = (Set<String>) fileTypeMap.get("text");
            allText.addAll((Set<String>) fileTypeMap.get("code"));
            allText.addAll((Set<String>) fileTypeMap.get("xml"));
            fileTypeMap.put("text", allText);
            Set<String> allSearchable = (Set<String>) fileTypeMap.get("binary");
            allSearchable.addAll((Set<String>) fileTypeMap.get("compressed"));
            allSearchable.addAll((Set<String>) fileTypeMap.get("text"));
            fileTypeMap.put("searchable", allSearchable);
        } catch (ParserConfigurationException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (SAXException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

        return fileTypeMap;
    }

    public FileUtil() {
        fileTypeMap = getFileTypeMap();
    }

	public String getExtension(File f) {
		String ext = "";
		String fileName = f.getName();
		int lastIndex = fileName.lastIndexOf(".");
		if (lastIndex > 0 && fileName.length() > lastIndex)
			ext = fileName.substring(lastIndex + 1);
		return ext;
	}

	public boolean isBinaryFile(File f) {
        return fileTypeMap.get("binary").contains(getExtension(f));
	}

	public boolean isCompressedFile(File f) {
        return fileTypeMap.get("compressed").contains(getExtension(f));
    }

	public boolean isSearchableFile(File f) {
        return fileTypeMap.get("searchable").contains(getExtension(f));
    }

	public boolean isTextFile(File f) {
        return fileTypeMap.get("text").contains(getExtension(f));
    }

	public boolean isUnknownFile(File f) {
        return fileTypeMap.get("unknown").contains(getExtension(f)) ||
               !fileTypeMap.get("searchable").contains(getExtension(f));
    }
}
