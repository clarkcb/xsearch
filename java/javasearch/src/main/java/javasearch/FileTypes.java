package javasearch;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

public class FileTypes {
    private static final String fileTypesXmlPath = "/filetypes.xml";
    private Map<String, Set<String>> fileTypeMap;

    private Map<String,Set<String>> getFileTypeMap() {
        Map<String, Set<String>> fileTypeMap = new HashMap<String, Set<String>>();
        InputStream fileTypesInputStream = getClass().getResourceAsStream(fileTypesXmlPath);
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(fileTypesInputStream);
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
            Set<String> allText = new HashSet<String>();
            allText.addAll(fileTypeMap.get("code"));
            allText.addAll(fileTypeMap.get("text"));
            allText.addAll(fileTypeMap.get("xml"));
            fileTypeMap.put("text", allText);
            Set<String> allSearchable = new HashSet<String>();
            allSearchable.addAll(fileTypeMap.get("archive"));
            allSearchable.addAll(fileTypeMap.get("binary"));
            allSearchable.addAll(fileTypeMap.get("text"));
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

    public FileTypes() {
        fileTypeMap = getFileTypeMap();
    }

    public FileType getFileType(File f) {
        if (isArchiveFile(f)) return FileType.ARCHIVE;
        if (isBinaryFile(f)) return FileType.BINARY;
        if (isTextFile(f)) return FileType.TEXT;
        return FileType.UNKNOWN;
    }

    public boolean isArchiveFile(File f) {
        return fileTypeMap.get("archive").contains(FileUtil.getExtension(f));
    }

    public boolean isBinaryFile(File f) {
        return fileTypeMap.get("binary").contains(FileUtil.getExtension(f));
    }

    public boolean isSearchableFile(File f) {
        return fileTypeMap.get("searchable").contains(FileUtil.getExtension(f));
    }

    public boolean isTextFile(File f) {
        return fileTypeMap.get("text").contains(FileUtil.getExtension(f));
    }

    public boolean isUnknownFile(File f) {
        return fileTypeMap.get("unknown").contains(FileUtil.getExtension(f)) ||
                !fileTypeMap.get("searchable").contains(FileUtil.getExtension(f));
    }

}
