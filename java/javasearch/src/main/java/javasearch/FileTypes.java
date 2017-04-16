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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FileTypes {
    private static final String FILETYPESXMLPATH = "/filetypes.xml";
    private static final String archive = "archive";
    private static final String binary = "binary";
    private static final String code = "code";
    private static final String searchable = "searchable";
    private static final String text = "text";
    private static final String unknown = "unknown";
    private static final String xml = "xml";
    private Map<String, Set<String>> fileTypeMap;

    private Map<String, Set<String>> getFileTypeMap() {
        int fileTypeKeys = 8;
        Map<String, Set<String>> ftMap = new HashMap<>(fileTypeKeys);
        InputStream fileTypesInputStream = getClass().
                getResourceAsStream(FILETYPESXMLPATH);
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(fileTypesInputStream);
            doc.getDocumentElement().normalize();
            NodeList filetypeNodes = doc.getElementsByTagName("filetype");
            for (int i = 0; i < filetypeNodes.getLength(); i++) {
                Node fileTypeNode = filetypeNodes.item(i);
                String name = fileTypeNode.getAttributes().getNamedItem("name").
                        getNodeValue();
                Node extNode = ((Element) fileTypeNode).
                        getElementsByTagName("extensions").item(0);
                String extensions = extNode.getChildNodes().item(0).
                        getNodeValue();
                Set<String> extSet = new HashSet<>(Arrays.asList(extensions.
                        split("\\s+")));
                ftMap.put(name, extSet);
            }
            Set<String> allText = new HashSet<>();
            allText.addAll(ftMap.get(code));
            allText.addAll(ftMap.get(text));
            allText.addAll(ftMap.get(xml));
            ftMap.put(text, allText);
            Set<String> allSearchable = new HashSet<>();
            allSearchable.addAll(ftMap.get(archive));
            allSearchable.addAll(ftMap.get(binary));
            allSearchable.addAll(ftMap.get(text));
            ftMap.put(searchable, allSearchable);
        } catch (ParserConfigurationException | SAXException | IOException e) {
            e.printStackTrace();
        }

        return ftMap;
    }

    public FileTypes() {
        fileTypeMap = getFileTypeMap();
    }

    static FileType fromName(final String name) {
        String lname = name.toLowerCase();
        if (lname.equals(text)) return FileType.TEXT;
        if (lname.equals(binary)) return FileType.BINARY;
        if (lname.equals(archive)) return FileType.ARCHIVE;
        if (lname.equals(code)) return FileType.CODE;
        if (lname.equals(xml)) return FileType.XML;
        return FileType.UNKNOWN;
    }

    final FileType getFileType(final File f) {
        if (isTextFile(f)) return FileType.TEXT;
        if (isBinaryFile(f)) return FileType.BINARY;
        if (isArchiveFile(f)) return FileType.ARCHIVE;
        if (isCodeFile(f)) return FileType.CODE;
        if (isXmlFile(f)) return FileType.XML;
        return FileType.UNKNOWN;
    }

    final boolean isArchiveFile(final File f) {
        return fileTypeMap.get(archive).contains(FileUtil.getExtension(f));
    }

    final boolean isBinaryFile(final File f) {
        return fileTypeMap.get(binary).contains(FileUtil.getExtension(f));
    }

    public final boolean isCodeFile(final File f) {
        return fileTypeMap.get(code).contains(FileUtil.getExtension(f));
    }

    final boolean isSearchableFile(final File f) {
        return fileTypeMap.get(searchable).contains(FileUtil.getExtension(f));
    }

    final boolean isTextFile(final File f) {
        return fileTypeMap.get(text).contains(FileUtil.getExtension(f));
    }

    final boolean isUnknownFile(final File f) {
        return fileTypeMap.get(unknown).contains(FileUtil.getExtension(f))
                ||
                !fileTypeMap.get(searchable).contains(FileUtil.getExtension(f));
    }

    public final boolean isXmlFile(final File f) {
        return fileTypeMap.get(xml).contains(FileUtil.getExtension(f));
    }
}
