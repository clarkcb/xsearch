package javasearch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class SearchFile {
    public static String CONTAINER_SEPARATOR = "!";
    private List<String> containers;
    private String path;
    private String fileName;
    private FileType fileType;

    public SearchFile(String path, String fileName, FileType fileType) {
        this(new ArrayList<String>(), path, fileName, fileType);
    }

    public SearchFile(List<String> containers, String path, String fileName,
                      FileType fileType) {
        this.containers = containers;
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
    }

    public void addContainer(String c) {
        containers.add(c);
    }

    public List<String> getContainers() {
        return this.containers;
    }

    protected void setContainers(List<String> cs) {
        this.containers = cs;
    }

    public String getPath() {
        return this.path;
    }

    protected void setPath(String p) {
        this.path = p;
    }

    public String getFileName() {
        return this.fileName;
    }

    protected void setFileName(String f) {
        this.fileName = f;
    }

    public FileType getFileType() {
        return this.fileType;
    }

    protected void setFileType(FileType ft) {
        this.fileType = ft;
    }

    public File toFile() {
        File dir = new File(path);
        return new File(dir, fileName);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (containers.size() > 0) {
            for (int i=0; i < containers.size(); i++) {
                if (i >0) sb.append(CONTAINER_SEPARATOR);
                sb.append(containers.get(i));
            }
            sb.append(CONTAINER_SEPARATOR);
        }
        if (path != null && !path.equals("")) {
            sb.append(path).append(File.separator);
        }
        sb.append(fileName);
        return sb.toString();
    }

}
