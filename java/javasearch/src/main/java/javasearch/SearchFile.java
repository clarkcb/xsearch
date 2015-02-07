package javasearch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class SearchFile {
    public static final String CONTAINER_SEPARATOR = "!";
    private List<String> containers;
    private String path;
    private String fileName;
    private FileType fileType;

    public SearchFile(final String path, final String fileName,
                      final FileType fileType) {
        this(new ArrayList<String>(), path, fileName, fileType);
    }

    public SearchFile(final List<String> containers, final String path,
                      final String fileName, FileType fileType) {
        this.containers = containers;
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
    }

    public final void addContainer(final String c) {
        containers.add(c);
    }

    public final List<String> getContainers() {
        return this.containers;
    }

    protected final void setContainers(final List<String> cs) {
        this.containers = cs;
    }

    public final String getPath() {
        return this.path;
    }

    protected final void setPath(final String p) {
        this.path = p;
    }

    public final String getFileName() {
        return this.fileName;
    }

    protected final void setFileName(final String f) {
        this.fileName = f;
    }

    public final FileType getFileType() {
        return this.fileType;
    }

    protected final void setFileType(final FileType ft) {
        this.fileType = ft;
    }

    public final File toFile() {
        File dir = new File(path);
        return new File(dir, fileName);
    }

    public final String toString() {
        StringBuilder sb = new StringBuilder();
        if (containers.size() > 0) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) sb.append(CONTAINER_SEPARATOR);
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
