package javasearch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class SearchFile {
    public static String CONTAINER_SEPARATOR = "!";
    private List<String> containers;
    private String path;
    private String file;

    public SearchFile(String path, String file) {
        this(new ArrayList<String>(), path, file);
    }

    public SearchFile(List<String> containers, String path, String file) {
        this.containers = containers;
        this.path = path;
        this.file = file;
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

    public String getFile() {
        return this.file;
    }

    protected void setFile(String f) {
        this.file = f;
    }

    public File toFile() {
        File dir = new File(path);
        return new File(dir, file);
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
        sb.append(file);
        return sb.toString();
    }

}
