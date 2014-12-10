/*******************************************************************************
SearchSettings

Class to encapsulate search settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

public class SearchSettings {

	private String startPath;

	private Set<String> inExtensions;
	private Set<String> outExtensions;
	private Set<Pattern> inDirPatterns;
	private Set<Pattern> outDirPatterns;
	private Set<Pattern> inFilePatterns;
	private Set<Pattern> outFilePatterns;
	private Set<Pattern> inArchiveFilePatterns;
	private Set<Pattern> outArchiveFilePatterns;
	private Set<Pattern> inLinesAfterPatterns;
	private Set<Pattern> outLinesAfterPatterns;
	private Set<Pattern> inLinesBeforePatterns;
	private Set<Pattern> outLinesBeforePatterns;
	private Set<Pattern> linesAfterToPatterns;
	private Set<Pattern> linesAfterUntilPatterns;
	private Set<Pattern> searchPatterns;

	private boolean archivesOnly;
	private boolean caseSensitive;
	private boolean debug;
	private boolean doTiming;
    private boolean excludeHidden;
    private boolean firstMatch;
	private int linesAfter;
	private int linesBefore;
	private boolean listDirs;
	private boolean listFiles;
	private boolean listLines;
	private int maxLineLength;
	private boolean multiLineSearch;
	private boolean printResults;
	private boolean printUsage;
	private boolean printVersion;
	private boolean recursive;
	private boolean searchArchives;
	private boolean uniqueLines;
	private boolean verbose;

	Set<Pattern> DEFAULT_OUT_DIRPATTERNS;
	Set<Pattern> DEFAULT_OUT_FILEPATTERNS;

	public SearchSettings() {
		this.inExtensions = new HashSet<String>();
		this.outExtensions = new HashSet<String>();
		this.inDirPatterns = new HashSet<Pattern>();
        DEFAULT_OUT_DIRPATTERNS = new HashSet<Pattern>(
            Arrays.asList(
                Pattern.compile("\\bCVS$"),
                Pattern.compile("\\.git$"),
                Pattern.compile("\\.svn$")
            )
        );
        this.outDirPatterns = DEFAULT_OUT_DIRPATTERNS;
		this.inFilePatterns = new HashSet<Pattern>();
        DEFAULT_OUT_FILEPATTERNS = new HashSet<Pattern>(
            Arrays.asList(
                Pattern.compile("^\\.DS_Store$")
            )
        );
        this.outFilePatterns = DEFAULT_OUT_FILEPATTERNS;
		this.inArchiveFilePatterns = new HashSet<Pattern>();
		this.outArchiveFilePatterns = new HashSet<Pattern>();
		this.inLinesAfterPatterns = new HashSet<Pattern>();
		this.outLinesAfterPatterns = new HashSet<Pattern>();
		this.inLinesBeforePatterns = new HashSet<Pattern>();
		this.outLinesBeforePatterns = new HashSet<Pattern>();
		this.linesAfterToPatterns = new HashSet<Pattern>();
		this.linesAfterUntilPatterns = new HashSet<Pattern>();

		this.searchPatterns = new HashSet<Pattern>();

		// set the defaults
		this.archivesOnly = false;
        this.caseSensitive = true;
        this.debug = false;
		this.doTiming = false;
        this.firstMatch = false;
        this.excludeHidden = true;
		this.linesAfter = 0;
		this.linesBefore = 0;
		this.listDirs = false;
		this.listFiles = false;
		this.listLines = false;
		this.maxLineLength = 100;
		this.multiLineSearch = false;
		this.printResults = false;
		this.printUsage = false;
		this.printVersion = false;
		this.recursive = true;
		this.searchArchives = false;
		this.uniqueLines = false;
		this.verbose = false;
	}

	public String getStartPath() {
		return this.startPath;
	}

	public void setStartPath(String startPath) {
		this.startPath = startPath;
	}

	public boolean getArchivesOnly() {
		return this.archivesOnly;
	}

	public void setArchivesOnly(boolean archivesOnly) {
		this.archivesOnly = archivesOnly;
	}

	public boolean getDebug() {
		return this.debug;
	}

	public void setDebug(boolean debug) {
		this.debug = debug;
	}

	public boolean getDoTiming() {
		return this.doTiming;
	}

	public void setDoTiming(boolean doTiming) {
		this.doTiming = doTiming;
	}

	public boolean getFirstMatch() {
		return this.firstMatch;
	}

	public void setFirstMatch(boolean firstMatch) {
		this.firstMatch = firstMatch;
	}

	public boolean getExcludeHidden() {
		return this.excludeHidden;
	}

	public void setExcludeHidden(boolean excludeHidden) {
		this.excludeHidden = excludeHidden;
	}

	public int getLinesAfter() {
		return this.linesAfter;
	}

	public void setLinesAfter(int linesAfter) {
		this.linesAfter = linesAfter;
	}

	public int getLinesBefore() {
		return this.linesBefore;
	}

	public void setLinesBefore(int linesBefore) {
		this.linesBefore = linesBefore;
	}

	public boolean getListDirs() {
		return this.listDirs;
	}

	public void setListDirs(boolean listDirs) {
		this.listDirs = listDirs;
	}

	public boolean getListFiles() {
		return this.listFiles;
	}

	public void setListFiles(boolean listFiles) {
		this.listFiles = listFiles;
	}

	public boolean getListLines() {
		return this.listLines;
	}

	public void setListLines(boolean listLines) {
		this.listLines = listLines;
	}

	public int getMaxLineLength() {
		return this.maxLineLength;
	}

	public void setMaxLineLength(int maxLineLength) {
		this.maxLineLength = maxLineLength;
	}

	public boolean getMultiLineSearch() {
		return this.multiLineSearch;
	}

	public void setMultiLineSearch(boolean multiLineSearch) {
		this.multiLineSearch = multiLineSearch;
	}

	public boolean getPrintResults() {
		return this.printResults;
	}

	public void setPrintResults(boolean printResults) {
		this.printResults = printResults;
	}

	public boolean getPrintUsage() {
		return this.printUsage;
	}

	public void setPrintUsage(boolean printUsage) {
		this.printUsage = printUsage;
	}

	public boolean getPrintVersion() {
		return this.printVersion;
	}

	public void setPrintVersion(boolean printVersion) {
		this.printVersion = printVersion;
	}

	public boolean getRecursive() {
		return this.recursive;
	}

	public void setRecursive(boolean recursive) {
		this.recursive = recursive;
	}

	public boolean getSearchArchives() {
		return this.searchArchives;
	}

	public void setSearchArchives(boolean searchArchives) {
		this.searchArchives = searchArchives;
	}

	public boolean getUniqueLines() {
		return this.uniqueLines;
	}

	public void setUniqueLines(boolean uniqueLines) {
		this.uniqueLines = uniqueLines;
	}

	public boolean getVerbose() {
		return this.verbose;
	}

	public void setVerbose(boolean verbose) {
		this.verbose = verbose;
	}

	private static void addExtension(Set<String> set, String ext) {
		set.add(ext.toLowerCase());
	}

	public Set<String> getInExtensions() {
		return this.inExtensions;
	}

	public void addInExtension(String ext) {
		addExtension(this.inExtensions, ext);
	}

	public Set<String> getOutExtensions() {
		return this.outExtensions;
	}

	public void addOutExtension(String ext) {
		addExtension(this.outExtensions, ext);
	}

	private static void addPattern(Set<Pattern> set, String pattern) {
		set.add(Pattern.compile(pattern));
	}

	public Set<Pattern> getInDirPatterns() {
		return this.inDirPatterns;
	}

	public void addInDirPattern(String pattern) {
		addPattern(this.inDirPatterns, pattern);
	}

	public Set<Pattern> getOutDirPatterns() {
		return this.outDirPatterns;
	}

	public void addOutDirPattern(String pattern) {
		addPattern(this.outDirPatterns, pattern);
	}

	public Set<Pattern> getInFilePatterns() {
		return this.inFilePatterns;
	}

	public void addInFilePattern(String pattern) {
		addPattern(this.inFilePatterns, pattern);
	}

	public Set<Pattern> getOutFilePatterns() {
		return this.outFilePatterns;
	}

	public void addOutFilePattern(String pattern) {
		addPattern(this.outFilePatterns, pattern);
	}

	public Set<Pattern> getInArchiveFilePatterns() {
		return this.inArchiveFilePatterns;
	}

	public void addInArchiveFilePattern(String pattern) {
		addPattern(this.inArchiveFilePatterns, pattern);
	}

	public Set<Pattern> getOutArchiveFilePatterns() {
		return this.outArchiveFilePatterns;
	}

	public void addOutArchiveFilePattern(String pattern) {
		addPattern(this.outArchiveFilePatterns, pattern);
	}

    public Set<Pattern> getInLinesAfterPatterns() {
        return this.inLinesAfterPatterns;
    }

    public void addInLinesAfterPattern(String pattern) {
        addPattern(this.inLinesAfterPatterns, pattern);
    }

    public Set<Pattern> getOutLinesAfterPatterns() {
        return this.outLinesAfterPatterns;
    }

    public void addOutLinesAfterPattern(String pattern) {
        addPattern(this.outLinesAfterPatterns, pattern);
    }

    public Set<Pattern> getInLinesBeforePatterns() {
        return this.inLinesBeforePatterns;
    }

    public void addInLinesBeforePattern(String pattern) {
        addPattern(this.inLinesBeforePatterns, pattern);
    }

    public Set<Pattern> getOutLinesBeforePatterns() {
        return this.outLinesBeforePatterns;
    }

    public void addOutLinesBeforePattern(String pattern) {
        addPattern(this.outLinesBeforePatterns, pattern);
    }

    public Set<Pattern> getLinesAfterToPatterns() {
        return this.linesAfterToPatterns;
    }

    public void addLinesAfterToPattern(String pattern) {
        addPattern(this.linesAfterToPatterns, pattern);
    }


    public Set<Pattern> getLinesAfterUntilPatterns() {
        return this.linesAfterUntilPatterns;
    }

    public void addLinesAfterUntilPattern(String pattern) {
        addPattern(this.linesAfterUntilPatterns, pattern);
    }

    public void addSearchPattern(String pattern) {
		addPattern(this.searchPatterns, pattern);
	}

	public Set<Pattern> getSearchPatterns() {
		return this.searchPatterns;
	}

	private static String stringSetToString(Set<String> set) {
		StringBuilder sb = new StringBuilder("[");
		int elemCount = 0;
		for (String s : set) {
			if (elemCount > 0)
				sb.append(", ");
			sb.append("\"").append(s).append("\"");
			elemCount++;
		}
		sb.append("]");
		return sb.toString();
	}

	private static String patternSetToString(Set<Pattern> set) {
		StringBuilder sb = new StringBuilder("[");
		int elemCount = 0;
		for (Pattern p : set) {
			if (elemCount > 0)
				sb.append(", ");
			sb.append("\"").append(p.toString()).append("\"");
			elemCount++;
		}
		sb.append("]");
		return sb.toString();
	}

	public String toString() {
        return "SearchSettings(" + "startPath: \"" + this.startPath + "\"" +
                ", inExtensions: " + stringSetToString(this.inExtensions) +
                ", outExtensions: " + stringSetToString(this.outExtensions) +
                ", inDirPatterns: " + patternSetToString(this.inDirPatterns) +
                ", outDirPatterns: " + patternSetToString(this.outDirPatterns) +
                ", inFilePatterns: " + patternSetToString(this.inFilePatterns) +
                ", outFilePatterns: " + patternSetToString(this.outFilePatterns) +
                ", searchPatterns: " + patternSetToString(this.searchPatterns) +
                ", archivesOnly: " + this.archivesOnly +
                ", debug: " + this.debug +
                ", doTiming: " + this.doTiming +
                ", excludeHidden: " + this.excludeHidden +
                ", firstMatch: " + this.firstMatch +
                ", listDirs: " + this.listDirs +
                ", listFiles: " + this.listFiles +
                ", listLines: " + this.listLines +
                ", maxLineLength: " + this.maxLineLength +
                ", multiLineSearch: " + this.multiLineSearch +
                ", printResults: " + this.printResults +
                ", printUsage: " + this.printUsage +
                ", printVersion: " + this.printVersion +
                ", recursive: " + this.recursive +
                ", searchArchives: " + this.searchArchives +
                ", uniqueLines: " + this.uniqueLines +
                ", verbose: " + this.verbose +
                ")";
	}
}
