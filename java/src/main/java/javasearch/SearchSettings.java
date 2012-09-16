/*******************************************************************************
SearchSettings

Class to encapsulate search settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

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
	private Set<Pattern> searchPatterns;

	private boolean debug;
	private boolean doTiming;
	private boolean firstMatch;
	private boolean listFiles;
	private boolean listLines;
	private boolean printResults;
	private boolean printUsage;
	private boolean printVersion;
	private boolean searchCompressed;
	private boolean verbose;

	public SearchSettings() {
		this.inExtensions = new HashSet<String>();
		this.outExtensions = new HashSet<String>();
		this.inDirPatterns = new HashSet<Pattern>();
		this.outDirPatterns = new HashSet<Pattern>();
		this.inFilePatterns = new HashSet<Pattern>();
		this.outFilePatterns = new HashSet<Pattern>();
		this.searchPatterns = new HashSet<Pattern>();

		// set the defaults
		this.debug = false;
		this.doTiming = false;
		this.firstMatch = false;
		this.listFiles = false;
		this.listLines = false;
		this.printResults = false;
		this.printUsage = false;
		this.printVersion = false;
		this.searchCompressed = false;
		this.verbose = false;
	}

	public String getStartPath() {
		return this.startPath;
	}

	public void setStartPath(String startPath) {
		this.startPath = startPath;
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

	public boolean getSearchCompressed() {
		return this.searchCompressed;
	}

	public void setSearchCompressed(boolean searchCompressed) {
		this.searchCompressed = searchCompressed;
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

	public Set<String> getInExtensions()
	{
		return this.inExtensions;
	}

	public void addInExtension(String ext)
	{
		addExtension(this.inExtensions, ext);
	}

	public Set<String> getOutExtensions()
	{
		return this.outExtensions;
	}

	public void addOutExtension(String ext)
	{
		addExtension(this.outExtensions, ext);
	}

	private static void addPattern(Set<Pattern> set, String pattern) {
		set.add(Pattern.compile(pattern));
	}

	public Set<Pattern> getInDirPatterns()
	{
		return this.inDirPatterns;
	}

	public void addInDirPattern(String pattern) {
		addPattern(this.inDirPatterns, pattern);
	}

	public Set<Pattern> getOutDirPatterns()
	{
		return this.outDirPatterns;
	}

	public void addOutDirPattern(String pattern) {
		addPattern(this.outDirPatterns, pattern);
	}

	public Set<Pattern> getInFilePatterns()
	{
		return this.inFilePatterns;
	}

	public void addInFilePattern(String pattern) {
		addPattern(this.inFilePatterns, pattern);
	}

	public Set<Pattern> getOutFilePatterns()
	{
		return this.outFilePatterns;
	}

	public void addOutFilePattern(String pattern) {
		addPattern(this.outFilePatterns, pattern);
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
			sb.append("\"" + s + "\"");
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
			sb.append("\"" + p.toString() + "\"");
			elemCount++;
		}
		sb.append("]");
		return sb.toString();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder("SearchSettings(");
		sb.append("startPath: \"" + this.startPath + "\"");
		sb.append(", inExtensions: " + stringSetToString(this.inExtensions));
		sb.append(", outExtensions: " + stringSetToString(this.outExtensions));
		sb.append(", inDirPatterns: " + patternSetToString(this.inDirPatterns));
		sb.append(", outDirPatterns: " + patternSetToString(this.outDirPatterns));
		sb.append(", inFilePatterns: " + patternSetToString(this.inFilePatterns));
		sb.append(", outFilePatterns: " + patternSetToString(this.outFilePatterns));
		sb.append(", searchPatterns: " + patternSetToString(this.searchPatterns));
		sb.append(", debug: " + this.debug);
		sb.append(", doTiming: " + this.doTiming);
		sb.append(", firstMatch: " + this.firstMatch);
		sb.append(", listFiles: " + this.listFiles);
		sb.append(", listLines: " + this.listLines);
		sb.append(", printResults: " + this.printResults);
		sb.append(", printUsage: " + this.printUsage);
		sb.append(", printVersion: " + this.printVersion);
		sb.append(", searchCompressed: " + this.searchCompressed);
		sb.append(", verbose: " + this.verbose);
		sb.append(")");
		return sb.toString();
	}
}
