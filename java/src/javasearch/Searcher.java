/*******************************************************************************
Searcher

Class to perform the file search

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Searcher {

	private SearchSettings settings;
	private List<SearchResult> results;
	private FileUtil fileUtil;
	private Set<File> fileSet;

	public Searcher(SearchSettings settings) {
		this.settings = settings;
		this.results = new ArrayList<SearchResult>();
		this.fileUtil = new FileUtil();
		this.fileSet = new HashSet<File>();
	}
	
	public SearchSettings getSearchSettings() {
		return this.settings;
	}

	public List<SearchResult> getSearchResults() {
		return this.results;
	}

	private boolean matchesAnyPattern(String s, Set<Pattern> patternSet) {
		if (null == s)
			return false;
		for (Pattern p : patternSet) {
			Matcher m = p.matcher(s);
			if (m.find()) {
				return true;
			}
		}
		return false;
	}

	private boolean isTargetDirectory(File d) {
		if (this.settings.getDebug()) {
			System.out.println("Called isTargetDirectory(\"" + d.getPath() + "\")");
		}
		return ((this.settings.getInDirPatterns().isEmpty() ||
				 this.matchesAnyPattern(d.getParent(), this.settings.getInDirPatterns())) &&
        		(this.settings.getOutDirPatterns().isEmpty() ||
				!this.matchesAnyPattern(d.getParent(), this.settings.getOutDirPatterns())));
	}

	private boolean isTargetFile(File f) {
		if (this.settings.getDebug()) {
			System.out.println("Called isTargetFile(\"" + f.getName() + "\")");
		}
		String ext = this.fileUtil.getExtension(f);
		if (this.settings.getDebug()) {
			System.out.println("ext: " + ext);
		}
		return ((this.settings.getInExtensions().isEmpty() ||
				 this.settings.getInExtensions().contains(ext)) &&
				(this.settings.getOutExtensions().isEmpty() ||
				!this.settings.getOutExtensions().contains(ext)) &&
				(this.settings.getInFilePatterns().isEmpty() ||
				 this.matchesAnyPattern(f.getName(), this.settings.getInFilePatterns())) &&
				(this.settings.getOutFilePatterns().isEmpty() ||
				!this.matchesAnyPattern(f.getName(), this.settings.getOutFilePatterns())));
	}

	public List<File> getSearchFiles(File startPath) {
		if (this.settings.getDebug()) {
			System.out.println("Getting files to search under " + startPath.getPath());
		}
		List<File> searchDirs = new ArrayList<File>();
		List<File> searchFiles = new ArrayList<File>();
		File currentFiles[] = startPath.listFiles();
		for (File f : currentFiles) {
			if (f.isDirectory()) {
				if (this.isTargetDirectory(f)) {
					searchDirs.add(f);
				}
			} else {
				if (this.isTargetFile(f)) {
					searchFiles.add(f);
				}
			}
		}
		for (File d : searchDirs) {
			searchFiles.addAll(this.getSearchFiles(d));
		}
		return searchFiles;
	}

	public void search() {
		List<File> searchFiles = this.getSearchFiles(new File(this.settings.getStartPath()));
		if (this.settings.getVerbose() || this.settings.getDebug()) {
			System.out.println("\nFiles to be searched:");
			for (File f : searchFiles) {
				System.out.println(f.getPath());
			}
			System.out.println("");
		}
		for (File f : searchFiles) {
			this.searchFile(f);
		}
	}

	public void searchFile(File f) {
		if (this.settings.getVerbose() || this.settings.getDebug()) {
			System.out.println("Searching " + f.getPath());
		}
		if (this.fileUtil.isTextFile(f)) {
			this.searchTextFile(f);
		}
	}

	public void searchTextFile(File f) {
		if (this.settings.getVerbose() || this.settings.getDebug()) {
			System.out.println("Searching text file " + f.getPath());
		}
		try {
			BufferedReader br = new BufferedReader(new FileReader(f));
			try {
				String line = null;
				int lineNum = 0;
				while ((line = br.readLine()) != null) {
					lineNum++;
					for (Pattern p : this.settings.getSearchPatterns()) {
						Matcher m = p.matcher(line);
						if (m.find()) {
							SearchResult searchResult =
								new SearchResult(p, f, lineNum, line);
							this.addSearchResult(searchResult);
						}
					}
				}
			} catch (IOException e) {
				System.out.println(e.toString());
			} finally {
				br.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void addSearchResult(SearchResult searchResult) {
		//if (this.settings.getPrintResults())
		if (true)
			System.out.println(searchResult.toString());
		this.results.add(searchResult);
		this.fileSet.add(searchResult.getFile());
	}
}
