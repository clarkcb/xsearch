/*******************************************************************************
Searcher

Class to perform the file search

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Searcher {

	private SearchSettings settings;
	private List<SearchResult> results;
	private FileUtil fileUtil;
	private Set<File> fileSet;
	private Map<String,Long> timers;

	public Searcher(SearchSettings settings) {
		this.settings = settings;
		this.results = new ArrayList<SearchResult>();
		this.fileUtil = new FileUtil();
		this.fileSet = new HashSet<File>();
		this.timers = new HashMap<String,Long>();
        validateSettings();
	}

    private void validateSettings() {
        if (settings.getStartPath() == null || settings.getStartPath().equals(""))
            throw new IllegalArgumentException("Missing startpath");
        if ((settings.getSearchPatterns().size() < 1))
            throw new IllegalArgumentException("No search patterns defined");
    }

	private boolean anyMatchesAnyPattern(List<String> sList, Set<Pattern> patternSet) {
		for (String s : sList) {
			if (matchesAnyPattern(s, patternSet)) return true;
		}
		return false;
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

	private boolean isSearchDir(File d) {
        List<String> pathElems = Arrays.asList(d.toString().split(File.separator));
        if (settings.getExcludeHidden()) {
            for (String p : pathElems) {
                if (p.equals(".") || p.equals("..")) continue;
                if (p.startsWith(".")) return false;
            }
        }
		return (settings.getInDirPatterns().isEmpty() ||
                anyMatchesAnyPattern(pathElems, settings.getInDirPatterns()))
                &&
                (settings.getOutDirPatterns().isEmpty() ||
                 !anyMatchesAnyPattern(pathElems, settings.getOutDirPatterns()));
	}

	public List<File> getSearchDirs(File startPath) {
		if (settings.getDebug()) {
			System.out.println("Getting files to search under " +
				startPath.getPath());
		}
		List<File> searchDirs = new ArrayList<File>();
        if (isSearchDir(startPath)) {
            searchDirs.add(startPath);
        }
		if (settings.getRecursive()) {
			searchDirs.addAll(recGetSearchDirs(startPath));
		}
		return searchDirs;
	}

	private List<File> getSubDirs(File dir) {
		List<File> subDirs = new ArrayList<File>();
		File dirFiles[] = dir.listFiles();
        if (dirFiles != null) {
            for (File f : dirFiles) {
                if (f.isDirectory()) {
                    subDirs.add(f);
                }
            }
        }
        return subDirs;
    }

	private List<File> recGetSearchDirs(File dir) {
		List<File> searchDirs = new ArrayList<File>();
		List<File> subDirs = getSubDirs(dir);

        for (File f : subDirs) {
            if (isSearchDir(f)) {
                searchDirs.add(f);
            }
        }
		for (File d : subDirs) {
			searchDirs.addAll(recGetSearchDirs(d));
		}
		return searchDirs;
	}

	private boolean isSearchFile(File f) {
        if (f.getName().startsWith(".") && settings.getExcludeHidden()) {
            return false;
        }
		String ext = fileUtil.getExtension(f);
		return (settings.getInExtensions().isEmpty() ||
                settings.getInExtensions().contains(ext))
               &&
               (settings.getOutExtensions().isEmpty() ||
                !settings.getOutExtensions().contains(ext))
               &&
               (settings.getInFilePatterns().isEmpty() ||
                matchesAnyPattern(f.getName(), settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty() ||
                !matchesAnyPattern(f.getName(), settings.getOutFilePatterns()));
	}

	private boolean isArchiveSearchFile(File f) {
        if (f.getName().startsWith(".") && settings.getExcludeHidden()) {
            return false;
        }
		String ext = fileUtil.getExtension(f);
		return (settings.getInArchiveExtensions().isEmpty() ||
                settings.getInArchiveExtensions().contains(ext))
               &&
               (settings.getOutArchiveExtensions().isEmpty() ||
                !settings.getOutArchiveExtensions().contains(ext))
               &&
               (settings.getInArchiveFilePatterns().isEmpty() ||
                matchesAnyPattern(f.getName(), settings.getInArchiveFilePatterns()))
               &&
               (settings.getOutArchiveFilePatterns().isEmpty() ||
                !matchesAnyPattern(f.getName(), settings.getOutArchiveFilePatterns()));
	}

	public List<File> getSearchFilesForDir(File dir) {
		if (settings.getDebug()) {
			System.out.println("Getting files to search under " + dir);
		}
		List<File> searchFiles = new ArrayList<File>();
		File currentFiles[] = dir.listFiles();
        if (currentFiles != null) {
            for (File f : currentFiles) {
                if (!f.isDirectory()) {
                    if (fileUtil.isArchiveFile(f) &&
							settings.getSearchArchives() &&
							isArchiveSearchFile(f)) {
                        searchFiles.add(f);
                    } else if (!settings.getArchivesOnly() && isSearchFile(f)) {
                        searchFiles.add(f);
                    }
                }
            }
        }
        return searchFiles;
	}

	public List<File> getSearchFiles(List<File> searchDirs) {
		List<File> searchFiles = new ArrayList<File>();
		for (File d : searchDirs) {
			searchFiles.addAll(getSearchFilesForDir(d));
		}
		return searchFiles;
	}

	public void addTimer(String name, String action) {
		timers.put(name+":"+action, System.currentTimeMillis());
	}

	public void startTimer(String name) {
		addTimer(name, "start");
	}

    public void stopTimer(String name) {
		addTimer(name, "stop");
    }

    public long getElapsed(String name) {
        long startTime = this.timers.get(name+":start");
        long stopTime = this.timers.get(name+":stop");
        return stopTime - startTime;
    }

    public void printElapsed(String name) {
        System.out.println(String.format("Elapsed time for \"%s\": %d milliseconds",
				name, getElapsed(name)));
    }

    public void search() {

		// get the search directories
		if (settings.getDoTiming())
			startTimer("getSearchDirs");
		List<File> searchDirs = getSearchDirs(new File(settings.getStartPath()));
		if (settings.getDoTiming()) {
            stopTimer("getSearchDirs");
            printElapsed("getSearchDirs");
        }
		if (settings.getVerbose()) {
			String hdr = String.format("\nDirectories to be searched (%d):",
					searchDirs.size());
			System.out.println(hdr);
			for (File d : searchDirs) {
				System.out.println(d.getPath());
			}
			System.out.println("");
		}

		// get the search files in the search directories
		if (settings.getDoTiming())
			startTimer("getSearchFiles");
		List<File> searchFiles = getSearchFiles(searchDirs);
		if (settings.getDoTiming()) {
            stopTimer("getSearchFiles");
            printElapsed("getSearchFiles");
        }
		if (settings.getVerbose()) {
			String hdr = String.format("\nFiles to be searched (%d):",
					searchFiles.size());
			System.out.println(hdr);
			for (File f : searchFiles) {
				System.out.println(f.getPath());
			}
			System.out.println("");
		}

		// search the files
		if (settings.getDoTiming())
			startTimer("searchFiles");
		for (File f : searchFiles) {
			searchFile(f);
		}
		if (settings.getDoTiming()) {
            stopTimer("searchFiles");
            printElapsed("searchFiles");
        }
        if (settings.getVerbose()) {
			System.out.println("\nFile search complete.\n");
        }
	}

	public void searchFile(File f) {
		if (settings.getVerbose()) {
			System.out.println("Searching " + f.getPath());
		}
		if (fileUtil.isTextFile(f)) {
			searchTextFile(f);
		} else if (fileUtil.isBinaryFile(f)) {
			searchBinaryFile(f);
		}
	}

	public void searchTextFile(File f) {
		if (settings.getVerbose()) {
			System.out.println("Searching text file " + f.getPath());
		}
		if (settings.getMultiLineSearch()) {
			searchTextFileContents(f);
		} else {
			searchTextFileLines(f);
		}
	}

	public void searchTextFileContents(File f) {
		try {
			Scanner scanner = new Scanner(f, "ISO8859-1").useDelimiter("\\Z");
			try {
				String content = scanner.next();
				List<SearchResult> results = searchMultiLineString(content);
				for (SearchResult r : results) {
					r.setFile(f);
					addSearchResult(r);
				}
			} catch (NoSuchElementException e) {
				System.out.println(e.toString());
			} catch (IllegalStateException e) {
				System.out.println(e.toString());
			} finally {
				scanner.close();
			}
		} catch (IOException e) {
			System.out.println(e.toString());
		}
	}

	private List<Integer> getNewLineIndices(String s) {
		List<Integer> newlineIndices = new ArrayList<Integer>();
		newlineIndices.add(0);
		for (int i=1; i < s.length(); i++) {
			if (s.charAt(i) == '\n')
				newlineIndices.add(i);
		}
		return newlineIndices;
	}

	private List<Integer> getStartLineIndices(String s) {
		List<Integer> newLineIndices = getNewLineIndices(s);
		List<Integer> startLineIndices = new ArrayList<Integer>();
		startLineIndices.add(0);
		for (Integer newLineIndex : newLineIndices) {
			startLineIndices.add(newLineIndex + 1);
		}
		return startLineIndices;
	}

	private List<Integer> getLessThan(int val, List<Integer> vals) {
		List<Integer> lessThans = new ArrayList<Integer>();
		for (Integer v : vals) {
			if (v < val)
				lessThans.add(v);
		}
		return lessThans;
	}

	private int getMax(List<Integer> vals) {
		int maxVal = 0;
		for (Integer v : vals) {
			if (v > maxVal)
				maxVal = v;
		}
		return maxVal;
	}

	private List<Integer> getGreaterThan(int val, List<Integer> vals) {
		List<Integer> greaterThans = new ArrayList<Integer>();
		for (Integer v : vals) {
			if (v > val)
				greaterThans.add(v);
		}
		return greaterThans;
	}

	private int getMin(List<Integer> vals) {
		int minVal = 0;
		if (vals.size() > 0) {
			minVal = vals.get(0);
			for (Integer v : vals) {
				if (v < minVal)
					minVal = v;
			}
		}
		return minVal;
	}

	public List<SearchResult> searchMultiLineString(String s) {
		Map<Pattern,Integer> patternMatches = new HashMap<Pattern,Integer>();
		List<SearchResult> results = new ArrayList<SearchResult>();
		List<Integer> startLineIndices = getStartLineIndices(s);
		for (Pattern p : settings.getSearchPatterns()) {
			Matcher m = p.matcher(s);
			boolean found = m.find();
			while (found) {
				if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
					found = false;
				} else {
					List<Integer> lessThan = getLessThan(m.start(), startLineIndices);
					int lineNum = lessThan.size() - 1;
					int startLineIndex = getMax(lessThan);
					List<Integer> greaterThan = getGreaterThan(m.start(), startLineIndices);
					int endLineIndex = getMin(greaterThan);
					String line = s.substring(startLineIndex, endLineIndex);
					SearchResult searchResult = new SearchResult(p, null,
							lineNum, m.start() - startLineIndex, m.end() - startLineIndex, line);
					results.add(searchResult);
					patternMatches.put(p, 1);
					found = m.find(m.end());
				}
			}
		}
		return results;
	}

	public void searchTextFileLines(File f) {
		LineIterator it = null;
		try {
			it = FileUtils.lineIterator(f, "ISO8859-1");
			List<SearchResult> results = searchStringIterator(it);
			for (SearchResult r : results) {
				r.setFile(f);
				addSearchResult(r);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (it != null) it.close();
		}
	}

	public List<SearchResult> searchStringIterator(Iterator<String> it) {
		Map<Pattern,Integer> patternMatches = new HashMap<Pattern,Integer>();
		List<SearchResult> results = new ArrayList<SearchResult>();
		String line;
		int lineNum = 0;
		while (it.hasNext()) {
			line = it.next();
			lineNum++;
			for (Pattern p : settings.getSearchPatterns()) {
				Matcher m = p.matcher(line);
				boolean found = m.find();
				while (found) {
					if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
						found = false;
					} else {
						SearchResult searchResult = new SearchResult(p, null,
								lineNum, m.start(), m.end(), line);
						results.add(searchResult);
						patternMatches.put(p, 1);
						found = m.find(m.end());
					}
				}
			}
		}
		return results;
	}

	public void searchBinaryFile(File f) {
		if (settings.getVerbose()) {
			System.out.println("Searching binary file " + f.getPath());
		}
		try {
			Scanner scanner = new Scanner(f, "ISO8859-1").useDelimiter("\\Z");
			try {
				String content = scanner.next();

				for (Pattern p : settings.getSearchPatterns()) {
					Matcher m = p.matcher(content);
					if (m.find()) {
						SearchResult searchResult =
							new SearchResult(p, f, 0, 0, 0, "");
						addSearchResult(searchResult);
					}
				}
			} catch (NoSuchElementException e) {
				System.out.println(e.toString());
			} catch (IllegalStateException e) {
				System.out.println(e.toString());
			} finally {
				scanner.close();
			}
		} catch (IOException e) {
			System.out.println(e.toString());
		}
	}

	private void addSearchResult(SearchResult searchResult) {
		results.add(searchResult);
		fileSet.add(searchResult.getFile());
	}

	void printSearchResults() {
		System.out.println(String.format("Search results (%d):",
			results.size()));
		for (SearchResult r : results) {
			System.out.println(r.toString());
		}
	}

	public List<File> getMatchingDirs() {
		Set<File> dirSet = new HashSet<File>();
		for (File f : fileSet) {
			dirSet.add(f.getParentFile());
		}
		List<File> dirs = new ArrayList<File>(dirSet);
  		java.util.Collections.sort(dirs);
        return dirs;
	}

    public void printMatchingDirs() {
		List<File> dirs = getMatchingDirs();
		System.out.println(String.format("\nMatching directories (%d directories):",
			dirs.size()));
		for (File d : dirs) {
			System.out.println(d.getPath());
		}
	}

	public List<File> gettMatchingFiles() {
		List<File> files = new ArrayList<File>(fileSet);
  		java.util.Collections.sort(files);
        return files;
	}

    public void printMatchingFiles() {
		List<File> files = gettMatchingFiles();
		System.out.println(String.format("\nMatching files (%d files):",
			files.size()));
		for (File f : files) {
			System.out.println(f.getPath());
		}
	}

	public List<String> gettMatchingLines() {
		Set<String> lineSet = new HashSet<String>();
		for (SearchResult r : results) {
			lineSet.add(r.getLine().trim());
		}
		List<String> lines = new ArrayList<String>(lineSet);
		java.util.Collections.sort(lines);
        return lines;
	}

    public void printMatchingLines() {
		List<String> lines = gettMatchingLines();
		System.out.println(String.format("\nMatching lines (%d unique lines):",
			lines.size()));
		for (String line : lines) {
			System.out.println(line);
		}
	}
}
