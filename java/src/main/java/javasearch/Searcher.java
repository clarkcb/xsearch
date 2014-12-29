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
	private Set<SearchFile> searchFileSet;
	private Map<String,Long> timers;

	public Searcher(SearchSettings settings) {
		this.settings = settings;
		this.results = new ArrayList<SearchResult>();
		this.fileUtil = new FileUtil();
		this.searchFileSet = new HashSet<SearchFile>();
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

	public List<SearchFile> getSearchFilesForDir(File dir) {
		if (settings.getDebug()) {
			System.out.println("Getting files to search under " + dir);
		}
		List<SearchFile> searchFiles = new ArrayList<SearchFile>();
		File currentFiles[] = dir.listFiles();
        if (currentFiles != null) {
            for (File f : currentFiles) {
                if (!f.isDirectory()) {
                    if ((fileUtil.isArchiveFile(f) && settings.getSearchArchives() && isArchiveSearchFile(f))
						||
						(!settings.getArchivesOnly() && isSearchFile(f))) {
                        searchFiles.add(new SearchFile(f.getParent(), f.getName()));
                    }
                }
            }
        }
        return searchFiles;
	}

	public List<SearchFile> getSearchFiles(List<File> searchDirs) {
		List<SearchFile> searchFiles = new ArrayList<SearchFile>();
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
		List<SearchFile> searchFiles = getSearchFiles(searchDirs);
		if (settings.getDoTiming()) {
            stopTimer("getSearchFiles");
            printElapsed("getSearchFiles");
        }
		if (settings.getVerbose()) {
			String hdr = String.format("\nFiles to be searched (%d):",
					searchFiles.size());
			System.out.println(hdr);
			for (SearchFile sf : searchFiles) {
				System.out.println(sf);
			}
			System.out.println("");
		}

		// search the files
		if (settings.getDoTiming())
			startTimer("searchFiles");
		for (SearchFile sf : searchFiles) {
			searchFile(sf);
		}
		if (settings.getDoTiming()) {
            stopTimer("searchFiles");
            printElapsed("searchFiles");
        }
        if (settings.getVerbose()) {
			System.out.println("\nFile search complete.\n");
        }
	}

	public void searchFile(SearchFile sf) {
		if (settings.getVerbose()) {
			System.out.println("Searching " + sf.toString());
		}
		if (fileUtil.isTextFile(sf.toFile())) {
			searchTextFile(sf);
		} else if (fileUtil.isBinaryFile(sf.toFile())) {
			searchBinaryFile(sf);
		}
	}

	public void searchTextFile(SearchFile sf) {
		if (settings.getVerbose()) {
			System.out.println("Searching text file " + sf.toString());
		}
		if (settings.getMultiLineSearch()) {
			searchTextFileContents(sf);
		} else {
			searchTextFileLines(sf);
		}
	}

	public void searchTextFileContents(SearchFile sf) {
		try {
			Scanner scanner = new Scanner(sf.toFile(), "ISO8859-1").useDelimiter("\\Z");
			try {
				String content = scanner.next();
				List<SearchResult> results = searchMultiLineString(content);
				for (SearchResult r : results) {
					r.setSearchFile(sf);
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
		List<Integer> lessThan = new ArrayList<Integer>();
		for (Integer v : vals) {
			if (v < val)
				lessThan.add(v);
		}
		return lessThan;
	}

	private List<Integer> getLessThanOrEqualTo(int val, List<Integer> vals) {
		List<Integer> lessOrEq = new ArrayList<Integer>();
		for (Integer v : vals) {
			if (v <= val)
				lessOrEq.add(v);
		}
		return lessOrEq;
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
		if (!vals.isEmpty()) {
			minVal = vals.get(0);
			for (Integer v : vals) {
				if (v < minVal)
					minVal = v;
			}
		}
		return minVal;
	}

	public List<SearchResult> searchMultiLineString(String s) {
		List<SearchResult> results = new ArrayList<SearchResult>();
		for (Pattern p : settings.getSearchPatterns()) {
			for (SearchResult r : searchMultiLineStringForPattern(s, p)) {
				results.add(r);
			}
		}
		return results;
	}

	private <T> List<T> takeLeft(List<T> list, int count) {
		if (list.size() <= count)
			return list;
		List<T>	left = new ArrayList<T>();
		int lastIndex = 0;
		while (count > 0 && lastIndex < list.size()) {
			left.add(list.get(lastIndex));
			count--;
			lastIndex++;
		}
		return left;
	}

	private <T> List<T> takeRight(List<T> list, int count) {
		if (list.size() <= count)
			return list;
		List<T>	right = new ArrayList<T>();
		int lastIndex = list.size() - 1;
		while (count > 0 && lastIndex >= 0) {
			right.add(0, list.get(lastIndex));
			count--;
			lastIndex--;
		}
		return right;
	}

	private List<String> getLinesBeforeFromMultiLineString(int currentStartIndex,
														   List<Integer> startLineIndices,
														   String content) {
		List<String> linesBefore = new ArrayList<String>();
		if (settings.getLinesBefore() > 0 && currentStartIndex > 0) {
			List<Integer> lessThan = getLessThan(currentStartIndex, startLineIndices);
			List<Integer> lastElems = takeRight(lessThan, settings.getLinesBefore());
			int nextIndex=0;
			while (nextIndex < lastElems.size() - 1) {
				linesBefore.add(content.substring(lastElems.get(nextIndex),
						lastElems.get(nextIndex+1) - 1));
				nextIndex++;
			}
			linesBefore.add(content.substring(lastElems.get(lastElems.size() - 1),
					currentStartIndex - 1));
		}
		return linesBefore;
	}

	private List<String> getLinesAfterFromMultiLineString(int currentStartIndex,
														  List<Integer> startLineIndices,
														  String content) {
		List<String> linesAfter = new ArrayList<String>();
		if (settings.getLinesAfter() > 0) {
			List<Integer> greaterThan = getGreaterThan(currentStartIndex, startLineIndices);
			// add content length+1 so the last line end can be determined if necessary
			greaterThan.add(content.length() + 1);
			if (greaterThan.size() > 0) {
				List<Integer> firstElems = takeLeft(greaterThan, settings.getLinesAfter() + 1);
				int nextIndex=0;
				while (nextIndex < firstElems.size() - 1) {
					linesAfter.add(content.substring(firstElems.get(nextIndex),
							firstElems.get(nextIndex + 1) - 1));
					nextIndex++;
				}
			}
		}
		return linesAfter;
	}

	private List<SearchResult> searchMultiLineStringForPattern(String s, Pattern p) {
		Map<Pattern,Integer> patternMatches = new HashMap<Pattern,Integer>();
		List<SearchResult> results = new ArrayList<SearchResult>();
		List<Integer> startLineIndices = getStartLineIndices(s);
		Matcher m = p.matcher(s);
		boolean found = m.find();
		while (found) {
			if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
				found = false;
			} else {
				List<Integer> lessOrEq = getLessThanOrEqualTo(m.start(), startLineIndices);
				int lineNum = 1;
				if (lessOrEq.size() > 1) lineNum = lessOrEq.size();
				int startLineIndex = getMax(lessOrEq);
				List<Integer> greaterThan = getGreaterThan(m.start(), startLineIndices);
				int endLineIndex = getMin(greaterThan) - 1;
				String line = s.substring(startLineIndex, endLineIndex);
				List<String> linesBefore = getLinesBeforeFromMultiLineString(
						startLineIndex, startLineIndices, s);
				List<String> linesAfter = getLinesAfterFromMultiLineString(
						startLineIndex, startLineIndices, s);
				if ((settings.getLinesBefore() == 0 || linesBefore.isEmpty() ||
						linesBeforeMatch(linesBefore))
						&&
						(settings.getLinesAfter() == 0 || linesAfter.isEmpty() ||
								linesAfterMatch(linesAfter))) {
					SearchResult searchResult = new SearchResult(
							p,
							null,
							lineNum,
							m.start() - startLineIndex + 1,
							m.end() - startLineIndex + 1,
							line,
							linesBefore,
							linesAfter);
					results.add(searchResult);
					patternMatches.put(p, 1);
				}
				found = m.find(m.end());
			}
		}
		return results;
	}

	public void searchTextFileLines(SearchFile sf) {
		LineIterator it = null;
		try {
			it = FileUtils.lineIterator(sf.toFile(), "ISO8859-1");
			List<SearchResult> results = searchStringIterator(it);
			for (SearchResult r : results) {
				r.setSearchFile(sf);
				addSearchResult(r);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (it != null) it.close();
		}
	}

	private boolean linesMatch(List<String> lines, Set<Pattern> inPatterns,
							   Set<Pattern> outPatterns) {
		return (inPatterns.size() == 0 || anyMatchesAnyPattern(lines, inPatterns)
				&&
				(outPatterns.size() == 0 || !anyMatchesAnyPattern(lines, outPatterns)));
	}

	private boolean linesBeforeMatch(List<String> linesBefore) {
		return linesMatch(linesBefore, settings.getInLinesBeforePatterns(),
				settings.getOutLinesBeforePatterns());
	}

	private boolean linesAfterMatch(List<String> linesAfter) {
		return linesMatch(linesAfter, settings.getInLinesAfterPatterns(),
				settings.getOutLinesAfterPatterns());
	}

	public List<SearchResult> searchStringIterator(Iterator<String> it) {
		boolean stop = false;
		int lineNum = 0;
		String line;
		List<String> linesBefore = new ArrayList<String>();
		List<String> linesAfter = new ArrayList<String>();
		Map<Pattern,Integer> patternMatches = new HashMap<Pattern,Integer>();
		List<SearchResult> results = new ArrayList<SearchResult>();
		while ((it.hasNext() || linesAfter.size() > 0) && !stop) {
			lineNum++;
			if (!linesAfter.isEmpty())
				line = linesAfter.remove(0);
			else
				line = it.next();
			if (settings.getLinesAfter() > 0) {
				while (linesAfter.size() < settings.getLinesAfter() && it.hasNext())
					linesAfter.add(it.next());
			}

			if ((settings.getLinesBefore() == 0 || linesBefore.isEmpty() ||
					linesBeforeMatch(linesBefore))
					&&
					(settings.getLinesAfter() == 0 || linesAfter.isEmpty() ||
							linesAfterMatch(linesAfter))) {

				for (Pattern p : settings.getSearchPatterns()) {
					Matcher m = p.matcher(line);
					boolean found = m.find();
					while (found) {
						if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
							found = false;
						} else {
							SearchResult searchResult = new SearchResult(
									p,
									null,
									lineNum,
									m.start() + 1,
									m.end() + 1,
									line,
									new ArrayList<String>(linesBefore),
									new ArrayList<String>(linesAfter));
							results.add(searchResult);
							patternMatches.put(p, 1);
							found = m.find(m.end());
						}
					}
				}
			}

			if (settings.getLinesBefore() > 0) {
				if (linesBefore.size() == settings.getLinesBefore())
					linesBefore.remove(0);
				if (linesBefore.size() < settings.getLinesBefore())
					linesBefore.add(line);
			}
		}
		return results;
	}

	public void searchBinaryFile(SearchFile sf) {
		if (settings.getVerbose()) {
			System.out.println("Searching binary file " + sf.getPath());
		}
		try {
			Scanner scanner = new Scanner(sf.toFile(), "ISO8859-1").useDelimiter("\\Z");
			try {
				String content = scanner.next();

				for (Pattern p : settings.getSearchPatterns()) {
					Matcher m = p.matcher(content);
					if (m.find()) {
						SearchResult searchResult =
							new SearchResult(p, sf, 0, 0, 0, "");
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
		searchFileSet.add(searchResult.getSearchFile());
	}

	void printSearchResults() {
		System.out.println(String.format("Search results (%d):",
			results.size()));
		for (SearchResult r : results) {
			System.out.println(r.toString());
		}
	}

	public List<String> getMatchingDirs() {
		Set<String> dirSet = new HashSet<String>();
		for (SearchFile sf : searchFileSet) {
			dirSet.add(sf.getPath());
		}
		List<String> dirs = new ArrayList<String>(dirSet);
  		java.util.Collections.sort(dirs);
        return dirs;
	}

    public void printMatchingDirs() {
		List<String> dirs = getMatchingDirs();
		System.out.println(String.format("\nMatching directories (%d directories):",
			dirs.size()));
		for (String d : dirs) {
			System.out.println(d);
		}
	}

	public List<String> getMatchingFiles() {
		List<String> files = new ArrayList<String>();
		for (SearchFile sf : searchFileSet) {
			files.add(sf.toString());
		}
  		java.util.Collections.sort(files);
        return files;
	}

    public void printMatchingFiles() {
		List<String> files = getMatchingFiles();
		System.out.println(String.format("\nMatching files (%d files):",
			files.size()));
		for (String f : files) {
			System.out.println(f);
		}
	}

	public List<String> getMatchingLines() {
		Set<String> lineSet = new HashSet<String>();
		for (SearchResult r : results) {
			lineSet.add(r.getLine().trim());
		}
		List<String> lines = new ArrayList<String>(lineSet);
		java.util.Collections.sort(lines);
        return lines;
	}

    public void printMatchingLines() {
		List<String> lines = getMatchingLines();
		System.out.println(String.format("\nMatching lines (%d unique lines):",
			lines.size()));
		for (String line : lines) {
			System.out.println(line);
		}
	}
}
