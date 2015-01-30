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
	private FileTypes fileTypes;
	private Set<SearchFile> searchFileSet;
	private Map<String,Long> timers;
	private long totalElapsedTime;

	public Searcher(SearchSettings settings) throws SearchException {
		this.settings = settings;
		this.results = new ArrayList<SearchResult>();
		this.fileTypes = new FileTypes();
		this.searchFileSet = new HashSet<SearchFile>();
		this.timers = new HashMap<String,Long>();
		this.totalElapsedTime = 0L;
        validateSettings();
	}

	private void log(String message) {
		System.out.println(message);
	}

	private void validateSettings() throws SearchException {
        if (null == settings.getStartPath() || settings.getStartPath().equals("")) {
            throw new SearchException("Startpath not defined");
        }
        File startPathFile = new File(settings.getStartPath());
        if (!startPathFile.exists()) {
            throw new SearchException("Startpath not found");
        }
        if (settings.getSearchPatterns().isEmpty()) {
            throw new SearchException("No search patterns defined");
        }
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
        List<String> pathElems = FileUtil.splitPath(d.toString());
        if (settings.getExcludeHidden()) {
            for (String p : pathElems) {
                if (FileUtil.isHidden(p)) return false;
            }
        }
		return (settings.getInDirPatterns().isEmpty() ||
                anyMatchesAnyPattern(pathElems, settings.getInDirPatterns()))
                &&
                (settings.getOutDirPatterns().isEmpty() ||
                 !anyMatchesAnyPattern(pathElems, settings.getOutDirPatterns()));
	}

	public List<File> getSearchDirs(File startPath) {
		List<File> searchDirs = new ArrayList<File>();
		if (settings.getDebug()) {
			log(String.format("Getting files to search under %s",
					startPath.getPath()));
		}
		searchDirs.add(startPath);
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
		String ext = FileUtil.getExtension(f);
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
		String ext = FileUtil.getExtension(f);
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

	private boolean filterFile(File f, FileType fileType) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return false;
        }
		if (fileType == FileType.ARCHIVE) {
            return settings.getSearchArchives() && isArchiveSearchFile(f);
        }
        return !settings.getArchivesOnly() && isSearchFile(f);
    }

	private List<SearchFile> getSearchFilesForDir(File dir) {
		if (settings.getDebug()) {
			log(String.format("Getting files to search under %s", dir));
		}
		List<SearchFile> searchFiles = new ArrayList<SearchFile>();
		File currentFiles[] = dir.listFiles();
        if (currentFiles != null) {
            for (File f : currentFiles) {
                if (!f.isDirectory()) {
					FileType fileType = fileTypes.getFileType(f);
					if (filterFile(f, fileType)) {
						searchFiles.add(new SearchFile(f.getParent(), f.getName(),
								fileType));
					}
                }
            }
        }
        return searchFiles;
	}

	public List<SearchFile> getSearchFiles(List<File> searchDirs) {
		File f = new File(settings.getStartPath());
		List<SearchFile> searchFiles = new ArrayList<SearchFile>();
		for (File d : searchDirs) {
			searchFiles.addAll(getSearchFilesForDir(d));
		}
		return searchFiles;
	}

	private void addTimer(String name, String action) {
		timers.put(name+":"+action, System.currentTimeMillis());
	}

	private void startTimer(String name) {
		addTimer(name, "start");
	}

	private void stopTimer(String name) {
		addTimer(name, "stop");
		totalElapsedTime += getElapsed(name);
    }

    public long getElapsed(String name) {
        long startTime = this.timers.get(name+":start");
        long stopTime = this.timers.get(name+":stop");
		return stopTime - startTime;
    }

    public void printElapsed(String name) {
        log(String.format("Elapsed time for \"%s\": %d ms",
				name, getElapsed(name)));
    }

    public void printTotalElapsed() {
        log(String.format("Total elapsed time: %d ms", totalElapsedTime));
    }

	public void search() throws SearchException {
		// figure out if startPath is a directory or a file and search accordingly
		File startPathFile = new File(settings.getStartPath());
		if (startPathFile.isDirectory()) {
			if (isSearchDir(startPathFile)) {
				searchPath(startPathFile);
			} else {
				throw new SearchException("Startpath does not match search settings");
			}
		} else if (startPathFile.isFile()) {
			FileType fileType = fileTypes.getFileType(startPathFile);
			if ((fileType == FileType.ARCHIVE && settings.getSearchArchives()
					&& isArchiveSearchFile(startPathFile))
					||
					(!settings.getArchivesOnly() && isSearchFile(startPathFile))) {
				File d = startPathFile.getParentFile();
				if (null == d)
					d = new File(".");
				searchFile(new SearchFile(d.getPath(), startPathFile.getName(),
						fileType));
			} else {
				throw new SearchException("Startpath does not match search settings");
			}
		} else {
			throw new SearchException("Startpath is not a searchable file type");
		}
		if (settings.getVerbose()) {
			log("\nFile search complete.\n");
		}
	}

	public void searchPath(File filePath) {
		// get the search directories
		if (settings.getDoTiming())
			startTimer("getSearchDirs");
		List<File> searchDirs = getSearchDirs(filePath);
		if (settings.getDoTiming()) {
			stopTimer("getSearchDirs");
			if (settings.getPrintResults()) {
				printElapsed("getSearchDirs");
			}
		}
		if (settings.getVerbose()) {
			log(String.format("\nDirectories to be searched (%d):",
					searchDirs.size()));
			for (File d : searchDirs) {
				System.out.println(d.getPath());
			}
			log("");
		}

		// get the search files in the search directories
		if (settings.getDoTiming())
			startTimer("getSearchFiles");
		List<SearchFile> searchFiles = getSearchFiles(searchDirs);
		if (settings.getDoTiming()) {
			stopTimer("getSearchFiles");
			if (settings.getPrintResults()) {
				printElapsed("getSearchFiles");
			}
		}
		if (settings.getVerbose()) {
			log(String.format("\nFiles to be searched (%d):",
					searchFiles.size()));
			for (SearchFile sf : searchFiles) {
				log(sf.toString());
			}
			log("");
		}

		// search the files
		if (settings.getDoTiming())
			startTimer("searchFiles");
		for (SearchFile sf : searchFiles) {
			searchFile(sf);
		}
		if (settings.getDoTiming()) {
			stopTimer("searchFiles");
			if (settings.getPrintResults()) {
				printElapsed("searchFiles");
				printTotalElapsed();
			}
		}
	}

	public void searchFile(SearchFile sf) {
		FileType fileType = sf.getFileType();
		if (fileType == FileType.TEXT) {
			searchTextFile(sf);
		} else if (fileType == FileType.BINARY) {
			searchBinaryFile(sf);
		}
	}

	public void searchTextFile(SearchFile sf) {
		if (settings.getVerbose()) {
			log(String.format("Searching text file %s", sf.toString()));
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
				log(e.toString());
			} catch (IllegalStateException e) {
				log(e.toString());
			} finally {
				scanner.close();
			}
		} catch (IOException e) {
			log(e.toString());
		}
	}

	private List<Number> getNewLineIndices(String s) {
		List<Number> newlineIndices = new ArrayList<Number>();
		for (int i=0; i < s.length(); i++) {
			if (s.charAt(i) == '\n')
				newlineIndices.add(i);
		}
		return newlineIndices;
	}

	private List<Number> getStartLineIndicesFromNewLineIndices(List<Number> newLineIndices) {
		List<Number> startLineIndices = new ArrayList<Number>();
		startLineIndices.add(0);
		for (Number newLineIndex : newLineIndices) {
			startLineIndices.add(newLineIndex.longValue() + 1);
		}
		return startLineIndices;
	}

	private List<Number> getEndLineIndicesFromNewLineIndices(String s,
															 List<Number> newLineIndices) {
		List<Number> endLineIndices = new ArrayList<Number>();
		for (Number newLineIndex : newLineIndices) {
			endLineIndices.add(newLineIndex.longValue());
		}
		endLineIndices.add(s.length() - 1);
		return endLineIndices;
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

	private List<String> getLinesBeforeFromMultiLineString(String s,
														   List<Number> beforeStartIndices,
														   List<Number> beforeEndIndices) {
		List<String> linesBefore = new ArrayList<String>();
		if (settings.getLinesBefore() > 0) {
			List<Number> starts = ListUtil.takeRight(beforeStartIndices,
					settings.getLinesBefore());
			List<Number> ends = ListUtil.tail(ListUtil.takeRight(beforeEndIndices,
					settings.getLinesBefore() + 1));
			for (int i=0; i < starts.size(); i++) {
				linesBefore.add(s.substring(starts.get(i).intValue(),
						ends.get(i).intValue() - 1));
			}
		}
		return linesBefore;
	}

	private List<String> getLinesAfterFromMultiLineString(String s,
														  List<Number> afterStartIndices,
														  List<Number> afterEndIndices) {
		List<String> linesAfter = new ArrayList<String>();
		if (settings.getLinesAfter() > 0) {
			List<Number> starts = ListUtil.take(afterStartIndices, settings.getLinesAfter());
			List<Number> ends = ListUtil.tail(ListUtil.take(afterEndIndices,
					settings.getLinesAfter() + 1));
			for (int i=0; i < starts.size(); i++) {
				linesAfter.add(s.substring(starts.get(i).intValue(),
						ends.get(i).intValue() - 1));
			}
		}
		return linesAfter;
	}

	private List<SearchResult> searchMultiLineStringForPattern(String s, Pattern p) {
		Map<Pattern,Integer> patternMatches = new HashMap<Pattern,Integer>();
		List<SearchResult> results = new ArrayList<SearchResult>();
		List<Number> newLineIndices = getNewLineIndices(s);
		List<Number> startLineIndices = getStartLineIndicesFromNewLineIndices(newLineIndices);
		List<Number> endLineIndices = getEndLineIndicesFromNewLineIndices(s,
				newLineIndices);
		Matcher m = p.matcher(s);
		boolean found = m.find();
		while (found) {
			if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
				found = false;
			} else {
				// get the start and end indices before the match index
				List<Number> beforeStartIndices = ListUtil.lessThanOrEqualTo(m.start(),
						startLineIndices);
				List<Number> beforeEndIndices = ListUtil.lessThan(m.start(),
						endLineIndices);
				// add another end line index if it exists or the tail index of the string
				if (endLineIndices.size() > beforeEndIndices.size())
					beforeEndIndices.add(endLineIndices.get(beforeEndIndices.size()));
				else
					beforeEndIndices.add(s.length() - 1);
				List<Number> afterStartIndices = ListUtil.greaterThan(m.start(),
						startLineIndices);
				List<Number> afterEndIndices = ListUtil.greaterThan(m.start(),
						endLineIndices);
				int lineNum = beforeStartIndices.size();
				int startLineIndex = ListUtil.max(beforeStartIndices).intValue();
				int endLineIndex = ListUtil.min(afterStartIndices).intValue() - 1;
				String line = s.substring(startLineIndex, endLineIndex);
				List<String> linesBefore = getLinesBeforeFromMultiLineString(s,
						ListUtil.init(beforeStartIndices),
						ListUtil.init(beforeEndIndices));
				List<String> linesAfter = getLinesAfterFromMultiLineString(s,
						afterStartIndices, afterEndIndices);
				if ((linesBefore.isEmpty() || linesBeforeMatch(linesBefore))
						&&
						(linesAfter.isEmpty() || linesAfterMatch(linesAfter))) {
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
		return ((inPatterns.size() == 0 || anyMatchesAnyPattern(lines, inPatterns))
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
						// take care of linesAfterToPatterns or linesAfterUntilPatterns
						boolean linesAfterToMatch = false;
						boolean linesAfterUntilMatch = false;
						if (settings.hasLinesAfterToOrUntilPatterns()) {
							if (settings.hasLinesAfterToPatterns()) {
								Set<Pattern> linesAfterToPatterns = settings.getLinesAfterToPatterns();
								if (anyMatchesAnyPattern(linesAfter, linesAfterToPatterns)) {
									linesAfterToMatch = true;
								} else {
									while (it.hasNext() && !linesAfterToMatch) {
										String nextLine = it.next();
										linesAfter.add(nextLine);
										if (matchesAnyPattern(nextLine, linesAfterToPatterns)) {
											linesAfterToMatch = true;
										}
									}
								}
							} else if (settings.hasLinesAfterUntilPatterns()) {
								Set<Pattern> linesAfterUntilPatterns = settings.getLinesAfterUntilPatterns();
								if (anyMatchesAnyPattern(linesAfter, linesAfterUntilPatterns)) {
									linesAfterUntilMatch = true;
								} else {
									while (it.hasNext() && !linesAfterUntilMatch) {
										String nextLine = it.next();
										linesAfter.add(nextLine);
										if (matchesAnyPattern(nextLine, linesAfterUntilPatterns)) {
											linesAfterUntilMatch = true;
										}
									}
								}
							}
						}

						if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
							stop = true;
						} else if (settings.hasLinesAfterToOrUntilPatterns() &&
								!linesAfterToMatch && !linesAfterUntilMatch) {
							found = false;
						} else {
							List<String> resLinesAfter;
							if (linesAfterUntilMatch)
								resLinesAfter = ListUtil.init(linesAfter);
							else
								resLinesAfter = linesAfter;
							SearchResult searchResult = new SearchResult(
									p,
									null,
									lineNum,
									m.start() + 1,
									m.end() + 1,
									line,
									new ArrayList<String>(linesBefore),
									new ArrayList<String>(resLinesAfter));
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
			log(String.format("Searching binary file %s", sf.getPath()));
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
				log(e.toString());
			} catch (IllegalStateException e) {
				log(e.toString());
			} finally {
				scanner.close();
			}
		} catch (IOException e) {
			log(e.toString());
		}
	}

	private void addSearchResult(SearchResult searchResult) {
		results.add(searchResult);
		searchFileSet.add(searchResult.getSearchFile());
	}

	void printSearchResults() {
		log(String.format("Search results (%d):", results.size()));
		for (SearchResult r : results) {
			log(r.toString());
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
		log(String.format("\nDirectories with matches (%d):", dirs.size()));
		for (String d : dirs) {
			log(d);
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
		log(String.format("\nFiles with matches (%d):", files.size()));
		for (String f : files) {
			log(f);
		}
	}

	public List<String> getMatchingLines() {
		List<String> lines = new ArrayList<String>();
		for (SearchResult r : results) {
			lines.add(r.getLine().trim());
		}
		if (settings.getUniqueLines()) {
			Set<String> lineSet = new HashSet<String>(lines);
			lines = new ArrayList<String>(lineSet);
		}
		java.util.Collections.sort(lines);
        return lines;
	}

    public void printMatchingLines() {
		List<String> lines = getMatchingLines();
		String hdr;
		if (settings.getUniqueLines())
			hdr = "\nUnique lines with matches (%d):";
		else
			hdr = "\nLines with matches (%d):";
		log(String.format(hdr, lines.size()));
		for (String line : lines) {
			log(line);
		}
	}
}
