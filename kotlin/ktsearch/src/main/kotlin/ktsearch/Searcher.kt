package ktsearch

import java.io.File
import java.io.IOException
import java.nio.charset.Charset
import java.util.*

/**
 * @author cary on 7/23/16.
 */
class Searcher(val settings: SearchSettings) {
    val fileTypes: FileTypes

    init {
        validateSettings(settings)
        fileTypes = FileTypes()
    }

    private fun validateSettings(settings: SearchSettings) {
        if (settings.startPath.isNullOrEmpty()) {
            throw SearchException("Startpath not defined")
        }
        val startPathFile = File(settings.startPath)
        if (!startPathFile.exists()) {
            throw SearchException("Startpath not found")
        }
        if (!startPathFile.canRead()) {
            throw SearchException("Startpath not readable")
        }
        if (settings.searchPatterns.isEmpty()) {
            throw SearchException("No search patterns defined")
        }
        try {
            val c = Charset.forName(settings.textFileEncoding)
        } catch (e: IllegalArgumentException) {
            throw SearchException("Invalid encoding provided")
        }
    }

    private fun anyMatchesAnyPattern(sList: List<String>,
                                     patternSet: Set<Regex>): Boolean {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private fun matchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.any { p -> p.containsMatchIn(s) }
    }

    private fun findAnyMatches(s: String, startIndex: Int, patternSet: Set<Regex>): List<MatchResult> {
        val matches: MutableList<MatchResult> = mutableListOf()
        patternSet.map { p -> p.find(s, startIndex) }.filter { m -> m != null }.
                forEach { m -> if (m != null) matches.add(m) }
        return matches.toList()
    }

    fun isSearchDir(d: File): Boolean {
        val pathElems = d.path.split(File.separatorChar)
        if (settings.excludeHidden && pathElems.any { FileUtil.isHidden(it) }) {
            return false
        }
        return (settings.inDirPatterns.isEmpty()
                        || anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.isEmpty()
                        || !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    fun getSearchDirs(startPath: File): List<File> {
        val searchDirs: MutableList<File> = mutableListOf()
        searchDirs.add(startPath)
        if (settings.recursive) {
            searchDirs.addAll(recGetSearchDirs(startPath))
        }
        return searchDirs.toList()
    }

    private fun recGetSearchDirs(dir: File): List<File> {
        val theseSearchDirs: List<File> = (dir.listFiles()?.toList() ?: listOf<File>()).
                filter { d -> d.isDirectory && isSearchDir(d) }
        val searchDirs: MutableList<File> = theseSearchDirs.toMutableList()
        for (d in theseSearchDirs) {
            searchDirs.addAll(recGetSearchDirs(d))
        }
        return searchDirs.toList()
    }

    fun isSearchFile(sf: SearchFile): Boolean {
        val ext = sf.file.extension
        return (settings.inExtensions.isEmpty()
                        || settings.inExtensions.contains(ext))
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(ext))
                &&
                (settings.inFilePatterns.isEmpty()
                        || matchesAnyPattern(sf.file.name, settings.inFilePatterns))
                &&
                (settings.outFilePatterns.isEmpty()
                        || !matchesAnyPattern(sf.file.name, settings.outFilePatterns))
    }

    fun isArchiveSearchFile(sf: SearchFile): Boolean {
        val ext = sf.file.extension
        return (settings.inArchiveExtensions.isEmpty()
                        || settings.inArchiveExtensions.contains(ext))
                &&
                (settings.outArchiveExtensions.isEmpty()
                        || !settings.outArchiveExtensions.contains(ext))
                &&
                (settings.inArchiveFilePatterns.isEmpty()
                        || matchesAnyPattern(sf.file.name, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.isEmpty()
                        || !matchesAnyPattern(sf.file.name, settings.outArchiveFilePatterns))
    }

    fun filterFile(sf: SearchFile): Boolean {
        if (sf.file.isHidden && settings.excludeHidden) {
            return false
        }
        if (sf.fileType === FileType.ARCHIVE) {
            return (settings.searchArchives || settings.archivesOnly) && isArchiveSearchFile(sf)
        }
        return !settings.archivesOnly && isSearchFile(sf)
    }

    fun getSearchFiles(searchDirs: List<File>): List<SearchFile> {
        val searchFiles: MutableList<SearchFile> = mutableListOf()
        for (d in searchDirs) {
            val theseSearchDirs: List<SearchFile> = (d.listFiles()?.toList() ?: listOf<File>()).
                    filter { it.isFile }.
                    map { f -> SearchFile(f, fileTypes.getFileType(f)) }.
                    filter { f -> filterFile(f) }
            searchFiles.addAll(theseSearchDirs)
        }
        return searchFiles.toList()
    }

    fun search(): List<SearchResult> {
        val startPathFile = File(settings.startPath)
        if (startPathFile.isDirectory) {
            return searchPath(startPathFile)
        } else if (startPathFile.isFile) {
            val fileType = fileTypes.getFileType(startPathFile)
            val sf = SearchFile(startPathFile, fileType)
            if (filterFile(sf)) {
                return searchFile(sf)
            } else {
                throw SearchException("Startpath does not match search settings")
            }
        } else {
            throw SearchException("startPath is invalid file type: " + settings.startPath)
        }
    }

    fun searchPath(filePath: File): List<SearchResult> {
        // get the search directories
        val searchDirs = getSearchDirs(filePath)
        if (settings.verbose) {
            log("\nDirectories to be searched (${searchDirs.size}):")
            for (d in searchDirs) {
                println(d.path)
            }
            log("")
        }

        // get the search files in the search directories
        val searchFiles = getSearchFiles(searchDirs)
        if (settings.verbose) {
            log("\nFiles to be searched (${searchFiles.size}):")
            for (sf in searchFiles) {
                log(sf.toString())
            }
            log("")
        }
        return searchFiles(searchFiles)
    }

    fun searchFiles(sfs: List<SearchFile>): List<SearchResult> {
        val results: MutableList<SearchResult> = mutableListOf()
        for (sf in sfs) {
            results.addAll(searchFile(sf))
        }
        return results.toList()
    }

    fun searchFile(sf: SearchFile): List<SearchResult> {
        if (sf.fileType === FileType.TEXT) {
            return searchTextFile(sf)
        } else if (sf.fileType === FileType.BINARY) {
            return searchBinaryFile(sf)
        } else {
            return listOf()
        }
    }

    fun searchTextFile(sf: SearchFile): List<SearchResult> {
        if (settings.verbose) {
            log("Searching text file ${sf.toString()}")
        }
        if (settings.multiLineSearch) {
            return searchTextFileContents(sf)
        } else {
            return searchTextFileLines(sf)
        }
    }

    fun searchTextFileContents(sf: SearchFile): List<SearchResult> {
        val results: List<SearchResult> =
                searchMultilineString(sf.file.readText(Charset.forName(settings.textFileEncoding)))
        return results.map { r -> r.copy(file = sf) }
    }

    fun getLinesFromMultiLineString(s: String, endLineIndices: List<Int>): List<String> {
        fun recGetLinesFromMultiLineString(indices: List<Int>, lines: List<String>): List<String> {
            if (indices.size < 2) {
                return lines
            } else {
                val nextLine = s.substring(indices.first() + 1, indices.get(1))
                return recGetLinesFromMultiLineString(indices.drop(1), lines.plus(nextLine))
            }
        }
        return recGetLinesFromMultiLineString(endLineIndices, listOf())
    }

    fun getLinesAfterFromMultiLineString(s: String,
                                         afterNewlineIndices: List<Int>): List<String> {
        return if (afterNewlineIndices.isNotEmpty()) {
            val startIndex = afterNewlineIndices.first()
            if (settings.linesAfterToPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterToPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minBy { it.range.start }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.start }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count + 1))
                } else listOf()
            } else if (settings.linesAfterUntilPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterUntilPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minBy { it.range.start }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.start }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count))
                } else listOf()
            } else if (settings.linesAfter > 0) {
                val laIndices = afterNewlineIndices.take(settings.linesAfter + 1)
                getLinesFromMultiLineString(s, laIndices)
            } else listOf<String>()
        } else listOf<String>()
    }

    fun searchMultilineString(s: String): List<SearchResult> {
        val results: MutableList<SearchResult> = mutableListOf()
        val newlineIndices: List<Int> = s.indices.zip(s.asIterable()).
                filter { it.second == '\n' }.map { it.first }

        for (p in settings.searchPatterns) {
            val matches: Sequence<MatchResult> =
                    if (settings.firstMatch) {
                        val m = p.find(s)
                        if (m == null) {
                            sequenceOf()
                        } else {
                            sequenceOf(m)
                        }
                    }
                    else p.findAll(s)

            for (m in matches) {
                val beforeNewlineIndices = newlineIndices.takeWhile { it <= m.range.start }
                val beforeLineCount = newlineIndices.count { it <= m.range.start }
                val linesBefore =
                        if (settings.linesBefore > 0) {
//                            val lbIndices = beforeNewlineIndices.reversed().
//                                    take(settings.linesBefore + 1).reversed()
                            val lbIndices = beforeNewlineIndices.reversed().
                                    take(settings.linesBefore + 1).reversed()
                            getLinesFromMultiLineString(s, lbIndices)
                        } else listOf<String>()
                val afterNewlineIndices = newlineIndices.dropWhile { it <= m.range.start }
                val linesAfter = getLinesAfterFromMultiLineString(s, afterNewlineIndices)
                if (!linesBeforeMatch(linesBefore) || !linesAfterMatch(linesAfter)) {
                    continue
                }
                val lineNum = beforeNewlineIndices.size + 1
                val startLineIndex =
                        if (lineNum == 1) 0
                        else beforeNewlineIndices.last() + 1
                val endLineIndex =
                        if (afterNewlineIndices.isNotEmpty()) afterNewlineIndices.first()
                        else s.length - 1
                val line = s.substring(startLineIndex, endLineIndex)
                val startMatchIndex = m.range.start - startLineIndex + 1
                val endMatchIndex = m.range.endInclusive - startLineIndex + 2
                results.add(SearchResult(
                        p,
                        null,
                        lineNum,
                        startMatchIndex,
                        endMatchIndex,
                        line,
                        linesBefore,
                        linesAfter))
            }
        }
        return results.toList()
    }

    fun searchTextFileLines(sf: SearchFile): List<SearchResult> {
        val results: List<SearchResult> = sf.file.reader(Charset.forName(settings.textFileEncoding)).
                useLines { ss -> searchStringIterator(ss.iterator()) }
        return results.map { r -> r.copy(file = sf) }
    }

    fun linesMatch(lines: List<String>, inPatterns: Set<Regex>,
                   outPatterns: Set<Regex>): Boolean {
        return (inPatterns.isEmpty() || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.isEmpty() || !anyMatchesAnyPattern(lines, outPatterns))

    }

    fun linesBeforeMatch(linesBefore: List<String>): Boolean {
        return linesBefore.isEmpty()
                ||
                linesMatch(linesBefore, settings.inLinesBeforePatterns,
                        settings.outLinesBeforePatterns)
    }

    fun linesAfterMatch(linesAfter: List<String>): Boolean {
        return linesAfter.isEmpty()
                ||
                linesMatch(linesAfter, settings.inLinesAfterPatterns,
                        settings.outLinesAfterPatterns)
    }

    private fun matchLinesAfterToOrUntil(linesAfter: MutableList<String>,
                                         lines: Iterator<String>): Boolean {
        if (settings.linesAfterToPatterns.isEmpty()
                && settings.linesAfterUntilPatterns.isEmpty())
            return true

        for (i: Int in linesAfter.indices) {
            if (matchesAnyPattern(linesAfter.get(i), settings.linesAfterToPatterns)) {
                while (i + 1 < linesAfter.size) linesAfter.removeAt(i + 1)
                return true
            } else if (matchesAnyPattern(linesAfter.get(i), settings.linesAfterUntilPatterns)) {
                while (i < linesAfter.size) linesAfter.removeAt(i)
                return true
            }
        }
        var foundMatch = false
        while (!foundMatch && lines.hasNext()) {
            val nextLine = lines.next()
            if (matchesAnyPattern(nextLine, settings.linesAfterToPatterns)) {
                linesAfter.add(nextLine)
                foundMatch = true
            } else if (matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns)) {
                foundMatch = true
            } else {
                linesAfter.add(nextLine)
            }
        }
        return foundMatch
    }

    fun searchStringIterator(lines: Iterator<String>): List<SearchResult> {
        val results: MutableList<SearchResult> = mutableListOf()
        val linesBefore: MutableList<String> = mutableListOf()
        val linesAfter: MutableList<String> = mutableListOf()
        var lineNum = 0
        val matchedPatterns : MutableSet<Regex> = mutableSetOf()
        while (true) {
            lineNum += 1
            val line =
                    if (!linesAfter.isEmpty()) {
                        linesAfter.removeAt(0)
                    } else if (lines.hasNext()) {
                        lines.next()
                    } else {
                        break
                    }
            if (settings.linesAfter > 0) {
                while (linesAfter.size < settings.linesAfter && lines.hasNext()) {
                    linesAfter.add(lines.next())
                }
            }

            val searchPatterns =
                    if (settings.firstMatch)
                        settings.searchPatterns.filterNot { matchedPatterns.contains(it) }
                    else settings.searchPatterns

            if (searchPatterns.isEmpty()) {
                break
            }

            for (p in searchPatterns) {
                val matches: Sequence<MatchResult> =
                        if (settings.firstMatch) {
                            val m = p.find(line)
                            if (m == null) {
                                sequenceOf()
                            } else {
                                sequenceOf(m)
                            }
                        }
                        else p.findAll(line)
                if (matches.count() == 0
                        || !linesBeforeMatch(linesBefore)
                        || !linesAfterMatch(linesAfter)
                        || !matchLinesAfterToOrUntil(linesAfter, lines.iterator())) {
                    continue
                }

                matchedPatterns.add(p)

                for (m in matches) {
                    results.add(SearchResult(
                            p,
                            null,
                            lineNum,
                            m.range.start + 1,
                            m.range.endInclusive + 2,
                            line,
                            linesBefore.toList(),
                            linesAfter.toList()))
                }
            }
            if (settings.linesBefore > 0) {
                if (linesBefore.size == settings.linesBefore) {
                    linesBefore.removeAt(0)
                }
                if (linesBefore.size < settings.linesBefore) {
                    linesBefore.add(line)
                }
            }
            if (settings.firstMatch && matchedPatterns.size == settings.searchPatterns.size) {
                break
            }
        }
        return results.toList()
    }

    fun searchBinaryFile(sf: SearchFile): List<SearchResult> {
        if (settings.verbose) {
            log("Searching binary file ${sf.toString()}")
        }
        val results: MutableList<SearchResult> = mutableListOf()

        try {
            val content: String = sf.file.readText(Charset.forName("ISO8859-1"))
            for (p in settings.searchPatterns) {
                val matches: Sequence<MatchResult> =
                        if (settings.firstMatch) {
                            val m = p.find(content)
                            if (m == null) {
                                sequenceOf()
                            } else {
                                sequenceOf(m)
                            }
                        }
                        else p.findAll(content)
                matches.forEach { m ->
                    results.add(SearchResult(
                            p,
                            sf,
                            0,
                            m.range.start + 1,
                            m.range.endInclusive + 1,
                            ""))
                }
            }
        } catch (e: IOException) {
            log(e.toString())
        } catch (e: NoSuchElementException) {
            log(e.toString())
        } catch (e: IllegalStateException) {
            log(e.toString())
        }
        return results.toList()
    }
}
