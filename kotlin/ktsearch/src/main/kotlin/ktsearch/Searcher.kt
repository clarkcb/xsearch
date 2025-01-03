package ktsearch

import ktfind.*
import java.io.IOException
import java.nio.charset.Charset
import kotlin.streams.toList

/**
 * @author cary on 7/23/16.
 */
class Searcher(val settings: SearchSettings) {
    private val finder: Finder
    private var charset: Charset? = null

    init {
        try {
            finder = Finder(settings.findSettings)
        } catch (e: FindException) {
            throw SearchException(e.message!!)
        }
        validateSettings(settings)
    }

    private fun validateSettings(settings: SearchSettings) {
        if (settings.searchPatterns.isEmpty()) {
            throw SearchException("No search patterns defined")
        }
        if (settings.linesAfter < 0) {
            throw SearchException("Invalid linesafter")
        }
        if (settings.linesBefore < 0) {
            throw SearchException("Invalid linesbefore")
        }
        if (settings.maxLineLength < 0) {
            throw SearchException("Invalid maxlinelength")
        }
        try {
            charset = Charset.forName(settings.textFileEncoding)
        } catch (_: IllegalArgumentException) {
            throw SearchException("Invalid or unsupported encoding: ${settings.textFileEncoding}")
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
        return patternSet.mapNotNull { p -> p.find(s, startIndex) }
    }

    fun search(): List<SearchResult> {
        val fileResults = finder.find()
        if (settings.verbose) {
            val dirResults: List<String> = fileResults.asSequence()
                .map { it.path.parent }
                .map { it?.toString() ?: "." }
                .distinct().sorted().toList()
            log("\nDirectories to be searched (${dirResults.size}):")
            for (d in dirResults) {
                log(d)
            }
            log("\n\nFiles to be searched (${fileResults.size}):")
            for (fr in fileResults) {
                log(fr.toString())
            }
            log("")
        }

        return searchFiles(fileResults)
    }

    private fun searchFiles(frs: List<FileResult>): List<SearchResult> {
        val results = frs.flatMap { searchFile(it) }
        return sortSearchResults(results)
    }

    private fun searchFile(fr: FileResult): List<SearchResult> {
        return when {
            setOf(FileType.TEXT, FileType.CODE, FileType.XML).contains(fr.fileType) -> {
                searchTextFile(fr)
            }
            fr.fileType === FileType.BINARY -> {
                searchBinaryFile(fr)
            }
            else -> {
                listOf()
            }
        }
    }

    private fun searchTextFile(fr: FileResult): List<SearchResult> {
        if (settings.debug) {
            log("Searching text file $fr")
        }
        return if (settings.multiLineSearch) {
            searchTextFileContents(fr)
        } else {
            searchTextFileLines(fr)
        }
    }

    private fun searchTextFileContents(fr: FileResult): List<SearchResult> {
        val results: List<SearchResult> =
                searchMultilineString(fr.path.toFile().readText(charset!!))
        return results.map { r -> r.copy(file = fr) }
    }

    private fun getLinesFromMultiLineString(s: String, endLineIndices: List<Int>): List<String> {
        fun recGetLinesFromMultiLineString(indices: List<Int>, lines: List<String>): List<String> {
            return if (indices.size < 2) {
                lines
            } else {
                val nextLine = s.substring(indices.first() + 1, indices[1])
                recGetLinesFromMultiLineString(indices.drop(1), lines.plus(nextLine))
            }
        }
        return recGetLinesFromMultiLineString(endLineIndices, listOf())
    }

    private fun getLinesAfterFromMultiLineString(s: String,
                                                 afterNewlineIndices: List<Int>): List<String> {
        return if (afterNewlineIndices.isNotEmpty()) {
            val startIndex = afterNewlineIndices.first()
            if (settings.linesAfterToPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterToPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minByOrNull { it.range.first }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.first }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count + 1))
                } else listOf()
            } else if (settings.linesAfterUntilPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterUntilPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minByOrNull { it.range.first }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.first }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count))
                } else listOf()
            } else if (settings.linesAfter > 0) {
                val laIndices = afterNewlineIndices.take(settings.linesAfter + 1)
                getLinesFromMultiLineString(s, laIndices)
            } else listOf()
        } else listOf()
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
                val beforeNewlineIndices = newlineIndices.takeWhile { it <= m.range.first }
                // val beforeLineCount = newlineIndices.count { it <= m.range.first }
                val linesBefore =
                        if (settings.linesBefore > 0) {
                            val lbIndices = beforeNewlineIndices.reversed().
                                    take(settings.linesBefore + 1).reversed()
                            getLinesFromMultiLineString(s, lbIndices)
                        } else listOf()
                val afterNewlineIndices = newlineIndices.dropWhile { it <= m.range.first }
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
                val startMatchIndex = m.range.first - startLineIndex + 1
                val endMatchIndex = m.range.last - startLineIndex + 2
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

    fun searchTextFileLines(sf: FileResult): List<SearchResult> {
        val results: List<SearchResult> = sf.path.toFile().reader(charset!!).
                useLines { ss -> searchLineIterator(ss.iterator()) }
        return results.map { r -> r.copy(file = sf) }
    }

    private fun linesMatch(lines: List<String>, inPatterns: Set<Regex>,
                   outPatterns: Set<Regex>): Boolean {
        return (inPatterns.isEmpty() || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.isEmpty() || !anyMatchesAnyPattern(lines, outPatterns))

    }

    private fun linesBeforeMatch(linesBefore: List<String>): Boolean {
        return linesBefore.isEmpty()
                ||
                linesMatch(linesBefore, settings.inLinesBeforePatterns,
                        settings.outLinesBeforePatterns)
    }

    private fun linesAfterMatch(linesAfter: List<String>): Boolean {
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
            if (matchesAnyPattern(linesAfter[i], settings.linesAfterToPatterns)) {
                while (i + 1 < linesAfter.size) linesAfter.removeAt(i + 1)
                return true
            } else if (matchesAnyPattern(linesAfter[i], settings.linesAfterUntilPatterns)) {
                while (i < linesAfter.size) linesAfter.removeAt(i)
                return true
            }
        }
        var foundMatch = false
        while (!foundMatch && lines.hasNext()) {
            val nextLine = lines.next()
            when {
                matchesAnyPattern(nextLine, settings.linesAfterToPatterns) -> {
                    linesAfter.add(nextLine)
                    foundMatch = true
                }
                matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns) -> {
                    foundMatch = true
                }
                else -> {
                    linesAfter.add(nextLine)
                }
            }
        }
        return foundMatch
    }

    fun searchLineIterator(lines: Iterator<String>): List<SearchResult> {
        val results: MutableList<SearchResult> = mutableListOf()
        val linesBefore: MutableList<String> = mutableListOf()
        val linesAfter: MutableList<String> = mutableListOf()
        var lineNum = 0
        val matchedPatterns : MutableSet<Regex> = mutableSetOf()
        while (true) {
            lineNum += 1
            val line =
                    if (linesAfter.isNotEmpty()) {
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
                            m.range.first + 1,
                            m.range.last + 2,
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

    private fun searchBinaryFile(sf: FileResult): List<SearchResult> {
        if (settings.verbose) {
            log("Searching binary file $sf")
        }
        val results: MutableList<SearchResult> = mutableListOf()

        try {
            val content: String = sf.path.toFile().readText(Charsets.ISO_8859_1)
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
                            m.range.first + 1,
                            m.range.last + 1,
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

    private fun sortSearchResults(searchResults: List<SearchResult>): List<SearchResult> {
        val sortedSearchResults: MutableList<SearchResult> =
            when (settings.sortBy) {
                SortBy.FILENAME -> {
                    searchResults.stream()
                        .sorted { sr1, sr2 -> sr1.compareByName(sr2, settings.sortCaseInsensitive) }.toList()
                }
                SortBy.FILESIZE -> {
                    searchResults.stream()
                        .sorted { sr1, sr2 -> sr1.compareBySize(sr2, settings.sortCaseInsensitive) }.toList()
                }
                SortBy.FILETYPE -> {
                    searchResults.stream()
                        .sorted { sr1, sr2 -> sr1.compareByType(sr2, settings.sortCaseInsensitive) }.toList()
                }
                SortBy.LASTMOD -> {
                    searchResults.stream()
                        .sorted { sr1, sr2 -> sr1.compareByLastMod(sr2, settings.sortCaseInsensitive) }.toList()
                }
                else -> {
                    searchResults.stream()
                        .sorted { sr1, sr2 -> sr1.compareByPath(sr2, settings.sortCaseInsensitive) }.toList()
                }
            }.toMutableList()

        if (settings.sortDescending) {
            sortedSearchResults.reverse()
        }

        return sortedSearchResults.toList()
    }
}
