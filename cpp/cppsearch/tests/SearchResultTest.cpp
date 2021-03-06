#include <catch2/catch.hpp>
#include "SearchFile.h"
#include "SearchPattern.h"
#include "SearchResult.h"
#include "SearchResultFormatter.h"
#include "SearchSettings.h"

TEST_CASE("Verify single-line result equals expected", "[SearchResult]") {
    auto *settings = new cppsearch::SearchSettings();
    settings->colorize(false);
    auto *formatter = new cppsearch::SearchResultFormatter(settings);
    auto *pattern = new cppsearch::SearchPattern("Search");
    std::string path = "~/src/xsearch/csharp/CsSearch/CsSearch";
    std::string filename = "Searcher.cs";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::CODE);
    unsigned long linenum = 10;
    unsigned long match_start_idx = 15;
    unsigned long match_end_idx = 23;
    std::string line = "\tpublic class Searcher\n";
    auto *result = new cppsearch::SearchResult(pattern, searchfile, linenum, match_start_idx, match_end_idx, line);
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    std::string expected_line = "public class Searcher";
    std::string expected_output = expected_path + ": " + std::to_string(linenum) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter->format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify single-line-longer-than-maxlinelength result equals expected", "[SearchResult]") {
    auto *settings = new cppsearch::SearchSettings();
    settings->colorize(false);
    settings->maxlinelength(100);
    auto *formatter = new cppsearch::SearchResultFormatter(settings);
    auto *pattern = new cppsearch::SearchPattern("maxlen");
    std::string path = ".";
    std::string filename = "maxlen.txt";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::TEXT);
    unsigned long linenum = 1;
    unsigned long match_start_idx = 53;
    unsigned long match_end_idx = 59;
    std::string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    auto *result = new cppsearch::SearchResult(pattern, searchfile, linenum, match_start_idx, match_end_idx, line);
    std::string expected_path = "./maxlen.txt";
    std::string expected_line = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
    std::string expected_output = expected_path + ": " + std::to_string(linenum) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter->format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify single-line-longer-and-colorize result equals expected", "[SearchResult]") {
    auto *settings = new cppsearch::SearchSettings();
    settings->colorize(true);
    settings->maxlinelength(100);
    auto *formatter = new cppsearch::SearchResultFormatter(settings);
    auto *pattern = new cppsearch::SearchPattern("maxlen");
    std::string path = ".";
    std::string filename = "maxlen.txt";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::TEXT);
    unsigned long linenum = 1;
    unsigned long match_start_idx = 53;
    unsigned long match_end_idx = 59;
    std::string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    auto *result = new cppsearch::SearchResult(pattern, searchfile, linenum, match_start_idx, match_end_idx, line);
    std::string expected_path = "./maxlen.txt";
    std::string expected_line = std::string("...89012345678901234567890123456789012345678901") +
            COLOR_GREEN +
            "maxlen" +
            COLOR_RESET +
            "89012345678901234567890123456789012345678901...";
    std::string expected_output = expected_path + ": " + std::to_string(linenum) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter->format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify binary result equals expected", "[SearchResult]") {
    auto *settings = new cppsearch::SearchSettings();
    auto *formatter = new cppsearch::SearchResultFormatter(settings);
    auto *pattern = new cppsearch::SearchPattern("Search");
    std::string path = "~/src/xsearch/csharp/CsSearch/CsSearch";
    std::string filename = "Searcher.exe";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::BINARY);
    unsigned long linenum = 0;
    unsigned long match_start_idx = 0;
    unsigned long match_end_idx = 0;
    auto *result = new cppsearch::SearchResult(pattern, searchfile, linenum, match_start_idx, match_end_idx, "");
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe";
    std::string expected_output = expected_path + " matches at [0:0]";
    std::string output = formatter->format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify multiline result equals expected", "[SearchResult]") {
    auto *settings = new cppsearch::SearchSettings();
    settings->colorize(false);
    settings->linesbefore(2);
    settings->linesafter(2);
    auto *formatter = new cppsearch::SearchResultFormatter(settings);
    auto *pattern = new cppsearch::SearchPattern("Search");
    std::string path = "~/src/xsearch/csharp/CsSearch/CsSearch";
    std::string filename = "Searcher.cs";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::CODE);
    unsigned long linenum = 10;
    unsigned long match_start_idx = 15;
    unsigned long match_end_idx = 23;
    std::string line = "\tpublic class Searcher";
    std::vector<std::string> lines_before = {"namespace CsSearch", "{"};
    std::vector<std::string> lines_after = {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
    auto *result = new cppsearch::SearchResult(pattern, searchfile, linenum, match_start_idx, match_end_idx, line,
                                               &lines_before, &lines_after);
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    std::string expected_output = std::string("================================================================================\n") +
            expected_path + ": " + std::to_string(linenum) + ": [" + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]\n" +
            "--------------------------------------------------------------------------------\n" +
            "   8 | namespace CsSearch\n" +
            "   9 | {\n" +
            "> 10 | \tpublic class Searcher\n" +
            "  11 | \t{\n" +
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    std::string output = formatter->format(result);

    REQUIRE(output == expected_output);
}
