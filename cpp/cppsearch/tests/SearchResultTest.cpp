#include <catch2/catch_all.hpp>
#include "SearchFileResult.h"
#include "SearchResultFormatter.h"
#include "SearchSettings.h"
#include "cppfind.h"

TEST_CASE("Verify single-line result equals expected", "[SearchResult]") {
    auto settings = cppsearch::SearchSettings();
    settings.colorize(false);
    auto formatter = cppsearch::SearchResultFormatter(settings);
    auto pattern = cppfind::RegexPattern("Search");

    const std::filesystem::path file_path{"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"};
    auto file_type = cppfind::FileType::CODE;
    uint64_t file_size = 1000;
    long mod_time = 1000;
    auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    unsigned long line_num = 10;
    unsigned long match_start_idx = 15;
    unsigned long match_end_idx = 23;
    std::string line = "\tpublic class Searcher\n";
    auto text_result = cppsearch::SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line);
    auto result = cppsearch::SearchFileResult(file_result, std::move(text_result));
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    std::string expected_line = "public class Searcher";
    std::string expected_output = expected_path + ": " + std::to_string(line_num) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter.format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify single-line-longer-than-max_line_length result equals expected", "[SearchResult]") {
    auto settings = cppsearch::SearchSettings();
    settings.colorize(false);
    settings.max_line_length(100);
    auto formatter = cppsearch::SearchResultFormatter(settings);
    auto pattern = cppfind::RegexPattern("maxlen");

    std::filesystem::path file_path{"./maxlen.txt"};
    auto file_type = cppfind::FileType::TEXT;
    uint64_t file_size = 0;
    long mod_time = 0;
    auto file_result = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);

    unsigned long line_num = 1;
    unsigned long match_start_idx = 53;
    unsigned long match_end_idx = 59;
    std::string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    auto text_result = cppsearch::SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line);
    auto result = cppsearch::SearchFileResult(file_result, std::move(text_result));
    std::string expected_path = "./maxlen.txt";
    std::string expected_line = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
    std::string expected_output = expected_path + ": " + std::to_string(line_num) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter.format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify single-line-longer-and-colorize result equals expected", "[SearchResult]") {
    auto settings = cppsearch::SearchSettings();
    settings.colorize(true);
    settings.max_line_length(100);
    auto formatter = cppsearch::SearchResultFormatter(settings);
    auto pattern = cppfind::RegexPattern("maxlen");

    std::filesystem::path file_path{"./maxlen.txt"};
    auto file_type = cppfind::FileType::TEXT;
    uint64_t file_size = 0;
    long mod_time = 0;
    auto file_result = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);

    unsigned long line_num = 1;
    unsigned long match_start_idx = 53;
    unsigned long match_end_idx = 59;
    std::string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    auto text_result = cppsearch::SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line);
    auto result = cppsearch::SearchFileResult(file_result, std::move(text_result));
    std::string expected_path = "./maxlen.txt";
    std::string expected_line = std::string("...89012345678901234567890123456789012345678901") +
            COLOR_GREEN +
            "maxlen" +
            COLOR_RESET +
            "89012345678901234567890123456789012345678901...";
    std::string expected_output = expected_path + ": " + std::to_string(line_num) + ": ["
            + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]: " + expected_line;
    std::string output = formatter.format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify binary result equals expected", "[SearchResult]") {
    auto settings = cppsearch::SearchSettings();
    auto formatter = cppsearch::SearchResultFormatter(settings);
    auto pattern = cppfind::RegexPattern("Search");

    std::filesystem::path file_path{"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"};
    auto file_type = cppfind::FileType::BINARY;
    uint64_t file_size = 0;
    long mod_time = 0;
    auto file_result = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);

    unsigned long line_num = 0;
    unsigned long match_start_idx = 0;
    unsigned long match_end_idx = 0;
    auto text_result = cppsearch::SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, "");
    auto result = cppsearch::SearchFileResult(file_result, std::move(text_result));
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe";
    std::string expected_output = expected_path + " matches at [0:0]";
    std::string output = formatter.format(result);

    REQUIRE(output == expected_output);
}

TEST_CASE("Verify multiline result equals expected", "[SearchResult]") {
    auto settings = cppsearch::SearchSettings();
    settings.colorize(false);
    settings.lines_before(2);
    settings.lines_after(2);
    auto formatter = cppsearch::SearchResultFormatter(settings);
    auto pattern = cppfind::RegexPattern("Search");

    std::filesystem::path file_path{"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"};
    auto file_type = cppfind::FileType::CODE;
    uint64_t file_size = 0;
    long mod_time = 0;
    auto file_result = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);
    unsigned long line_num = 10;
    unsigned long match_start_idx = 15;
    unsigned long match_end_idx = 23;
    std::string line = "\tpublic class Searcher";
    std::vector<std::string> lines_before = {"namespace CsSearch", "{"};
    std::vector<std::string> lines_after = {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
    auto text_result = cppsearch::SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line,
        lines_before, lines_after);
    auto result = cppsearch::SearchFileResult(file_result, std::move(text_result));
    std::string expected_path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    std::string expected_output = std::string("================================================================================\n") +
            expected_path + ": " + std::to_string(line_num) + ": [" + std::to_string(match_start_idx) + ":" + std::to_string(match_end_idx) + "]\n" +
            "--------------------------------------------------------------------------------\n" +
            "   8 | namespace CsSearch\n" +
            "   9 | {\n" +
            "> 10 | \tpublic class Searcher\n" +
            "  11 | \t{\n" +
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    std::string output = formatter.format(result);

    REQUIRE(output == expected_output);
}
