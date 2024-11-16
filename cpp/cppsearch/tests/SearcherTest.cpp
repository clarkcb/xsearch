#include <catch2/catch_all.hpp>
#include "SearchConfig.h"
#include "Searcher.h"

/***************************************************************************
 * search tests
 **************************************************************************/
TEST_CASE("Test search with test file start_path", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    const auto p = start_path;
    settings.add_path(p);
    settings.add_search_pattern("Searcher");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
    REQUIRE(results[1].line_num() == 35);
    REQUIRE(results[1].match_start_idx() == 24);
    REQUIRE(results[1].match_end_idx() == 32);
}

TEST_CASE("Test search with in lines before matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.lines_before(2);
    settings.add_in_lines_before_pattern("FileUtil");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
}

TEST_CASE("Test search with out lines before matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.lines_before(2);
    settings.add_out_lines_before_pattern("FileUtil");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 35);
    REQUIRE(results[0].match_start_idx() == 24);
    REQUIRE(results[0].match_end_idx() == 32);
}

TEST_CASE("Test search with in lines after matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.lines_after(2);
    settings.add_in_lines_after_pattern("Settings");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
}

TEST_CASE("Test search with out lines after matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.lines_after(2);
    settings.add_out_lines_after_pattern("Settings");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 35);
    REQUIRE(results[0].match_start_idx() == 24);
    REQUIRE(results[0].match_end_idx() == 32);
}

TEST_CASE("Test multiline search with test file start_path", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.multi_line_search(true);
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
    REQUIRE(results[1].line_num() == 35);
    REQUIRE(results[1].match_start_idx() == 24);
    REQUIRE(results[1].match_end_idx() == 32);
}

TEST_CASE("Test multiline search with in lines before matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.multi_line_search(true);
    settings.lines_before(2);
    settings.add_in_lines_before_pattern("FileUtil");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
}

TEST_CASE("Test multiline search with out lines before matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.multi_line_search(true);
    settings.lines_before(2);
    settings.add_out_lines_before_pattern("FileUtil");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 35);
    REQUIRE(results[0].match_start_idx() == 24);
    REQUIRE(results[0].match_end_idx() == 32);
}

TEST_CASE("Test multiline search with in lines after matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.multi_line_search(true);
    settings.lines_after(2);
    settings.add_in_lines_after_pattern("Settings");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 29);
    REQUIRE(results[0].match_start_idx() == 3);
    REQUIRE(results[0].match_end_idx() == 11);
}

TEST_CASE("Test multiline search with out lines after matching", "[Searcher]") {
    auto settings = cppsearch::SearchSettings();
    std::string start_path = cppsearch::xsearchpath();
    start_path.append("/shared/testFiles/testFile2.txt");
    settings.add_path(start_path);
    settings.add_search_pattern("Searcher");
    settings.multi_line_search(true);
    settings.lines_after(2);
    settings.add_out_lines_after_pattern("Settings");
    auto searcher = cppsearch::Searcher(settings);

    auto results = searcher.search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0].line_num() == 35);
    REQUIRE(results[0].match_start_idx() == 24);
    REQUIRE(results[0].match_end_idx() == 32);
}
