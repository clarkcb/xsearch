#include <catch2/catch.hpp>
#include "config.h"
#include "Searcher.h"

/***************************************************************************
 * filter_file tests
 **************************************************************************/
TEST_CASE("Test filter_file hidden file should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file hidden file include-hidden should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->exclude_hidden(false);
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file archive file should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file search-archives should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->search_archives(true);
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file is_archive_file should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->search_archives(true);
    settings->add_in_archive_extension("zip");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file !is_archive_file should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->search_archives(true);
    settings->add_out_archive_extension("zip");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file archives-only should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->archives_only(true);
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file non-archive file archives-only should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->archives_only(true);
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file no exts no patterns should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching in-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_extension("cs");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching in-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_extension("cpp");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching out-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_extension("cs");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching out-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_extension("cpp");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->filter_file("FileUtil.cs"));
}

/***************************************************************************
 * is_search_dir tests
 **************************************************************************/
TEST_CASE("Test is_search_dir single dot should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir("."));
}

TEST_CASE("Test is_search_dir double dot should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir(".."));
}

TEST_CASE("Test is_search_dir hidden dir should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_dir(".git"));
}

TEST_CASE("Test is_search_dir hidden dir include hidden should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->exclude_hidden(false);
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir(".git"));
}

TEST_CASE("Test is_search_dir no patterns should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir("Users"));
}

TEST_CASE("Test is_search_dir matches in-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_dir_pattern("Search");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir("CsSearch"));
}

TEST_CASE("Test is_search_dir matches out-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_dir_pattern("Search");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_dir("CsSearch"));
}

TEST_CASE("Test is_search_dir doesn't match in-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_dir_pattern("SearchFiles");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_dir("CsSearch"));
}

TEST_CASE("Test is_search_dir doesn't match out-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_dir_pattern("SearchFiles");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_dir("CsSearch"));
}

/***************************************************************************
 * is_search_file tests
 **************************************************************************/
TEST_CASE("Test is_search_file no exts no patterns should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file matches in-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_extension("cs");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file does not match in-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_extension("java");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file matches out-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_extension("cs");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file does not match out-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_extension("java");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file matches in-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_file_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_file("Searcher.cs"));
}

TEST_CASE("Test is_search_file does not match in-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_file_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_file("FileUtil.cs"));
}

TEST_CASE("Test is_search_file matches out-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_file_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_search_file("Searcher.cs"));
}

TEST_CASE("Test is_search_file does not match out-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_file_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_search_file("FileUtil.cs"));
}

/***************************************************************************
 * is_archive_search_file tests
 **************************************************************************/
TEST_CASE("Test is_archive_search_file no exts no patterns should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file matches in-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_archive_extension("zip");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file does not match in-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_archive_extension("gz");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file matches out-ext should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_archive_extension("zip");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file does not match out-ext should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_archive_extension("gz");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file matches in-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_archive_file_pattern("arch");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file does not match in-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_in_archive_file_pattern("archives");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file matches out-pattern should be false", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_archive_file_pattern("arch");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(!searcher->is_archive_search_file("archive.zip"));
}

TEST_CASE("Test is_archive_search_file does not match out-pattern should be true", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = ".";
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->add_out_archive_file_pattern("archives");
    auto *searcher = new cppsearch::Searcher(settings);

    REQUIRE(searcher->is_archive_search_file("archive.zip"));
}

/***************************************************************************
 * search tests
 **************************************************************************/
TEST_CASE("Test search with test file start_path", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
    REQUIRE(results[1]->line_num() == 35);
    REQUIRE(results[1]->match_start_idx() == 24);
    REQUIRE(results[1]->match_end_idx() == 32);
}

TEST_CASE("Test search with in lines before matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->lines_before(2);
    settings->add_in_lines_before_pattern("FileUtil");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test search with out lines before matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->lines_before(2);
    settings->add_out_lines_before_pattern("FileUtil");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test search with in lines after matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->lines_after(2);
    settings->add_in_lines_after_pattern("Settings");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test search with out lines after matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->lines_after(2);
    settings->add_out_lines_after_pattern("Settings");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test multiline search with test file start_path", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->multi_line_search(true);
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
    REQUIRE(results[1]->line_num() == 35);
    REQUIRE(results[1]->match_start_idx() == 24);
    REQUIRE(results[1]->match_end_idx() == 32);
}

TEST_CASE("Test multiline search with in lines before matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->multi_line_search(true);
    settings->lines_before(2);
    settings->add_in_lines_before_pattern("FileUtil");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test multiline search with out lines before matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->multi_line_search(true);
    settings->lines_before(2);
    settings->add_out_lines_before_pattern("FileUtil");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test multiline search with in lines after matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->multi_line_search(true);
    settings->lines_after(2);
    settings->add_in_lines_after_pattern("Settings");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test multiline search with out lines after matching", "[Searcher]") {
    auto *settings = new cppsearch::SearchSettings();
    std::string start_path = std::string(XSEARCHPATH);
    start_path.append("/shared/testFiles/testFile2.txt");
    settings->add_path(start_path);
    settings->add_search_pattern("Searcher");
    settings->multi_line_search(true);
    settings->lines_after(2);
    settings->add_out_lines_after_pattern("Settings");
    auto *searcher = new cppsearch::Searcher(settings);

    auto results = searcher->search();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->line_num() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}
