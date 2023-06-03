#include <catch2/catch.hpp>
#include "SearchFile.h"

TEST_CASE("Verify fullpath search file string equals expected", "[SearchFile]") {
    std::string path = "/Users/cary/src/xsearch/cpp/cppsearch/src";
    std::string file_name = "Searcher.cpp";
    auto *search_file = new cppsearch::SearchFile(path, file_name, cppsearch::FileType::CODE);

    REQUIRE(search_file->path() == "/Users/cary/src/xsearch/cpp/cppsearch/src");
    REQUIRE(search_file->file_name() == "Searcher.cpp");
    REQUIRE(search_file->file_type() == cppsearch::FileType::CODE);
    REQUIRE(search_file->string() == "/Users/cary/src/xsearch/cpp/cppsearch/src/Searcher.cpp");
}

TEST_CASE("Verify relative path search file string equals expected", "[SearchFile]") {
    std::string path = ".";
    std::string file_name = "Searcher.cpp";
    auto *search_file = new cppsearch::SearchFile(path, file_name, cppsearch::FileType::CODE);

    REQUIRE(search_file->path() == ".");
    REQUIRE(search_file->file_name() == "Searcher.cpp");
    REQUIRE(search_file->file_type() == cppsearch::FileType::CODE);
    REQUIRE(search_file->string() == "./Searcher.cpp");
}
