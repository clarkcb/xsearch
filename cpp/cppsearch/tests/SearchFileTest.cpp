#include <catch2/catch.hpp>
#include "SearchFile.h"

TEST_CASE("Verify fullpath searchfile string equals expected", "[SearchFile]") {
    std::string path = "/Users/cary/src/xsearch/cpp/cppsearch/src";
    std::string filename = "Searcher.cpp";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::CODE);

    REQUIRE(searchfile->path() == "/Users/cary/src/xsearch/cpp/cppsearch/src");
    REQUIRE(searchfile->filename() == "Searcher.cpp");
    REQUIRE(searchfile->filetype() == cppsearch::FileType::CODE);
    REQUIRE(searchfile->string() == "/Users/cary/src/xsearch/cpp/cppsearch/src/Searcher.cpp");
}

TEST_CASE("Verify relative path searchfile string equals expected", "[SearchFile]") {
    std::string path = ".";
    std::string filename = "Searcher.cpp";
    auto *searchfile = new cppsearch::SearchFile(path, filename, cppsearch::FileType::CODE);

    REQUIRE(searchfile->path() == ".");
    REQUIRE(searchfile->filename() == "Searcher.cpp");
    REQUIRE(searchfile->filetype() == cppsearch::FileType::CODE);
    REQUIRE(searchfile->string() == "./Searcher.cpp");
}
