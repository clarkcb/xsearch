#include <config.h>
#include <catch2/catch.hpp>
#include "FileUtil.h"

TEST_CASE("Expand paths", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::expand_path("filename.txt") == "filename.txt");
    REQUIRE(cppsearch::FileUtil::expand_path("./filename.txt") == "./filename.txt");
    REQUIRE(cppsearch::FileUtil::expand_path("/Users/cary/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(cppsearch::FileUtil::expand_path("~/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(cppsearch::FileUtil::expand_path("~") == "/Users/cary");
    REQUIRE(cppsearch::FileUtil::expand_path("~/") == "/Users/cary/");
}

TEST_CASE("Detect file existence", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::file_exists(std::string(XSEARCHPATH) + "/shared/config.json"));
    REQUIRE(!cppsearch::FileUtil::file_exists(std::string(XSEARCHPATH) + "/nonexistant.txt"));
}

TEST_CASE("Get extensions from filenames", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::get_extension("filename.txt") == "txt");
    REQUIRE(cppsearch::FileUtil::get_extension("filename.").empty());
    REQUIRE(cppsearch::FileUtil::get_extension("filename").empty());
    REQUIRE(cppsearch::FileUtil::get_extension(".filename.txt") == "txt");
    REQUIRE(cppsearch::FileUtil::get_extension(".filename.").empty());
    REQUIRE(cppsearch::FileUtil::get_extension(".filename").empty());
}

TEST_CASE("Detect directories", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::is_directory("."));
    REQUIRE(cppsearch::FileUtil::is_directory(".."));
    REQUIRE(cppsearch::FileUtil::is_directory("./"));
    REQUIRE(cppsearch::FileUtil::is_directory("../"));
    REQUIRE(cppsearch::FileUtil::is_directory(std::string(XSEARCHPATH)));
    REQUIRE(!cppsearch::FileUtil::is_directory(std::string(XSEARCHPATH) + "/shared/config.json"));
}

TEST_CASE("Detect dot dirs", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::is_dot_dir("."));
    REQUIRE(cppsearch::FileUtil::is_dot_dir(".."));
    REQUIRE(cppsearch::FileUtil::is_dot_dir("./"));
    REQUIRE(cppsearch::FileUtil::is_dot_dir("../"));
    REQUIRE(!cppsearch::FileUtil::is_dot_dir("./path"));
    REQUIRE(!cppsearch::FileUtil::is_dot_dir("../path"));
    REQUIRE(!cppsearch::FileUtil::is_dot_dir(".gitignore"));
}

TEST_CASE("Detect hidden files", "[FileUtil]") {
    REQUIRE(!cppsearch::FileUtil::is_hidden("."));
    REQUIRE(!cppsearch::FileUtil::is_hidden(".."));
    REQUIRE(!cppsearch::FileUtil::is_hidden("./"));
    REQUIRE(!cppsearch::FileUtil::is_hidden("../"));
//    REQUIRE(!cppsearch::FileUtil::is_hidden("./path"));
//    REQUIRE(!cppsearch::FileUtil::is_hidden("../path"));
    REQUIRE(cppsearch::FileUtil::is_hidden(".gitignore"));
    REQUIRE(!cppsearch::FileUtil::is_hidden("filename.txt"));
}

TEST_CASE("Split paths", "[FileUtil]") {
    REQUIRE(cppsearch::FileUtil::split_path(".").size() == 1);
    REQUIRE(cppsearch::FileUtil::split_path(".")[0] == ".");
    REQUIRE(cppsearch::FileUtil::split_path("./").size() == 1);
    REQUIRE(cppsearch::FileUtil::split_path("./")[0] == ".");
    REQUIRE(cppsearch::FileUtil::split_path("./path").size() == 2);
    REQUIRE(cppsearch::FileUtil::split_path("./path")[0] == ".");
    REQUIRE(cppsearch::FileUtil::split_path("./path")[1] == "path");
    REQUIRE(cppsearch::FileUtil::split_path("./path/").size() == 2);
    REQUIRE(cppsearch::FileUtil::split_path("./path/")[0] == ".");
    REQUIRE(cppsearch::FileUtil::split_path("./path/")[1] == "path");
}
