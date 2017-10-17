#include <config.h>
#include "catch.hpp"
#include "FileUtil.h"

TEST_CASE("Expand paths", "[FileUtil]") {
    REQUIRE(FileUtil::expand_path("filename.txt") == "filename.txt");
    REQUIRE(FileUtil::expand_path("./filename.txt") == "./filename.txt");
    REQUIRE(FileUtil::expand_path("/Users/cary/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(FileUtil::expand_path("~/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(FileUtil::expand_path("~") == "/Users/cary");
    REQUIRE(FileUtil::expand_path("~/") == "/Users/cary/");
}

TEST_CASE("Detect file existence", "[FileUtil]") {
    REQUIRE(FileUtil::file_exists(std::string(XSEARCHPATH) + "/shared/config.json"));
    REQUIRE(!FileUtil::file_exists(std::string(XSEARCHPATH) + "/nonexistant.txt"));
}

TEST_CASE("Get extensions from filenames", "[FileUtil]") {
    REQUIRE(FileUtil::get_extension("filename.txt") == "txt");
    REQUIRE(FileUtil::get_extension("filename.").empty());
    REQUIRE(FileUtil::get_extension("filename").empty());
    REQUIRE(FileUtil::get_extension(".filename.txt") == "txt");
    REQUIRE(FileUtil::get_extension(".filename.").empty());
    REQUIRE(FileUtil::get_extension(".filename").empty());
}

TEST_CASE("Detect directories", "[FileUtil]") {
    REQUIRE(FileUtil::is_directory("."));
    REQUIRE(FileUtil::is_directory(".."));
    REQUIRE(FileUtil::is_directory("./"));
    REQUIRE(FileUtil::is_directory("../"));
    REQUIRE(FileUtil::is_directory(std::string(XSEARCHPATH)));
    REQUIRE(!FileUtil::is_directory(std::string(XSEARCHPATH) + "/shared/config.json"));
}

TEST_CASE("Detect dot dirs", "[FileUtil]") {
    REQUIRE(FileUtil::is_dot_dir("."));
    REQUIRE(FileUtil::is_dot_dir(".."));
    REQUIRE(FileUtil::is_dot_dir("./"));
    REQUIRE(FileUtil::is_dot_dir("../"));
    REQUIRE(!FileUtil::is_dot_dir("./path"));
    REQUIRE(!FileUtil::is_dot_dir("../path"));
    REQUIRE(!FileUtil::is_dot_dir(".gitignore"));
}

TEST_CASE("Detect hidden files", "[FileUtil]") {
    REQUIRE(!FileUtil::is_hidden("."));
    REQUIRE(!FileUtil::is_hidden(".."));
    REQUIRE(!FileUtil::is_hidden("./"));
    REQUIRE(!FileUtil::is_hidden("../"));
//    REQUIRE(!FileUtil::is_hidden("./path"));
//    REQUIRE(!FileUtil::is_hidden("../path"));
    REQUIRE(FileUtil::is_hidden(".gitignore"));
    REQUIRE(!FileUtil::is_hidden("filename.txt"));
}

TEST_CASE("Split paths", "[FileUtil]") {
    REQUIRE(FileUtil::split_path(".").size() == 1);
    REQUIRE(FileUtil::split_path(".")[0] == ".");
    REQUIRE(FileUtil::split_path("./").size() == 1);
    REQUIRE(FileUtil::split_path("./")[0] == ".");
    REQUIRE(FileUtil::split_path("./path").size() == 2);
    REQUIRE(FileUtil::split_path("./path")[0] == ".");
    REQUIRE(FileUtil::split_path("./path")[1] == "path");
    REQUIRE(FileUtil::split_path("./path/").size() == 2);
    REQUIRE(FileUtil::split_path("./path/")[0] == ".");
    REQUIRE(FileUtil::split_path("./path/")[1] == "path");
}
