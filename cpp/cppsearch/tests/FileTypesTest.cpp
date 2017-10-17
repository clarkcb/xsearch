#include "catch.hpp"
#include "FileTypes.h"

auto* fileTypes = new FileTypes();

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    REQUIRE(fileTypes->is_archive_file("archive.zip"));
    REQUIRE(fileTypes->is_binary_file("binary.exe"));
    REQUIRE(fileTypes->is_text_file("textfile.txt"));
    REQUIRE(fileTypes->is_code_file("source.cpp"));
    REQUIRE(fileTypes->is_xml_file("markup.xml"));
    REQUIRE(fileTypes->is_searchable_file("markup.xml"));
    REQUIRE(fileTypes->is_unknown_file("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_filetype returns the expected type", "[FileType]") {
    REQUIRE(fileTypes->get_filetype("archive.zip") == FileType::ARCHIVE);
    REQUIRE(fileTypes->get_filetype("binary.exe") == FileType::BINARY);
    REQUIRE(fileTypes->get_filetype("textfile.txt") == FileType::TEXT);
    REQUIRE(fileTypes->get_filetype("source.cpp") == FileType::CODE);
    REQUIRE(fileTypes->get_filetype("markup.xml") == FileType::XML);
    REQUIRE(fileTypes->get_filetype("unknown.UNKNOWN") == FileType::UNKNOWN);
}

