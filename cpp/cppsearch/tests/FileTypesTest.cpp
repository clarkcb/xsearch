#include <catch2/catch.hpp>
#include "FileTypes.h"

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    auto* file_types = new cppsearch::FileTypes();
    REQUIRE(file_types->is_archive_file("archive.zip"));
    REQUIRE(file_types->is_binary_file("binary.exe"));
    REQUIRE(file_types->is_text_file("textfile.txt"));
    REQUIRE(file_types->is_code_file("source.cpp"));
    REQUIRE(file_types->is_xml_file("markup.xml"));
    REQUIRE(file_types->is_searchable_file("markup.xml"));
    REQUIRE(file_types->is_unknown_file("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_file_type returns the expected type", "[FileType]") {
    auto* file_types = new cppsearch::FileTypes();
    REQUIRE(file_types->get_file_type("archive.zip") == cppsearch::FileType::ARCHIVE);
    REQUIRE(file_types->get_file_type("binary.exe") == cppsearch::FileType::BINARY);
    REQUIRE(file_types->get_file_type("textfile.txt") == cppsearch::FileType::TEXT);
    REQUIRE(file_types->get_file_type("source.cpp") == cppsearch::FileType::CODE);
    REQUIRE(file_types->get_file_type("markup.xml") == cppsearch::FileType::XML);
    REQUIRE(file_types->get_file_type("unknown.UNKNOWN") == cppsearch::FileType::UNKNOWN);
}
