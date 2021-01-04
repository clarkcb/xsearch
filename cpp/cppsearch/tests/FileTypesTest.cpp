#include <catch2/catch.hpp>
#include "FileTypes.h"

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    auto* filetypes = new cppsearch::FileTypes();
    REQUIRE(filetypes->is_archive_file("archive.zip"));
    REQUIRE(filetypes->is_binary_file("binary.exe"));
    REQUIRE(filetypes->is_text_file("textfile.txt"));
    REQUIRE(filetypes->is_code_file("source.cpp"));
    REQUIRE(filetypes->is_xml_file("markup.xml"));
    REQUIRE(filetypes->is_searchable_file("markup.xml"));
    REQUIRE(filetypes->is_unknown_file("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_filetype returns the expected type", "[FileType]") {
    auto* filetypes = new cppsearch::FileTypes();
    REQUIRE(filetypes->get_filetype("archive.zip") == cppsearch::FileType::ARCHIVE);
    REQUIRE(filetypes->get_filetype("binary.exe") == cppsearch::FileType::BINARY);
    REQUIRE(filetypes->get_filetype("textfile.txt") == cppsearch::FileType::TEXT);
    REQUIRE(filetypes->get_filetype("source.cpp") == cppsearch::FileType::CODE);
    REQUIRE(filetypes->get_filetype("markup.xml") == cppsearch::FileType::XML);
    REQUIRE(filetypes->get_filetype("unknown.UNKNOWN") == cppsearch::FileType::UNKNOWN);
}
