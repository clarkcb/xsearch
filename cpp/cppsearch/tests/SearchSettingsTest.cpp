#include <catch2/catch.hpp>
#include "SearchSettings.h"

TEST_CASE("Get default SearchSettings", "[SearchSettings]") {
    auto* settings = new cppsearch::SearchSettings();

    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->debug());
    REQUIRE(settings->exclude_hidden());
    REQUIRE(!settings->first_match());
    REQUIRE((settings->lines_after() == 0));
    REQUIRE((settings->lines_before() == 0));
    REQUIRE(!settings->list_dirs());
    REQUIRE(!settings->list_files());
    REQUIRE(!settings->list_lines());
    REQUIRE((settings->max_line_length() == 150));
    REQUIRE(!settings->multi_line_search());
    REQUIRE(!settings->print_results());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->search_archives());
    REQUIRE(!settings->unique_lines());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions()->empty());
    REQUIRE(settings->in_archive_file_patterns()->empty());
    REQUIRE(settings->in_dir_patterns()->empty());
    REQUIRE(settings->in_extensions()->empty());
    REQUIRE(settings->in_file_patterns()->empty());
    REQUIRE(settings->out_archive_extensions()->empty());
    REQUIRE(settings->out_archive_file_patterns()->empty());
    REQUIRE(settings->out_dir_patterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_file_patterns()->empty());
    REQUIRE(settings->paths()->empty());
    REQUIRE(settings->search_patterns()->empty());
}

TEST_CASE("Add extensions to SearchSettings", "[SearchSettings]") {
    auto *settings = new cppsearch::SearchSettings();

    REQUIRE(settings->in_archive_extensions()->empty());
    settings->add_in_archive_extension("zip,gz");
    REQUIRE(settings->in_archive_extensions()->size() == 2);

    REQUIRE(settings->out_archive_extensions()->empty());
    settings->add_out_archive_extension("rar,");
    REQUIRE(settings->out_archive_extensions()->size() == 1);

    REQUIRE(settings->in_extensions()->empty());
    settings->add_in_extension("cpp,h");
    REQUIRE(settings->in_extensions()->size() == 2);

    REQUIRE(settings->out_extensions()->empty());
    settings->add_out_extension("a,o");
    REQUIRE(settings->out_extensions()->size() == 2);
}

TEST_CASE("Add patterns to SearchSettings", "[SearchSettings]") {
    auto *settings = new cppsearch::SearchSettings();

    REQUIRE(settings->in_archive_file_patterns()->empty());
    settings->add_in_archive_file_pattern("archive");
    REQUIRE(settings->in_archive_file_patterns()->size() == 1);

    REQUIRE(settings->out_archive_file_patterns()->empty());
    settings->add_out_archive_file_pattern("old");
    REQUIRE(settings->out_archive_file_patterns()->size() == 1);

    REQUIRE(settings->in_dir_patterns()->empty());
    settings->add_in_dir_pattern("dir");
    REQUIRE(settings->in_dir_patterns()->size() == 1);

    REQUIRE(settings->out_dir_patterns()->empty());
    settings->add_out_dir_pattern("tmp");
    REQUIRE(settings->out_dir_patterns()->size() == 1);

    REQUIRE(settings->in_file_patterns()->empty());
    settings->add_in_file_pattern("file");
    REQUIRE(settings->in_file_patterns()->size() == 1);

    REQUIRE(settings->out_file_patterns()->empty());
    settings->add_out_file_pattern("stream");
    REQUIRE(settings->out_file_patterns()->size() == 1);
}

TEST_CASE("Alter booleans in SearchSettings", "[SearchSettings]") {
    auto *settings = new cppsearch::SearchSettings();

    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->search_archives());
    settings->archives_only(true);
    REQUIRE(settings->archives_only());
    REQUIRE(settings->search_archives());

    REQUIRE(!settings->debug());
    REQUIRE(!settings->verbose());
    settings->debug(true);
    REQUIRE(settings->debug());
    REQUIRE(settings->verbose());

    REQUIRE(settings->exclude_hidden());
    settings->exclude_hidden(false);
    REQUIRE(!settings->exclude_hidden());

    REQUIRE(!settings->first_match());
    settings->first_match(true);
    REQUIRE(settings->first_match());

    REQUIRE(!settings->multi_line_search());
    settings->multi_line_search(true);
    REQUIRE(settings->multi_line_search());

    REQUIRE(!settings->list_dirs());
    settings->list_dirs(true);
    REQUIRE(settings->list_dirs());

    REQUIRE(!settings->list_files());
    settings->list_files(true);
    REQUIRE(settings->list_files());

    REQUIRE(!settings->list_lines());
    settings->list_lines(true);
    REQUIRE(settings->list_lines());

    REQUIRE(!settings->print_results());
    settings->print_results(true);
    REQUIRE(settings->print_results());

    REQUIRE(!settings->print_usage());
    settings->print_usage(true);
    REQUIRE(settings->print_usage());

    REQUIRE(!settings->print_version());
    settings->print_version(true);
    REQUIRE(settings->print_version());

    REQUIRE(settings->recursive());
    settings->recursive(false);
    REQUIRE(!settings->recursive());

    REQUIRE(!settings->unique_lines());
    settings->unique_lines(true);
    REQUIRE(settings->unique_lines());
}

TEST_CASE("Alter ints in SearchSettings", "[SearchSettings]") {
    auto *settings = new cppsearch::SearchSettings();

    REQUIRE(settings->lines_before() == 0);
    settings->lines_before(5);
    REQUIRE(settings->lines_before() == 5);

    // TODO: fix handling of negative values
    //m_settings->lines_before(-5);
    //REQUIRE(m_settings->lines_before() == 5);

    REQUIRE(settings->lines_after() == 0);
    settings->lines_after(5);
    REQUIRE(settings->lines_after() == 5);

    // TODO: fix handling of negative values
    //m_settings->lines_after(-5);
    //REQUIRE(m_settings->lines_after() == 5);
}
