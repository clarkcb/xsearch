#include <catch2/catch.hpp>
#include "SearchOptions.h"

TEST_CASE("Get SearchSettings from minimal args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-s"), const_cast<char *>("Searcher"),
                     const_cast<char *>(".") };
    int argc = 4;
    cppsearch::SearchSettings* settings = options->settings_from_args(argc, argv);
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
    REQUIRE(settings->print_results());
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

    std::vector<cppfind::RegexPattern*>* search_patterns = settings->search_patterns();
    REQUIRE(search_patterns->size() == 1);
    REQUIRE(search_patterns->at(0)->pattern() == "Searcher");

    REQUIRE(!settings->paths()->empty());
    REQUIRE(settings->paths()->at(0) == ".");
}

TEST_CASE("Get SearchSettings from valid args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>(".") };
    int argc = 6;
    cppsearch::SearchSettings* settings = options->settings_from_args(argc, argv);
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
    REQUIRE(settings->print_results());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->search_archives());
    REQUIRE(!settings->unique_lines());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions()->empty());
    REQUIRE(settings->in_archive_file_patterns()->empty());
    REQUIRE(settings->in_dir_patterns()->empty());
    REQUIRE(settings->in_file_patterns()->empty());
    REQUIRE(settings->out_archive_extensions()->empty());
    REQUIRE(settings->out_archive_file_patterns()->empty());
    REQUIRE(settings->out_dir_patterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_file_patterns()->empty());

    std::vector<std::string>* in_exts = settings->in_extensions();
    REQUIRE(in_exts->size() == 2);
    REQUIRE(in_exts->at(0) == "java");
    REQUIRE(in_exts->at(1) == "scala");

    std::vector<cppfind::RegexPattern*>* search_patterns = settings->search_patterns();
    REQUIRE(search_patterns->size() == 1);
    REQUIRE(search_patterns->at(0)->pattern() == "Searcher");

    REQUIRE(!settings->paths()->empty());
    REQUIRE(settings->paths()->at(0) == ".");
}

TEST_CASE("Get SearchSettings with archives-only", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>("--archivesonly"),
                     const_cast<char *>(".") };
    int argc = 7;
    cppsearch::SearchSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(settings->archives_only());
    REQUIRE(settings->search_archives());
}

TEST_CASE("Get SearchSettings from JSON", "[SearchOptions]") {
    std::string json = R"(
{
    "path": "~/src/xsearch/",
    "in-ext": ["js","ts"],
    "out-dirpattern": ["build", "node_module", "tests", "typings"],
    "out-filepattern": ["gulpfile", "\\.min\\."],
    "searchpattern": "Searcher",
    "linesbefore": 2,
    "linesafter": 2,
    "debug": true,
    "allmatches": false,
    "includehidden": false
}
)";

    auto *options = new cppsearch::SearchOptions();
    auto *settings = new cppsearch::SearchSettings();
    options->settings_from_json(json, settings);

    REQUIRE(!settings->paths()->empty());
    REQUIRE(settings->paths()->at(0) == "~/src/xsearch/");
    REQUIRE(settings->in_extensions()->size() == 2);
    REQUIRE(settings->in_extensions()->at(0) == "js");
    REQUIRE(settings->in_extensions()->at(1) == "ts");
    REQUIRE(settings->out_dir_patterns()->size() == 4);
    REQUIRE(settings->out_dir_patterns()->at(0)->pattern() == "build");
    REQUIRE(settings->out_dir_patterns()->at(1)->pattern() == "node_module");
    REQUIRE(settings->out_dir_patterns()->at(2)->pattern() == "tests");
    REQUIRE(settings->out_dir_patterns()->at(3)->pattern() == "typings");
    REQUIRE(settings->out_file_patterns()->size() == 2);
    REQUIRE(settings->out_file_patterns()->at(0)->pattern() == "gulpfile");
    REQUIRE(settings->out_file_patterns()->at(1)->pattern() == "\\.min\\.");
    REQUIRE(settings->search_patterns()->size() == 1);
    REQUIRE(settings->search_patterns()->at(0)->pattern() == "Searcher");
    REQUIRE(settings->lines_before() == 2);
    REQUIRE(settings->lines_after() == 2);
    REQUIRE(settings->debug() == true);
    REQUIRE(settings->first_match() == true);
    REQUIRE(settings->exclude_hidden() == true);
}
