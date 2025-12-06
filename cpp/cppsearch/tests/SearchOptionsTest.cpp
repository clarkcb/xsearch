#include <algorithm>
#include <catch2/catch_all.hpp>
#include <unordered_set>
#include "SearchOptions.h"

TEST_CASE("Get SearchSettings from minimal args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-s"), const_cast<char *>("Searcher"),
                     const_cast<char *>(".") };
    int argc = 4;
    auto settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings.archives_only());
    REQUIRE(!settings.debug());
    REQUIRE(!settings.first_match());
    REQUIRE(!settings.include_hidden());
    REQUIRE((settings.lines_after() == 0));
    REQUIRE((settings.lines_before() == 0));
    REQUIRE((settings.max_line_length() == 150));
    REQUIRE(!settings.multi_line_search());
    REQUIRE(!settings.print_dirs());
    REQUIRE(!settings.print_files());
    REQUIRE(!settings.print_lines());
    REQUIRE(settings.print_results());
    REQUIRE(!settings.print_usage());
    REQUIRE(!settings.print_version());
    REQUIRE(!settings.search_archives());
    REQUIRE(!settings.unique_lines());
    REQUIRE(!settings.verbose());

    REQUIRE(settings.in_archive_extensions().empty());
    REQUIRE(settings.in_archive_file_patterns().empty());
    REQUIRE(settings.in_dir_patterns().empty());
    REQUIRE(settings.in_extensions().empty());
    REQUIRE(settings.in_file_patterns().empty());
    REQUIRE(settings.out_archive_extensions().empty());
    REQUIRE(settings.out_archive_file_patterns().empty());
    REQUIRE(settings.out_dir_patterns().empty());
    REQUIRE(settings.out_extensions().empty());
    REQUIRE(settings.out_file_patterns().empty());

    auto search_patterns = settings.search_patterns();
    REQUIRE(search_patterns.size() == 1);
    std::unordered_set<std::string> expected_patterns{"Searcher"};
    auto contains_pattern = [&](auto& p) {
        return expected_patterns.contains(p.pattern());
    };
    REQUIRE(std::all_of(search_patterns.cbegin(), search_patterns.cend(), contains_pattern));

    REQUIRE(!settings.paths().empty());
    REQUIRE(settings.paths().size() == 1);
    REQUIRE(settings.paths().contains("."));
}

TEST_CASE("Get SearchSettings from valid args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>(".") };
    int argc = 6;
    auto settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings.archives_only());
    REQUIRE(!settings.debug());
    REQUIRE(!settings.first_match());
    REQUIRE(settings.include_hidden());
    REQUIRE((settings.lines_after() == 0));
    REQUIRE((settings.lines_before() == 0));
    REQUIRE((settings.max_line_length() == 150));
    REQUIRE(!settings.multi_line_search());
    REQUIRE(!settings.print_dirs());
    REQUIRE(!settings.print_files());
    REQUIRE(!settings.print_lines());
    REQUIRE(settings.print_results());
    REQUIRE(!settings.print_usage());
    REQUIRE(!settings.print_version());
    REQUIRE(!settings.search_archives());
    REQUIRE(!settings.unique_lines());
    REQUIRE(!settings.verbose());

    REQUIRE(settings.in_archive_extensions().empty());
    REQUIRE(settings.in_archive_file_patterns().empty());
    REQUIRE(settings.in_dir_patterns().empty());
    REQUIRE(settings.in_file_patterns().empty());
    REQUIRE(settings.out_archive_extensions().empty());
    REQUIRE(settings.out_archive_file_patterns().empty());
    REQUIRE(settings.out_dir_patterns().empty());
    REQUIRE(settings.out_extensions().empty());
    REQUIRE(settings.out_file_patterns().empty());

    std::unordered_set<std::string> in_exts = settings.in_extensions();
    REQUIRE(in_exts.size() == 2);
    REQUIRE(in_exts.contains("java"));
    REQUIRE(in_exts.contains("scala"));

    auto search_patterns = settings.search_patterns();
    REQUIRE(search_patterns.size() == 1);
    std::unordered_set<std::string> expected_patterns{"Searcher"};
    auto contains_pattern = [&](auto& p) {
        return expected_patterns.contains(p.pattern());
    };
    REQUIRE(std::all_of(search_patterns.cbegin(), search_patterns.cend(), contains_pattern));

    REQUIRE(!settings.paths().empty());
    REQUIRE(settings.paths().size() == 1);
    REQUIRE(settings.paths().contains("."));
}

TEST_CASE("Get SearchSettings with archives-only", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>("--archivesonly"),
                     const_cast<char *>(".") };
    int argc = 7;
    auto settings = options->settings_from_args(argc, argv);
    REQUIRE(settings.archives_only());
    REQUIRE(settings.search_archives());
}

TEST_CASE("Get SearchSettings from JSON", "[SearchOptions]") {
    std::string json = R"(
{
    "out-dirpattern": ["build", "node_module", "tests", "typings"],
    "out-filepattern": ["gulpfile", "\\.min\\."],
    "path": "~/src/xsearch/",
    "in-ext": ["js","ts"],
    "searchpattern": "Searcher",
    "linesbefore": 2,
    "linesafter": 2,
    "debug": true,
    "allmatches": false,
    "includehidden": true
}
)";

    auto options = cppsearch::SearchOptions();
    auto settings = cppsearch::SearchSettings();
    options.update_settings_from_json(settings, json);

    REQUIRE(!settings.paths().empty());
    REQUIRE(settings.paths().size() == 1);
    REQUIRE(settings.paths().contains("~/src/xsearch/"));
    REQUIRE(settings.in_extensions().size() == 2);
    REQUIRE(settings.in_extensions().contains("js"));
    REQUIRE(settings.in_extensions().contains("ts"));

    auto out_dir_patterns = settings.out_dir_patterns();
    REQUIRE(out_dir_patterns.size() == 4);
    std::unordered_set<std::string> expected_out_dir_patterns{"build", "node_module", "tests", "typings"};
    auto contains_out_dir_pattern = [&](auto& p) {
        return expected_out_dir_patterns.contains(p.pattern());
    };
    REQUIRE(std::all_of(out_dir_patterns.cbegin(), out_dir_patterns.cend(), contains_out_dir_pattern));

    auto out_file_patterns = settings.out_file_patterns();
    REQUIRE(out_file_patterns.size() == 2);
    std::unordered_set<std::string> expected_out_file_patterns{"gulpfile", "\\.min\\."};
    auto contains_out_file_pattern = [&](auto& p) {
        return expected_out_file_patterns.contains(p.pattern());
    };
    REQUIRE(std::all_of(out_file_patterns.cbegin(), out_file_patterns.cend(), contains_out_file_pattern));

    auto search_patterns = settings.search_patterns();
    REQUIRE(search_patterns.size() == 1);
    std::unordered_set<std::string> expected_search_patterns{"Searcher"};
    auto contains_pattern = [&](auto& p) {
        return expected_search_patterns.contains(p.pattern());
    };
    REQUIRE(std::all_of(search_patterns.cbegin(), search_patterns.cend(), contains_pattern));

    REQUIRE(settings.lines_before() == 2);
    REQUIRE(settings.lines_after() == 2);
    REQUIRE(settings.debug() == true);
    REQUIRE(settings.first_match() == true);
    REQUIRE(settings.include_hidden() == true);
}
