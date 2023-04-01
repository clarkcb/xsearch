#include <catch2/catch.hpp>
#include "SearchOptions.h"

TEST_CASE("Get SearchSettings from minimal args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-s"), const_cast<char *>("Searcher"),
                     const_cast<char *>(".") };
    int argc = 4;
    cppsearch::SearchSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->firstmatch());
    REQUIRE((settings->linesafter() == 0));
    REQUIRE((settings->linesbefore() == 0));
    REQUIRE(!settings->listdirs());
    REQUIRE(!settings->listfiles());
    REQUIRE(!settings->listlines());
    REQUIRE((settings->maxlinelength() == 150));
    REQUIRE(!settings->multilinesearch());
    REQUIRE(settings->printresults());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->searcharchives());
    REQUIRE(!settings->uniquelines());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archiveextensions()->empty());
    REQUIRE(settings->in_archivefilepatterns()->empty());
    REQUIRE(settings->in_dirpatterns()->empty());
    REQUIRE(settings->in_extensions()->empty());
    REQUIRE(settings->in_filepatterns()->empty());
    REQUIRE(settings->out_archiveextensions()->empty());
    REQUIRE(settings->out_archivefilepatterns()->empty());
    REQUIRE(settings->out_dirpatterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_filepatterns()->empty());

    std::vector<cppsearch::SearchPattern*>* searchpatterns = settings->searchpatterns();
    REQUIRE(searchpatterns->size() == 1);
    REQUIRE(searchpatterns->at(0)->pattern() == "Searcher");

    REQUIRE(!settings->paths()->empty());
    REQUIRE(settings->paths()->at(0) == ".");
}

TEST_CASE("Get SearchSettings from valid args", "[SearchOptions]") {
    auto* options = new cppsearch::SearchOptions();
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>(".") };
    int argc = 6;
    cppsearch::SearchSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->firstmatch());
    REQUIRE((settings->linesafter() == 0));
    REQUIRE((settings->linesbefore() == 0));
    REQUIRE(!settings->listdirs());
    REQUIRE(!settings->listfiles());
    REQUIRE(!settings->listlines());
    REQUIRE((settings->maxlinelength() == 150));
    REQUIRE(!settings->multilinesearch());
    REQUIRE(settings->printresults());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->searcharchives());
    REQUIRE(!settings->uniquelines());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archiveextensions()->empty());
    REQUIRE(settings->in_archivefilepatterns()->empty());
    REQUIRE(settings->in_dirpatterns()->empty());
    REQUIRE(settings->in_filepatterns()->empty());
    REQUIRE(settings->out_archiveextensions()->empty());
    REQUIRE(settings->out_archivefilepatterns()->empty());
    REQUIRE(settings->out_dirpatterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_filepatterns()->empty());

    std::vector<std::string>* in_exts = settings->in_extensions();
    REQUIRE(in_exts->size() == 2);
    REQUIRE(in_exts->at(0) == "java");
    REQUIRE(in_exts->at(1) == "scala");

    std::vector<cppsearch::SearchPattern*>* searchpatterns = settings->searchpatterns();
    REQUIRE(searchpatterns->size() == 1);
    REQUIRE(searchpatterns->at(0)->pattern() == "Searcher");

    REQUIRE(!settings->paths()->empty());
    REQUIRE(settings->paths()->at(0) == ".");
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
    REQUIRE(settings->out_dirpatterns()->size() == 4);
    REQUIRE(settings->out_dirpatterns()->at(0)->pattern() == "build");
    REQUIRE(settings->out_dirpatterns()->at(1)->pattern() == "node_module");
    REQUIRE(settings->out_dirpatterns()->at(2)->pattern() == "tests");
    REQUIRE(settings->out_dirpatterns()->at(3)->pattern() == "typings");
    REQUIRE(settings->out_filepatterns()->size() == 2);
    REQUIRE(settings->out_filepatterns()->at(0)->pattern() == "gulpfile");
    REQUIRE(settings->out_filepatterns()->at(1)->pattern() == "\\.min\\.");
    REQUIRE(settings->searchpatterns()->size() == 1);
    REQUIRE(settings->searchpatterns()->at(0)->pattern() == "Searcher");
    REQUIRE(settings->linesbefore() == 2);
    REQUIRE(settings->linesafter() == 2);
    REQUIRE(settings->debug() == true);
    REQUIRE(settings->firstmatch() == true);
    REQUIRE(settings->excludehidden() == true);
}
