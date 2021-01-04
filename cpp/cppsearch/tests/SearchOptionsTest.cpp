#include <catch2/catch.hpp>
#include "SearchOptions.h"

TEST_CASE("Get SearchSettings from minimal args", "[FileType]") {
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

    auto* startpath = settings->startpath();
    REQUIRE(*startpath == ".");
}

TEST_CASE("Get SearchSettings from valid args", "[FileType]") {
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

    auto* startpath = settings->startpath();
    REQUIRE(*startpath == ".");
}
