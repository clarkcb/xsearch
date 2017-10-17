#include "catch.hpp"
#include "SearchOptions.h"

auto* options = new SearchOptions();

TEST_CASE("Get SearchSettings from minimal args", "[FileType]") {
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-s"), const_cast<char *>("Searcher"),
                     const_cast<char *>(".") };
    int argc = 4;
    SearchSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->get_archivesonly());
    REQUIRE(!settings->get_debug());
    REQUIRE(settings->get_excludehidden());
    REQUIRE(!settings->get_firstmatch());
    REQUIRE((settings->get_linesafter() == 0));
    REQUIRE((settings->get_linesbefore() == 0));
    REQUIRE(!settings->get_listdirs());
    REQUIRE(!settings->get_listfiles());
    REQUIRE(!settings->get_listlines());
    REQUIRE((settings->get_maxlinelength() == 150));
    REQUIRE(!settings->get_multilinesearch());
    REQUIRE(settings->get_printresults());
    REQUIRE(!settings->get_printusage());
    REQUIRE(!settings->get_printversion());
    REQUIRE(!settings->get_searcharchives());
    REQUIRE(!settings->get_uniquelines());
    REQUIRE(!settings->get_verbose());

    REQUIRE(settings->get_in_archiveextensions()->empty());
    REQUIRE(settings->get_in_archivefilepatterns()->empty());
    REQUIRE(settings->get_in_dirpatterns()->empty());
    REQUIRE(settings->get_in_extensions()->empty());
    REQUIRE(settings->get_in_filepatterns()->empty());
    REQUIRE(settings->get_out_archiveextensions()->empty());
    REQUIRE(settings->get_out_archivefilepatterns()->empty());
    REQUIRE(settings->get_out_dirpatterns()->empty());
    REQUIRE(settings->get_out_extensions()->empty());
    REQUIRE(settings->get_out_filepatterns()->empty());

    vector<SearchPattern*>* searchpatterns = settings->get_searchpatterns();
    REQUIRE(searchpatterns->size() == 1);
    REQUIRE(*(searchpatterns->at(0)->pattern) == "Searcher");

    auto* startpath = settings->get_startpath();
    REQUIRE(*startpath == ".");
}

TEST_CASE("Get SearchSettings from valid args", "[FileType]") {
    char* argv[] = { const_cast<char *>("cppsearch"), const_cast<char *>("-x"), const_cast<char *>("java,scala"),
                     const_cast<char *>("-s"), const_cast<char *>("Searcher"), const_cast<char *>(".") };
    int argc = 6;
    SearchSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->get_archivesonly());
    REQUIRE(!settings->get_debug());
    REQUIRE(settings->get_excludehidden());
    REQUIRE(!settings->get_firstmatch());
    REQUIRE((settings->get_linesafter() == 0));
    REQUIRE((settings->get_linesbefore() == 0));
    REQUIRE(!settings->get_listdirs());
    REQUIRE(!settings->get_listfiles());
    REQUIRE(!settings->get_listlines());
    REQUIRE((settings->get_maxlinelength() == 150));
    REQUIRE(!settings->get_multilinesearch());
    REQUIRE(settings->get_printresults());
    REQUIRE(!settings->get_printusage());
    REQUIRE(!settings->get_printversion());
    REQUIRE(!settings->get_searcharchives());
    REQUIRE(!settings->get_uniquelines());
    REQUIRE(!settings->get_verbose());

    REQUIRE(settings->get_in_archiveextensions()->empty());
    REQUIRE(settings->get_in_archivefilepatterns()->empty());
    REQUIRE(settings->get_in_dirpatterns()->empty());
    REQUIRE(settings->get_in_filepatterns()->empty());
    REQUIRE(settings->get_out_archiveextensions()->empty());
    REQUIRE(settings->get_out_archivefilepatterns()->empty());
    REQUIRE(settings->get_out_dirpatterns()->empty());
    REQUIRE(settings->get_out_extensions()->empty());
    REQUIRE(settings->get_out_filepatterns()->empty());

    vector<string>* in_exts = settings->get_in_extensions();
    REQUIRE(in_exts->size() == 2);
    REQUIRE(in_exts->at(0) == "java");
    REQUIRE(in_exts->at(1) == "scala");

    vector<SearchPattern*>* searchpatterns = settings->get_searchpatterns();
    REQUIRE(searchpatterns->size() == 1);
    REQUIRE(*(searchpatterns->at(0)->pattern) == "Searcher");

    auto* startpath = settings->get_startpath();
    REQUIRE(*startpath == ".");
}
