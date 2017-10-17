#include "catch.hpp"
#include "SearchSettings.h"

TEST_CASE("Get default SearchSettings", "[SearchSettings]") {
    auto* settings = new SearchSettings();

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
    REQUIRE(!settings->get_printresults());
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
    REQUIRE(settings->get_searchpatterns()->empty());
}

TEST_CASE("Add extensions to SearchSettings", "[SearchSettings]") {
    auto *settings = new SearchSettings();

    REQUIRE(settings->get_in_archiveextensions()->empty());
    settings->add_in_archiveextension(new string("zip,gz"));
    REQUIRE(settings->get_in_archiveextensions()->size() == 2);

    REQUIRE(settings->get_out_archiveextensions()->empty());
    settings->add_out_archiveextension(new string("rar,"));
    REQUIRE(settings->get_out_archiveextensions()->size() == 1);

    REQUIRE(settings->get_in_extensions()->empty());
    settings->add_in_extension(new string("cpp,h"));
    REQUIRE(settings->get_in_extensions()->size() == 2);

    REQUIRE(settings->get_out_extensions()->empty());
    settings->add_out_extension(new string("a,o"));
    REQUIRE(settings->get_out_extensions()->size() == 2);
}

TEST_CASE("Add patterns to SearchSettings", "[SearchSettings]") {
    auto *settings = new SearchSettings();

    REQUIRE(settings->get_in_archivefilepatterns()->empty());
    settings->add_in_archivefilepattern(new string("archive"));
    REQUIRE(settings->get_in_archivefilepatterns()->size() == 1);

    REQUIRE(settings->get_out_archivefilepatterns()->empty());
    settings->add_out_archivefilepattern(new string("old"));
    REQUIRE(settings->get_out_archivefilepatterns()->size() == 1);

    REQUIRE(settings->get_in_dirpatterns()->empty());
    settings->add_in_dirpattern(new string("dir"));
    REQUIRE(settings->get_in_dirpatterns()->size() == 1);

    REQUIRE(settings->get_out_dirpatterns()->empty());
    settings->add_out_dirpattern(new string("tmp"));
    REQUIRE(settings->get_out_dirpatterns()->size() == 1);

    REQUIRE(settings->get_in_filepatterns()->empty());
    settings->add_in_filepattern(new string("file"));
    REQUIRE(settings->get_in_filepatterns()->size() == 1);

    REQUIRE(settings->get_out_filepatterns()->empty());
    settings->add_out_filepattern(new string("stream"));
    REQUIRE(settings->get_out_filepatterns()->size() == 1);
}

TEST_CASE("Alter booleans in SearchSettings", "[SearchSettings]") {
    auto *settings = new SearchSettings();

    REQUIRE(!settings->get_archivesonly());
    REQUIRE(!settings->get_searcharchives());
    settings->set_archivesonly(true);
    REQUIRE(settings->get_archivesonly());
    REQUIRE(settings->get_searcharchives());

    REQUIRE(!settings->get_debug());
    REQUIRE(!settings->get_verbose());
    settings->set_debug(true);
    REQUIRE(settings->get_debug());
    REQUIRE(settings->get_verbose());

    REQUIRE(settings->get_excludehidden());
    settings->set_excludehidden(false);
    REQUIRE(!settings->get_excludehidden());

    REQUIRE(!settings->get_firstmatch());
    settings->set_firstmatch(true);
    REQUIRE(settings->get_firstmatch());

    REQUIRE(!settings->get_multilinesearch());
    settings->set_multilinesearch(true);
    REQUIRE(settings->get_multilinesearch());

    REQUIRE(!settings->get_listdirs());
    settings->set_listdirs(true);
    REQUIRE(settings->get_listdirs());

    REQUIRE(!settings->get_listfiles());
    settings->set_listfiles(true);
    REQUIRE(settings->get_listfiles());

    REQUIRE(!settings->get_listlines());
    settings->set_listlines(true);
    REQUIRE(settings->get_listlines());

    REQUIRE(!settings->get_printresults());
    settings->set_printresults(true);
    REQUIRE(settings->get_printresults());

    REQUIRE(!settings->get_printusage());
    settings->set_printusage(true);
    REQUIRE(settings->get_printusage());

    REQUIRE(!settings->get_printversion());
    settings->set_printversion(true);
    REQUIRE(settings->get_printversion());

    REQUIRE(settings->get_recursive());
    settings->set_recursive(false);
    REQUIRE(!settings->get_recursive());

    REQUIRE(!settings->get_uniquelines());
    settings->set_uniquelines(true);
    REQUIRE(settings->get_uniquelines());
}


TEST_CASE("Alter ints in SearchSettings", "[SearchSettings]") {
    auto *settings = new SearchSettings();

    REQUIRE(settings->get_linesbefore() == 0);
    settings->set_linesbefore(5);
    REQUIRE(settings->get_linesbefore() == 5);

    // TODO: fix handling of negative values
    //settings->set_linesbefore(-5);
    //REQUIRE(settings->get_linesbefore() == 5);

    REQUIRE(settings->get_linesafter() == 0);
    settings->set_linesafter(5);
    REQUIRE(settings->get_linesafter() == 5);

    // TODO: fix handling of negative values
    //settings->set_linesafter(-5);
    //REQUIRE(settings->get_linesafter() == 5);
}