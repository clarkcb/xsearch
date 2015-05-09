#!/usr/bin/perl -w
#
# searchresult_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $curpath;

BEGIN {
    $curpath = dirname(dirname(abs_path($0)));
    unshift @INC, $curpath;
}

use Test::Simple tests => 3;

use plsearch::common;
use plsearch::config;
use plsearch::SearchResult;

my $cssearch_path = $XSEARCHPATH . '/csharp/CsSearch/CsSearch';

sub test_singleline_searchresult {
    my $pattern = 'Search';
    my $file = $cssearch_path . '/Searcher.cs';
    my $linenum = 10;
    my $match_start_index = 15;
    my $match_end_index = 23;
    my $line = "\tpublic class Searcher\n";
    my $linesbefore = [];
    my $linesafter = [];
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum,
        $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
    my $expectedoutput = sprintf("%s: %d: [%d:%d]: %s", $file, $linenum,
        $match_start_index, $match_end_index, plsearch::common::trim($line));
    # print "searchresult->to_string():\n".$searchresult->to_string()."\n";
    # print "expectedoutput:\n$expectedoutput\n";
    ok($expectedoutput eq $searchresult->to_string(),
        'single-line searchresult matches expected output');
}

sub test_binaryfile_searchresult {
    my $pattern = 'Search';
    my $file = $cssearch_path . '/Searcher.exe';
    my $linenum = 0;
    my $match_start_index = 0;
    my $match_end_index = 0;
    my $line = "";
    my $linesbefore = [];
    my $linesafter = [];
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum,
        $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
    my $expectedoutput = sprintf("%s matches", $file);
    # print "searchresult->to_string():\n".$searchresult->to_string()."\n";
    # print "expectedoutput:\n$expectedoutput\n";
    ok($expectedoutput eq $searchresult->to_string(),
        'binary-file searchresult matches expected output');
}

sub test_multiline_searchresult {
    my $pattern = 'Search';
    my $file = $cssearch_path . '/Searcher.cs';
    my $linenum = 10;
    my $match_start_index = 15;
    my $match_end_index = 23;
    my $line = "\tpublic class Searcher\n";
    my $linesbefore = ["namespace CsSearch\n", "{\n"];
    my $linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum,
        $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
    my $outputtemplate = 
        "================================================================================\n" .
        "%s: %d: [%d:%d]\n" .
        "--------------------------------------------------------------------------------\n" .
        "   8 | namespace CsSearch\n" .
        "   9 | {\n" .
        "> 10 | \tpublic class Searcher\n" .
        "  11 | \t{\n" .
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    my $expectedoutput = sprintf($outputtemplate, $file, $linenum,
        $match_start_index, $match_end_index);
    # print "searchresult->to_string():\n".$searchresult->to_string()."\n";
    # print "expectedoutput:\n$expectedoutput\n";
    ok($expectedoutput eq $searchresult->to_string(),
        'multi-line searchresult matches expected output');
}

sub main {
    test_singleline_searchresult();
    test_binaryfile_searchresult();
    test_multiline_searchresult();
}

main();
