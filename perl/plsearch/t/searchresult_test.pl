#!/usr/bin/perl -w
#
# searchresult_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    # print "lib_path: $lib_path\n";
    unshift @INC, $lib_path;
}

use Test::Simple tests => 5;

use plsearch::common;
use plsearch::config;
use plsearch::Color;
use plsearch::SearchResult;
use plsearch::SearchResultFormatter;
use plsearch::SearchSettings;

my $cssearch_path = $XSEARCHPATH . '/csharp/CsSearch/CsSearch';

sub test_singleline_searchresult {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'Search';
    my $file = $cssearch_path . '/Searcher.cs';
    my $linenum = 10;
    my $match_start_index = 15;
    my $match_end_index = 21;
    my $line = "\tpublic class Searcher\n";
    my $linesbefore = [];
    my $linesafter = [];
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum, $match_start_index,
        $match_end_index, $line, $linesbefore, $linesafter);
    my $expectedoutput = sprintf("%s: %d: [%d:%d]: %s", $file, $linenum,
        $match_start_index, $match_end_index, plsearch::common::trim($line));
    my $output = $formatter->format($searchresult);
    ok($expectedoutput eq $output,
        'single-line searchresult matches expected output');
}

sub test_singleline_longer_than_maxlength_searchresult {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    $settings->{maxlinelength} = 100;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'maxlen';
    my $file = $cssearch_path . '/maxlen.txt';
    my $linenum = 1;
    my $match_start_index = 53;
    my $match_end_index = 59;
    # This line is 110 chars long
    my $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    my $linesbefore = [];
    my $linesafter = [];
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum, $match_start_index,
        $match_end_index, $line, $linesbefore, $linesafter);
    my $expectedline = '...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...';
    my $expectedoutput = sprintf("%s: %d: [%d:%d]: %s", $file, $linenum,
        $match_start_index, $match_end_index, $expectedline);
    my $output = $formatter->format($searchresult);
    # print "expected: $expectedoutput\n";
    # print "output:   $output\n";
    ok($expectedoutput eq $output,
        'single-line searchresult longer than maxlength matches expected output');
}

sub test_singleline_longer_colorize_searchresult {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 1;
    $settings->{maxlinelength} = 100;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'maxlen';
    my $file = $cssearch_path . '/maxlen.txt';
    my $linenum = 1;
    my $match_start_index = 53;
    my $match_end_index = 59;
    # This line is 110 chars long
    my $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    my $linesbefore = [];
    my $linesafter = [];
    my $maxlinelength = 100;
    my $colorize = 1;
    my $searchresult = new plsearch::SearchResult($pattern, $file, $linenum, $match_start_index,
        $match_end_index, $line, $linesbefore, $linesafter, $maxlinelength, $colorize);
    my $expectedline = '...89012345678901234567890123456789012345678901'.
        plsearch::Color->GREEN .
        'maxlen' .
        plsearch::Color->RESET .
        '89012345678901234567890123456789012345678901...';
    my $expectedoutput = sprintf("%s: %d: [%d:%d]: %s", $file, $linenum,
        $match_start_index, $match_end_index, $expectedline);
    my $output = $formatter->format($searchresult);
    ok($expectedoutput eq $output,
        'single-line searchresult longer than maxlength colorize matches expected output');
}

sub test_binaryfile_searchresult {
    my $settings = new plsearch::SearchSettings();
    my $formatter = new plsearch::SearchResultFormatter($settings);
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
    my $expectedoutput = sprintf("%s matches at [%d:%d]", $file,
        $match_start_index, $match_end_index);
    my $output = $formatter->format($searchresult);
    ok($expectedoutput eq $output,
        'binary-file searchresult matches expected output');
}

sub test_multiline_searchresult {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    my $formatter = new plsearch::SearchResultFormatter($settings);
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
    my $output = $formatter->format($searchresult);
    ok($expectedoutput eq $output,
        'multi-line searchresult matches expected output');
}

sub main {
    test_singleline_searchresult();
    test_singleline_longer_than_maxlength_searchresult();
    test_singleline_longer_colorize_searchresult();
    test_binaryfile_searchresult();
    test_multiline_searchresult();
}

main();
