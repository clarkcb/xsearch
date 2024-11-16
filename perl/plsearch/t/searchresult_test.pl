#!/usr/bin/perl -w
#
# search_result_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;
use Path::Class;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    # print "lib_path: $lib_path\n";
    unshift @INC, $lib_path;
}

use Test::Simple tests => 5;

use plfind::common;
use plfind::FileResult;
use plfind::FileType;
use plsearch::config;
use plsearch::Color;
use plsearch::SearchResult;
use plsearch::SearchResultFormatter;
use plsearch::SearchSettings;

my $cssearch_path = $XSEARCHPATH . '/csharp/CsSearch/CsSearch';

sub test_single_line_search_result {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'Search';
    my $file_path = file("$cssearch_path/Searcher.cs");
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $line_num = 10;
    my $match_start_index = 15;
    my $match_end_index = 21;
    my $line = "\tpublic class Searcher\n";
    my $lines_before = [];
    my $lines_after = [];
    my $search_result = new plsearch::SearchResult($pattern, $file_result, $line_num, $match_start_index,
        $match_end_index, $line, $lines_before, $lines_after);
    my $expected_output = sprintf("%s: %d: [%d:%d]: %s", $file_path, $line_num,
        $match_start_index, $match_end_index, plfind::common::trim($line));
    my $output = $formatter->format($search_result);
    ok($expected_output eq $output,
        'single-line search_result matches expected output');
}

sub test_single_line_longer_than_maxlength_search_result {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    $settings->{max_line_length} = 100;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'maxlen';
    my $file_path = file("$cssearch_path/maxlen.txt");
    my $file_type = plfind::FileType->TEXT;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $line_num = 1;
    my $match_start_index = 53;
    my $match_end_index = 59;
    # This line is 110 chars long
    my $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    my $lines_before = [];
    my $lines_after = [];
    my $search_result = new plsearch::SearchResult($pattern, $file_result, $line_num, $match_start_index,
        $match_end_index, $line, $lines_before, $lines_after);
    my $expectedline = '...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...';
    my $expected_output = sprintf("%s: %d: [%d:%d]: %s", $file_path, $line_num,
        $match_start_index, $match_end_index, $expectedline);
    my $output = $formatter->format($search_result);
    # print "expected: $expected_output\n";
    # print "output:   $output\n";
    ok($expected_output eq $output,
        'single-line search_result longer than maxlength matches expected output');
}

sub test_single_line_longer_colorize_search_result {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 1;
    $settings->{max_line_length} = 100;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'maxlen';
    my $file_path = file("$cssearch_path/maxlen.txt");
    my $file_type = plfind::FileType->TEXT;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $line_num = 1;
    my $match_start_index = 53;
    my $match_end_index = 59;
    # This line is 110 chars long
    my $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
    my $lines_before = [];
    my $lines_after = [];
    my $max_line_length = 100;
    my $colorize = 1;
    my $search_result = new plsearch::SearchResult($pattern, $file_result, $line_num, $match_start_index,
        $match_end_index, $line, $lines_before, $lines_after, $max_line_length, $colorize);
    my $expectedline = '...89012345678901234567890123456789012345678901'.
        plsearch::Color->GREEN .
        'maxlen' .
        plsearch::Color->RESET .
        '89012345678901234567890123456789012345678901...';
    my $expected_output = sprintf("%s: %d: [%d:%d]: %s", $file_path, $line_num,
        $match_start_index, $match_end_index, $expectedline);
    my $output = $formatter->format($search_result);
    ok($expected_output eq $output,
        'single-line search_result longer than maxlength colorize matches expected output');
}

sub test_binary_file_search_result {
    my $settings = new plsearch::SearchSettings();
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'Search';
    my $file_path = file("$cssearch_path/Searcher.exe");
    my $file_type = plfind::FileType->BINARY;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $line_num = 0;
    my $match_start_index = 0;
    my $match_end_index = 0;
    my $line = "";
    my $lines_before = [];
    my $lines_after = [];
    my $search_result = new plsearch::SearchResult($pattern, $file_result, $line_num,
        $match_start_index, $match_end_index, $line, $lines_before, $lines_after);
    my $expected_output = sprintf("%s matches at [%d:%d]", $file_path,
        $match_start_index, $match_end_index);
    my $output = $formatter->format($search_result);
    ok($expected_output eq $output,
        'binary-file search_result matches expected output');
}

sub test_multi_line_search_result {
    my $settings = new plsearch::SearchSettings();
    $settings->{colorize} = 0;
    my $formatter = new plsearch::SearchResultFormatter($settings);
    my $pattern = 'Search';
    my $file_path = file("$cssearch_path/Searcher.cs");
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $line_num = 10;
    my $match_start_index = 15;
    my $match_end_index = 23;
    my $line = "\tpublic class Searcher\n";
    my $lines_before = ["namespace CsSearch\n", "{\n"];
    my $lines_after = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
    my $search_result = new plsearch::SearchResult($pattern, $file_result, $line_num,
        $match_start_index, $match_end_index, $line, $lines_before, $lines_after);
    my $outputtemplate = 
        "================================================================================\n" .
        "%s: %d: [%d:%d]\n" .
        "--------------------------------------------------------------------------------\n" .
        "   8 | namespace CsSearch\n" .
        "   9 | {\n" .
        "> 10 | \tpublic class Searcher\n" .
        "  11 | \t{\n" .
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    my $expected_output = sprintf($outputtemplate, $file_path, $line_num,
        $match_start_index, $match_end_index);
    my $output = $formatter->format($search_result);
    ok($expected_output eq $output,
        'multi-line search_result matches expected output');
}

sub main {
    test_single_line_search_result();
    test_single_line_longer_than_maxlength_search_result();
    test_single_line_longer_colorize_search_result();
    test_binary_file_search_result();
    test_multi_line_search_result();
}

main();
