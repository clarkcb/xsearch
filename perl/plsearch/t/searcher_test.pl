#!/usr/bin/perl -w
#
# searcher_test.pl
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

use Test::Simple tests => 17;

use plsearch::config;
use plfind::FileUtil;
use plsearch::SearchSettings;
use plsearch::Searcher;


sub get_settings {
    my $settings = plsearch::SearchSettings->new();
    push(@{$settings->{paths}}, ".");
    push(@{$settings->{search_patterns}}, "Searcher");
    return $settings;
}

sub get_test_file {
  return "$SHARED_PATH/testFiles/testFile2.txt";
}

sub test_validate_settings {
    my $settings = get_settings();
    my ($searcher, $errs) = plsearch::Searcher->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
}

################################################################################
# search_lines tests
################################################################################
sub test_search_lines {
    my $settings = get_settings();
    my ($searcher, $errs) = plsearch::Searcher->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $test_file = get_test_file();
    my $contents = plfind::FileUtil::get_file_contents($test_file);
    my $results = $searcher->search_multiline_string($contents);
    ok(scalar @{$results} == 2, 'Two search results');
    my $first_result = $results->[0];
    ok($first_result->{line_num} == 30, "First result on line 30");
    ok($first_result->{match_start_index} == 3, "First result match_start_index == 3");
    ok($first_result->{match_end_index} == 11, "First result match_start_index == 11");

    my $second_result = $results->[1];
    ok($second_result->{line_num} == 36, "Second result on line 36");
    ok($second_result->{match_start_index} == 24, "Second result match_start_index == 24");
    ok($second_result->{match_end_index} == 32, "Second result match_start_index == 32");
}

################################################################################
# search_multiline_string tests
################################################################################
sub test_search_multiline_string {
    my $settings = get_settings();
    my ($searcher, $errs) = plsearch::Searcher->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $test_file = get_test_file();
    my $lines = plfind::FileUtil::get_file_lines($test_file);
    my $results = $searcher->search_lines($lines);
    ok(scalar @{$results} == 2, 'Two search results');
    my $first_result = $results->[0];
    ok($first_result->{line_num} == 30, "First result on line 30");
    ok($first_result->{match_start_index} == 3, "First result match_start_index == 3");
    ok($first_result->{match_end_index} == 11, "First result match_start_index == 11");

    my $second_result = $results->[1];
    ok($second_result->{line_num} == 36, "Second result on line 36");
    ok($second_result->{match_start_index} == 24, "Second result match_start_index == 24");
    ok($second_result->{match_end_index} == 32, "Second result match_start_index == 32");
}

################################################################################
# main
################################################################################
sub main {

    test_validate_settings();                             # 1 test

    # search_lines tests
    test_search_lines();                                  # 8 tests

    # search_multiline_string tests
    test_search_multiline_string();                       # 8 tests
}

main();
