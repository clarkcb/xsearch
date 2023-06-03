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

use Test::Simple tests => 89;

use plsearch::config;
use plsearch::FileUtil;
use plsearch::SearchSettings;
use plsearch::Searcher;


sub get_settings {
    my $settings = new plsearch::SearchSettings();
    push(@{$settings->{paths}}, ".");
    push(@{$settings->{search_patterns}}, "Searcher");
    return $settings;
}

sub get_test_file {
  return "$SHAREDPATH/testFiles/testFile2.txt";
}

sub test_validate_settings {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
}

################################################################################
# is_search_dir tests
################################################################################
sub test_is_search_dir_no_patterns {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plsearch';
    ok($searcher->is_search_dir($dir), "$dir is search dir with no patterns");
}

sub test_is_search_dir_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plsearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plsearch';
    ok($searcher->is_search_dir($dir), "$dir matches in_dir_patterns");
}

sub test_is_search_dir_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plsearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pysearch';
    ok(!$searcher->is_search_dir($dir), "$dir does not match in_dir_patterns");
}

sub test_is_search_dir_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pysearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pysearch';
    ok(!$searcher->is_search_dir($dir), "$dir matches out_dir_patterns");
}

sub test_is_search_dir_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pysearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plsearch';
    ok($searcher->is_search_dir($dir), "$dir does not match out_dir_patterns");
}

sub test_is_search_dir_single_dot {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.';
    ok($searcher->is_search_dir($dir), "$dir is search dir");
}

sub test_is_search_dir_double_dot {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '..';
    ok($searcher->is_search_dir($dir), "$dir is search dir");
}

sub test_is_search_dir_hidden_dir {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok(!$searcher->is_search_dir($dir), "Hidden dir $dir is not search dir by default");
}

sub test_is_search_dir_hidden_dir_include_hidden {
    my $settings = get_settings();
    $settings->{exclude_hidden} = 0;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok($searcher->is_search_dir($dir),
        "Hidden dir $dir is search dir with exclude_hidden set to false");
}

################################################################################
# is_search_file tests
################################################################################
sub test_is_search_file_matches_by_default {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->is_search_file($file), "$file is search file by default");
}

sub test_is_search_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->is_search_file($file), "$file matches in_extensions");
}

sub test_is_search_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$searcher->is_search_file($file), "$file does not match in_extensions");
}

sub test_is_search_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'pm');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$searcher->is_search_file($file), "$file matches out_extensions");
}

sub test_is_search_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'py');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->is_search_file($file), "$file does not match out_extensions");
}

sub test_is_search_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Searcher.pm';
    ok($searcher->is_search_file($file), "$file matches in_file_patterns");
}

sub test_is_search_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$searcher->is_search_file($file), "$file does not match in_file_patterns");
}

sub test_is_search_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Searcher.pm';
    ok(!$searcher->is_search_file($file), "$file matches out_file_patterns");
}

sub test_is_search_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->is_search_file($file), "$file does not match out_file_patterns");
}

################################################################################
# is__archive_search_file tests
################################################################################
sub test_is_archive_search_file_matches_by_default {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file is archive search file by default");
}

sub test_is_archive_search_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'zip');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file matches in_archive_extensions");
}

sub test_is_archive_search_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'gz');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file does not match in_archive_extensions");
}

sub test_is_archive_search_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'zip');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file matches out_archive_extensions");
}

sub test_is_archive_search_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'gz');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file does not match out_archive_extensions");
}

sub test_is_archive_search_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'arch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file matches in_archive_file_patterns");
}

sub test_is_archive_search_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'archives');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file does not match in_archive_file_patterns");
}

sub test_is_archive_search_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_file_patterns}}, 'arch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file matches out_archive_file_patterns");
}

sub test_is_archive_search_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_file_patterns}}, 'archives');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file does not match out_archive_file_patterns");
}

################################################################################
# filter_file tests
################################################################################
sub test_filter_file_matches_by_default {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->filter_file($file), "$file passes filter_file by default");
}

sub test_filter_file_is_search_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->filter_file($file), "$file passes filter_file when is_search_file");
}

sub test_filter_file_not_is_search_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when !is_search_file");
}

sub test_filter_file_is_hidden_file {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when exclude_hidden=1");
}

sub test_filter_file_hidden_includehidden {
    my $settings = get_settings();
    $settings->{exclude_hidden} = 0;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok($searcher->filter_file($file), "$file passes filter_file when hidden and exclude_hidden=0");
}

sub test_filter_file_archive_no_search_archives {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when search_archives=0");
}

sub test_filter_file_archive_search_archives {
    my $settings = get_settings();
    $settings->{search_archives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok($searcher->filter_file($file), "$file passes filter_file when search_archives=1");
}

sub test_filter_file_archive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{search_archives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok($searcher->filter_file($file), "$file passes filter_file when archives_only=1");
}

sub test_filter_file_nonarchive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{search_archives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when archives_only=1");
}

################################################################################
# search_lines tests
################################################################################
sub test_search_lines {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $test_file = get_test_file();
    my $contents = plsearch::FileUtil::get_file_contents($test_file);
    my $results = $searcher->search_multiline_string($contents);
    ok(scalar @{$results} == 2, 'Two search results');
    my $first_result = $results->[0];
    ok($first_result->{line_num} == 29, "First result on line 23");
    ok($first_result->{match_start_index} == 3, "First result match_start_index == 3");
    ok($first_result->{match_end_index} == 11, "First result match_start_index == 11");

    my $second_result = $results->[1];
    ok($second_result->{line_num} == 35, "Second result on line 29");
    ok($second_result->{match_start_index} == 24, "Second result match_start_index == 24");
    ok($second_result->{match_end_index} == 32, "Second result match_start_index == 32");
}

################################################################################
# search_multiline_string tests
################################################################################
sub test_search_multiline_string {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $test_file = get_test_file();
    my $lines = plsearch::FileUtil::get_file_lines($test_file);
    my $results = $searcher->search_lines($lines);
    ok(scalar @{$results} == 2, 'Two search results');
    my $first_result = $results->[0];
    ok($first_result->{line_num} == 29, "First result on line 23");
    ok($first_result->{match_start_index} == 3, "First result match_start_index == 3");
    ok($first_result->{match_end_index} == 11, "First result match_start_index == 11");

    my $second_result = $results->[1];
    ok($second_result->{line_num} == 35, "Second result on line 29");
    ok($second_result->{match_start_index} == 24, "Second result match_start_index == 24");
    ok($second_result->{match_end_index} == 32, "Second result match_start_index == 32");
}

################################################################################
# main
################################################################################
sub main {

    test_validate_settings();                             # 1 test

    # is_search_dir tests
    test_is_search_dir_no_patterns();                     # 2 tests
    test_is_search_dir_matches_in_pattern();              # 2 tests
    test_is_search_dir_no_match_in_pattern();             # 2 tests
    test_is_search_dir_matches_out_pattern();             # 2 tests
    test_is_search_dir_no_match_out_pattern();            # 2 tests
    test_is_search_dir_single_dot();                      # 2 tests
    test_is_search_dir_double_dot();                      # 2 tests
    test_is_search_dir_hidden_dir();                      # 2 tests
    test_is_search_dir_hidden_dir_include_hidden();       # 2 tests

    # is_search_file tests
    test_is_search_file_matches_by_default();             # 2 tests
    test_is_search_file_matches_in_extension();           # 2 tests
    test_is_search_file_no_match_in_extension();          # 2 tests
    test_is_search_file_matches_out_extension();          # 2 tests
    test_is_search_file_no_match_out_extension();         # 2 tests
    test_is_search_file_matches_in_pattern();             # 2 tests
    test_is_search_file_no_match_in_pattern();            # 2 tests
    test_is_search_file_matches_out_pattern();            # 2 tests
    test_is_search_file_no_match_out_pattern();           # 2 tests

    # is_archive_search_file tests
    test_is_archive_search_file_matches_by_default();     # 2 tests
    test_is_archive_search_file_matches_in_extension();   # 2 tests
    test_is_archive_search_file_no_match_in_extension();  # 2 tests
    test_is_archive_search_file_matches_out_extension();  # 2 tests
    test_is_archive_search_file_no_match_out_extension(); # 2 tests
    test_is_archive_search_file_matches_in_pattern();     # 2 tests
    test_is_archive_search_file_no_match_in_pattern();    # 2 tests
    test_is_archive_search_file_matches_out_pattern();    # 2 tests
    test_is_archive_search_file_no_match_out_pattern();   # 2 tests

    # filter_file tests
    test_filter_file_matches_by_default();                # 2 tests
    test_filter_file_is_search_file();                    # 2 tests
    test_filter_file_not_is_search_file();                # 2 tests
    test_filter_file_is_hidden_file();                    # 2 tests
    test_filter_file_hidden_includehidden();              # 2 tests
    test_filter_file_archive_no_search_archives();         # 2 tests
    test_filter_file_archive_search_archives();            # 2 tests
    test_filter_file_archive_archives_only();              # 2 tests
    test_filter_file_nonarchive_archives_only();           # 2 tests

    # search_lines tests
    test_search_lines();                                  # 8 tests

    # search_multiline_string tests
    test_search_multiline_string();                       # 8 tests
}

main();
