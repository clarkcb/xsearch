#!/usr/bin/perl -w
#
# searcher_test.pl
#
#
use strict;
use warnings;

use Test::Simple tests => 73;

use lib '/Users/cary/src/git/xsearch/perl';

use plsearch::SearchSettings;
use plsearch::Searcher;


sub get_settings {
    my $settings = new plsearch::SearchSettings();
    $settings->{startpath} = '.';
    push(@{$settings->{searchpatterns}}, "Searcher");
    return $settings;
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
    push(@{$settings->{in_dirpatterns}}, 'plsearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plsearch';
    ok($searcher->is_search_dir($dir), "$dir matches in_dirpatterns");
}

sub test_is_search_dir_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dirpatterns}}, 'plsearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pysearch';
    ok(!$searcher->is_search_dir($dir), "$dir does not match in_dirpatterns");
}

sub test_is_search_dir_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dirpatterns}}, 'pysearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pysearch';
    ok(!$searcher->is_search_dir($dir), "$dir matches out_dirpatterns");
}

sub test_is_search_dir_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dirpatterns}}, 'pysearch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plsearch';
    ok($searcher->is_search_dir($dir), "$dir does not match out_dirpatterns");
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
    $settings->{excludehidden} = 0;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok($searcher->is_search_dir($dir),
        "Hidden dir $dir is search dir with excludehidden set to false");
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
    push(@{$settings->{in_filepatterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Searcher.pm';
    ok($searcher->is_search_file($file), "$file matches in_filepatterns");
}

sub test_is_search_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_filepatterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$searcher->is_search_file($file), "$file does not match in_filepatterns");
}

sub test_is_search_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_filepatterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Searcher.pm';
    ok(!$searcher->is_search_file($file), "$file matches out_filepatterns");
}

sub test_is_search_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_filepatterns}}, 'Search');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($searcher->is_search_file($file), "$file does not match out_filepatterns");
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
    push(@{$settings->{in_archiveextensions}}, 'zip');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file matches in_archiveextensions");
}

sub test_is_archive_search_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archiveextensions}}, 'gz');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file does not match in_archiveextensions");
}

sub test_is_archive_search_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archiveextensions}}, 'zip');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file matches out_archiveextensions");
}

sub test_is_archive_search_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archiveextensions}}, 'gz');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file does not match out_archiveextensions");
}

sub test_is_archive_search_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archivefilepatterns}}, 'arch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file matches in_archivefilepatterns");
}

sub test_is_archive_search_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archivefilepatterns}}, 'archives');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file does not match in_archivefilepatterns");
}

sub test_is_archive_search_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archivefilepatterns}}, 'arch');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$searcher->is_archive_search_file($file), "$file matches out_archivefilepatterns");
}

sub test_is_archive_search_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archivefilepatterns}}, 'archives');
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($searcher->is_archive_search_file($file), "$file does not match out_archivefilepatterns");
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
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when hidden");
}

sub test_filter_file_hidden_includehidden {
    my $settings = get_settings();
    $settings->{excludehidden} = 0;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok($searcher->filter_file($file), "$file passes filter_file when hidden and excludehidden=0");
}

sub test_filter_file_archive_no_searcharchives {
    my $settings = get_settings();
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when searcharchives=0");
}

sub test_filter_file_archive_searcharchives {
    my $settings = get_settings();
    $settings->{searcharchives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok($searcher->filter_file($file), "$file passes filter_file when searcharchives=1");
}

sub test_filter_file_archive_archivesonly {
    my $settings = get_settings();
    $settings->{archivesonly} = 1;
    $settings->{searcharchives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok($searcher->filter_file($file), "$file passes filter_file when archivesonly=1");
}

sub test_filter_file_nonarchive_archivesonly {
    my $settings = get_settings();
    $settings->{archivesonly} = 1;
    $settings->{searcharchives} = 1;
    my ($searcher, $errs) = new plsearch::Searcher($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
    ok(!$searcher->filter_file($file), "$file does not pass filter_file when archivesonly=1");
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
    test_filter_file_archive_no_searcharchives();         # 2 tests
    test_filter_file_archive_searcharchives();            # 2 tests
    test_filter_file_archive_archivesonly();              # 2 tests
    test_filter_file_nonarchive_archivesonly();           # 2 tests

}

main();
