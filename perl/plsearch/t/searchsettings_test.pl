#!/usr/bin/perl -w
#
# searchsettings_test.pl
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

use Test::Simple tests => 30;

use plsearch::SearchSettings;

sub test_default_settings {
    my $settings = new plsearch::SearchSettings();
    ok(!$settings->{archives_only}, "archives_only is false by default");
    ok($settings->{colorize}, "colorize is true by default");
    ok(!$settings->{debug}, "debug is false by default");
    ok(!$settings->{first_match}, "first_match is false by default");
    ok(!$settings->{include_hidden}, "include_hidden is false by default");
    ok($settings->{lines_after} == 0, "lines_after == 0 by default");
    ok($settings->{lines_before} == 0, "lines_before == 0 by default");
    ok(!$settings->{list_dirs}, "list_dirs is false by default");
    ok(!$settings->{list_files}, "list_files is false by default");
    ok(!$settings->{list_lines}, "list_lines is false by default");
    ok($settings->{max_line_length} == 150, "max_line_length == 150 by default");
    ok(!$settings->{multi_line_search}, "multi_line_search is false by default");
    ok($settings->{print_results}, "print_results is true by default");
    ok(!$settings->{print_usage}, "print_usage is false by default");
    ok(!$settings->{print_version}, "print_version is false by default");
    ok($settings->{recursive}, "recursive is true by default");
    ok(!$settings->{search_archives}, "search_archives is false by default");
    ok(scalar @{$settings->{paths}} == 0, "paths is empty by default");
    ok(!$settings->{unique_lines}, "unique_lines is false by default");
    ok(!$settings->{verbose}, "verbose is false by default");
}

sub test_add_single_extension {
    my $settings = new plsearch::SearchSettings();
    $settings->add_exts('pl', $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 1, "in_extensions has one extension");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
}

sub test_add_comma_delimited_extensions {
    my $settings = new plsearch::SearchSettings();
    $settings->add_exts('pl,py', $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
    ok($settings->{in_extensions}->[1] eq 'py', "in_extensions contains py extension");
}

sub test_add_array_extensions {
    my $settings = new plsearch::SearchSettings();
    $settings->add_exts(['pl','py'], $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
    ok($settings->{in_extensions}->[1] eq 'py', "in_extensions contains py extension");
}

sub test_add_single_pattern {
    my $settings = new plsearch::SearchSettings();
    $settings->add_patterns('Searcher', $settings->{search_patterns});
    ok(scalar @{$settings->{search_patterns}} == 1, "search_patterns has one pattern");
}

sub test_add_array_patterns {
    my $settings = new plsearch::SearchSettings();
    $settings->add_patterns(['Searcher', 'Result'], $settings->{search_patterns});
    ok(scalar @{$settings->{search_patterns}} == 2, "search_patterns has two patterns");
}

sub main {
    test_default_settings();
    test_add_single_extension();
    test_add_comma_delimited_extensions();
    test_add_array_extensions();
    test_add_single_pattern();
    test_add_array_patterns();
}

main();
