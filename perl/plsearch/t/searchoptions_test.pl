#!/usr/bin/perl -w
#
# search_options_test.pl
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

use Test::Simple tests => 55;

use plsearch::SearchOptions;

my $search_options = plsearch::SearchOptions->new();

sub test_no_args {
    my $args = [];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from empty args');
    ok(!$settings->{archives_only}, 'archives_only is false by default');
    ok(!$settings->{debug}, 'debug is false by default');
    ok(!$settings->{include_hidden}, 'include_hidden is false by default');
    ok(!$settings->{first_match}, 'first_match is false by default');
    ok($settings->{lines_after} == 0, 'lines_after == 0 by default');
    ok($settings->{lines_before} == 0, 'lines_before == 0 by default');
    ok(!$settings->{list_dirs}, 'list_dirs is false by default');
    ok(!$settings->{list_files}, 'list_files is false by default');
    ok(!$settings->{list_lines}, 'list_lines is false by default');
    ok($settings->{max_line_length} == 150, 'max_line_length == 150 by default');
    ok(!$settings->{multi_line_search}, 'multi_line_search is false by default');
    ok(scalar @{$settings->{paths}} == 0, 'paths is empty by default');
    ok($settings->{print_results}, 'print_results is true by default');
    ok(!$settings->{print_usage}, 'print_usage is false by default');
    ok(!$settings->{print_version}, 'print_version is false by default');
    ok($settings->{recursive}, 'recursive is true by default');
    ok(!$settings->{search_archives}, 'search_archives is false by default');
    ok(!$settings->{unique_lines}, 'unique_lines is false by default');
    ok(!$settings->{verbose}, 'verbose is false by default');
}

sub test_valid_args {
    my $args = ['-x', 'pl,py', '-s', 'Search', '.'];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid args');
    ok(scalar @{$settings->{in_extensions}} == 2, 'in_extensions has two extensions');
    ok($settings->{in_extensions}->[0] eq 'pl', 'in_extensions has "pl" extension');
    ok($settings->{in_extensions}->[1] eq 'py', 'in_extensions has "py" extension');
    ok(scalar @{$settings->{paths}} == 1, 'paths has one path');
    ok($settings->{paths}->[0] eq '.', 'paths has "." path');
    ok(scalar @{$settings->{search_patterns}} == 1, 'search_patterns has one pattern');
    ok($settings->{search_patterns}->[0] eq 'Search', 'search_patterns has "Search" pattern');
}

sub test_archives_only_arg {
    my $args = ['--archivesonly'];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid archives_only arg');
    ok($settings->{archives_only}, 'archives_only is true');
    # ok($settings->{search_archives}, 'search_archives is true');
}

sub test_debug_arg {
    my $args = ['--debug'];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid debug arg');
    ok($settings->{debug}, 'debug is true');
    ok($settings->{verbose}, 'verbose is true');
}

sub test_missing_arg {
    my $args = ['-x'];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from missing value for arg');
    ok($errs->[0] eq 'Missing value for x', 'Correct missing value error message');
}

sub test_invalid_arg {
    my $args = ['-Q'];
    my ($settings, $errs) = $search_options->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from unknown arg');
    ok($errs->[0] eq 'Invalid option: Q', 'Correct unknown option error message');
}

sub test_settings_from_json {
    my $json = <<"END_JSON";
{
  "path": "~/src/xsearch/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "searchpattern": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}
END_JSON
    my ($settings, $errs) = $search_options->settings_from_json($json);
    ok(scalar @$errs == 0, 'no errors getting settings from json');
    ok(scalar @{$settings->{paths}} == 1, "paths has one path");
    my $expected_path = "~/src/xsearch";
    ok($settings->{paths}->[0]->stringify eq $expected_path, 'paths has "~/src/xsearch/" path');
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'js', "in_extensions contains js extension");
    ok($settings->{in_extensions}->[1] eq 'ts', "in_extensions contains ts extension");
    ok(scalar @{$settings->{out_dir_patterns}} == 1, "out_dir_patterns has one pattern");
    ok($settings->{out_dir_patterns}->[0] eq 'node_module', "out_dir_patterns[0] is node_module");
    ok(scalar @{$settings->{out_file_patterns}} == 1, "out_file_patterns has one pattern");
    ok($settings->{out_file_patterns}->[0] eq 'temp', "out_file_patterns[0] is temp");
    ok(scalar @{$settings->{search_patterns}} == 1, "search_patterns has one pattern");
    ok($settings->{search_patterns}->[0] eq 'Searcher', "search_patterns[0] is Searcher");
    ok($settings->{lines_before} == 2, "lines_before is set to 2");
    ok($settings->{lines_after} == 2, "lines_after is set to 2");
    ok($settings->{debug} == 1, "debug is set to true");
    ok($settings->{verbose} == 1, "verbose is set to true");
    ok($settings->{first_match} == 1, "first_match is set to true by setting allmatches to false");
    ok($settings->{include_hidden} == 1, 'include_hidden is set to true');
}

sub main {
    test_no_args();
    test_valid_args();
    test_archives_only_arg();
    test_debug_arg();
    test_missing_arg();
    test_invalid_arg();
    test_settings_from_json();
}

main();
