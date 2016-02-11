#!/usr/bin/perl -w
#
# searchoptions_test.pl
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

use Test::Simple tests => 53;

use plsearch::SearchOptions;

my $searchoptions = new plsearch::SearchOptions();

sub test_no_args {
    my $args = [];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from empty args');
    ok(!$settings->{archivesonly}, 'archivesonly is false by default');
    ok(!$settings->{debug}, 'debug is false by default');
    ok($settings->{excludehidden}, 'excludehidden is true by default');
    ok(!$settings->{firstmatch}, 'firstmatch is false by default');
    ok($settings->{linesafter} == 0, 'linesafter == 0 by default');
    ok($settings->{linesbefore} == 0, 'linesbefore == 0 by default');
    ok(!$settings->{listdirs}, 'listdirs is false by default');
    ok(!$settings->{listfiles}, 'listfiles is false by default');
    ok(!$settings->{listlines}, 'listlines is false by default');
    ok($settings->{maxlinelength} == 150, 'maxlinelength == 150 by default');
    ok(!$settings->{multilinesearch}, 'multilinesearch is false by default');
    ok($settings->{printresults}, 'printresults is true by default');
    ok(!$settings->{printusage}, 'printusage is false by default');
    ok(!$settings->{printversion}, 'printversion is false by default');
    ok($settings->{recursive}, 'recursive is true by default');
    ok(!$settings->{searcharchives}, 'searcharchives is false by default');
    ok($settings->{startpath} eq '', 'startpath is empty by default');
    ok(!$settings->{uniquelines}, 'uniquelines is false by default');
    ok(!$settings->{verbose}, 'verbose is false by default');
}

sub test_valid_args {
    my $args = ['-x', 'pl,py', '-s', 'Search', '.'];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid args');
    ok(scalar @{$settings->{in_extensions}} == 2, 'in_extensions has two extensions');
    ok($settings->{in_extensions}->[0] eq 'pl', 'in_extensions has "pl" extension');
    ok($settings->{in_extensions}->[1] eq 'py', 'in_extensions has "py" extension');
    ok(scalar @{$settings->{searchpatterns}} == 1, 'searchpatterns has one pattern');
    ok($settings->{searchpatterns}->[0] eq 'Search', 'searchpatterns has "Search" pattern');
    ok($settings->{startpath} eq '.', 'startpath eq '.'');
}

sub test_archivesonly_arg {
    my $args = ['--archivesonly'];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid archivesonly arg');
    ok($settings->{archivesonly}, 'archivesonly is true');
    ok($settings->{searcharchives}, 'searcharchives is true');
}

sub test_debug_arg {
    my $args = ['--debug'];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid debug arg');
    ok($settings->{debug}, 'debug is true');
    ok($settings->{verbose}, 'verbose is true');
}

sub test_missing_arg {
    my $args = ['-x'];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from missing value for arg');
    ok($errs->[0] eq 'Missing value for x', 'Correct missing value error message');
}

sub test_unknown_arg {
    my $args = ['-Q'];
    my ($settings, $errs) = $searchoptions->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from unknown arg');
    ok($errs->[0] eq 'Invalid option: Q', 'Correct unknown option error message');
}

sub test_settings_from_json {
    my $settings = new plsearch::SearchSettings();
    my $json = <<"END_JSON";
{
  "startpath": "~/src/xsearch/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "search": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}
END_JSON
    $searchoptions->settings_from_json($json, $settings);
    ok($settings->{startpath} eq '~/src/xsearch/', "startpath is set to ~/src/xsearch/");
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'js', "in_extensions contains js extension");
    ok($settings->{in_extensions}->[1] eq 'ts', "in_extensions contains ts extension");
    ok(scalar @{$settings->{out_dirpatterns}} == 1, "out_dirpatterns has one pattern");
    ok($settings->{out_dirpatterns}->[0] eq 'node_module', "out_dirpatterns[0] is node_module");
    ok(scalar @{$settings->{out_filepatterns}} == 1, "out_filepatterns has one pattern");
    ok($settings->{out_filepatterns}->[0] eq 'temp', "out_filepatterns[0] is temp");
    ok(scalar @{$settings->{searchpatterns}} == 1, "searchpatterns has one pattern");
    ok($settings->{searchpatterns}->[0] eq 'Searcher', "searchpatterns[0] is Searcher");
    ok($settings->{linesbefore} == 2, "linesbefore is set to 2");
    ok($settings->{linesafter} == 2, "linesafter is set to 2");
    ok($settings->{debug} == 1, "debug is set to true");
    ok($settings->{verbose} == 1, "verbose is set to true");
    ok($settings->{firstmatch} == 1, "firstmatch is set to true by setting allmatches to false");
    ok($settings->{excludehidden} == 0, 'excludehidden is set to false by setting includehidden to true');
}

sub main {
    test_no_args();
    test_valid_args();
    test_archivesonly_arg();
    test_debug_arg();
    test_missing_arg();
    test_unknown_arg();
    test_settings_from_json();
}

main();
