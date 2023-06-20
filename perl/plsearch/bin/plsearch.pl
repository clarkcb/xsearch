#!/usr/bin/perl -w
#
# plsearch.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    unshift @INC, $lib_path;
}

use lib $ENV{XFIND_PATH} . '/perl/plfind/lib';

use plfind::common;

use plsearch::config;
use plsearch::Searcher;
use plsearch::SearchOptions;

sub log_error {
    my $err = shift;
    plfind::common::log_msg('ERROR: '.$err);
}

sub sort_results ($$) {
    my $a = $_[0];
    my $b = $_[1];
    if ($a->{file} eq $b->{file}) {
        if ($a->{line_num} == $b->{line_num}) {
            $a->{match_start_index} <=> $b->{match_start_index}
        } else {
            $a->{line_num} <=> $b->{line_num}
        }
    } else {
        $a->{file} cmp $b->{file}
    }
}

sub print_results {
    my ($results, $settings) = @_;
    my $len = scalar @$results;
    my $formatter = new plsearch::SearchResultFormatter($settings);

    plfind::common::log_msg("Search results ($len):");
    foreach my $r (@$results) {
        plfind::common::log_msg($formatter->format($r));
    }
}

sub get_matching_dirs {
    my ($results) = @_;
    my $dir_hash = {};
    foreach my $r (@$results) {
        my $d = dirname($r->{file});
        $dir_hash->{$d}++;
    }
    my @dirs = keys %{$dir_hash};
    @dirs = sort(@dirs);
    return \@dirs;
}

sub print_matching_dirs {
    my ($results) = @_;
    my $dirs = get_matching_dirs($results);
    plfind::common::log_msg(sprintf("\nDirectories with matches (%d):", scalar @{$dirs}));
    foreach my $d (@{$dirs}) {
        plfind::common::log_msg($d);
    }
}

sub get_matching_files {
    my ($results) = @_;
    my $file_hash = {};
    foreach my $r (@$results) {
        my $f = $r->{file};
        $file_hash->{$f}++;
    }
    my @files = keys %{$file_hash};
    @files = sort(@files);
    return \@files;
}

sub print_matching_files {
    my ($results) = @_;
    my $files = get_matching_files($results);
    plfind::common::log_msg(sprintf("\nFiles with matches (%d):", scalar @{$files}));
    foreach my $f (@{$files}) {
        plfind::common::log_msg($f);
    }
}

sub get_matching_lines {
    my ($results, $settings) = @_;
    my $line_hash = {};
    my @lines = ();
    foreach my $r (@$results) {
        my $l = plfind::common::trim($r->{line});
        $line_hash->{$l}++;
        push(@lines, $l);
    }
    if ($settings->{unique_lines}) {
        @lines = keys %{$line_hash};
    }
    @lines = sort {uc($a) cmp uc($b)} @lines;
    return \@lines;
}

sub print_matching_lines {
    my ($results, $settings) = @_;
    my $lines = get_matching_lines($results, $settings);
    my $msg = "\nLines with matches (%d):";
    if ($settings->{unique_lines}) {
        $msg = "\nUnique lines with matches (%d):";
    }
    plfind::common::log_msg(sprintf($msg, scalar @{$lines}));
    foreach my $l (@{$lines}) {
        plfind::common::log_msg($l);
    }
}

sub main {
    my $search_options = new plsearch::SearchOptions();
    my ($settings, $errs) = $search_options->settings_from_args(\@ARGV);

    if (scalar @{$errs}) {
        plfind::common::log_msg('');
        log_error($errs->[0]);
        plfind::common::log_msg('');
        $search_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    if ($settings->{debug}) {
        print 'settings: ' . $settings->to_string() . "\n";
    }

    if ($settings->{print_usage}) {
        plfind::common::log_msg('');
        $search_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my ($searcher, $errs2) = new plsearch::Searcher($settings);

    if (scalar @{$errs2}) {
        plfind::common::log_msg('');
        log_error($errs2->[0]);
        plfind::common::log_msg('');
        $search_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my $results = $searcher->search();

    if ($settings->{print_results}) {
        plfind::common::log_msg('');
        print_results($results, $settings);
    }

    # print matching dirs
    if ($settings->{list_dirs}) {
        print_matching_dirs($results);
    }

    # print matching files
    if ($settings->{list_files}) {
        print_matching_files($results);
    }

    # print matching lines
    if ($settings->{list_lines}) {
        print_matching_lines($results, $settings);
    }
}

main();
