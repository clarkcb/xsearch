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
    my ($search_results) = @_;
    my @dirs = map { $_->{file}->{path} } @$search_results;
    my $uniq = plfind::common::uniq(\@dirs);
    return $uniq;
}

sub print_matching_dirs {
    my ($search_results) = @_;
    my $dirs = get_matching_dirs($search_results);
    if (scalar @{$dirs}) {
        plfind::common::log_msg(sprintf("\nMatching directories (%d):", scalar @{$dirs}));
        foreach my $d (@{$dirs}) {
            plfind::common::log_msg($d);
        }
    } else {
        plfind::common::log_msg("\nMatching directories: 0");
    }
}

sub get_matching_files {
    my ($search_results) = @_;
    my @files = map { $_->{file}->to_string() } @$search_results;
    return \@files;
}

sub print_matching_files {
    my ($search_results) = @_;
    my $files = get_matching_files($search_results);
    if (scalar @{$files}) {
        plfind::common::log_msg(sprintf("\nMatching files (%d):", scalar @{$files}));
        foreach my $f (@{$files}) {
            plfind::common::log_msg($f);
        }
    } else {
        plfind::common::log_msg("\nMatching files: 0");
    }
}

sub get_matching_lines {
    my ($search_results, $settings) = @_;
    my @lines = map { plfind::common::trim($_->{line}) } @$search_results;
    if ($settings->{unique_lines}) {
        my $uniq = plfind::common::uniq(\@lines);
        @lines = @{$uniq};
    }
    @lines = sort {uc($a) cmp uc($b)} @lines;
    return \@lines;
}

sub print_matching_lines {
    my ($search_results, $settings) = @_;
    my $lines = get_matching_lines($search_results, $settings);
    my $title = 'Matching lines';
    if ($settings->{unique_lines}) {
        $title = 'Unique matching lines';
    }
    if (scalar @{$lines}) {
        my $msg = "\n%s (%d):";
        plfind::common::log_msg(sprintf($msg, $title, scalar @{$lines}));
        foreach my $l (@{$lines}) {
            plfind::common::log_msg($l);
        }
    } else {
        plfind::common::log_msg("\n$title: 0");
    }
}

sub main {
    my $search_options = plsearch::SearchOptions->new();
    my ($settings, $errs) = $search_options->settings_from_args(\@ARGV);

    if (scalar @{$errs}) {
        plfind::common::log_msg('');
        plfind::common::log_err($errs->[0]);
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

    my ($searcher, $errs2) = plsearch::Searcher->new($settings);

    if (scalar @{$errs2}) {
        plfind::common::log_msg('');
        plfind::common::log_err($errs2->[0]);
        plfind::common::log_msg('');
        $search_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my $search_results = $searcher->search();

    if ($settings->{print_results}) {
        plfind::common::log_msg('');
        print_results($search_results, $settings);
    }

    # print matching dirs
    if ($settings->{print_dirs}) {
        print_matching_dirs($search_results);
    }

    # print matching files
    if ($settings->{print_files}) {
        print_matching_files($search_results);
    }

    # print matching lines
    if ($settings->{print_lines}) {
        print_matching_lines($search_results, $settings);
    }
}

main();
