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
use plsearch::SearchConfig;
use plsearch::Searcher;
use plsearch::SearchOptions;
use plsearch::SearchResultFormatter;

sub handle_err {
    my ($err, $search_options, $colorize) = @_;
    plfind::common::log_msg('');
    plfind::common::log_err($err, $colorize);
    plfind::common::log_msg('');
    $search_options->usage();
    plfind::common::log_msg('');
    exit;
}

sub main {
    my $config = plsearch::SearchConfig->new();
    my $search_options = plsearch::SearchOptions->new($config);
    my ($settings, $errs) = $search_options->settings_from_args(\@ARGV);

    if (scalar @$errs) {
        handle_err($errs->[0], $search_options, 1);
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

    my ($searcher, $errs2) = plsearch::Searcher->new($config, $settings);
    if (scalar @$errs2) {
        handle_err($errs2->[0], $search_options, $settings->{colorize});
    }

    my ($search_results, $errs3) = $searcher->search();
    if (scalar @$errs3) {
        handle_err($errs3->[0], $search_options, $settings->{colorize});
    }

    my $formatter;
    if (scalar @$search_results) {
        $formatter = plsearch::SearchResultFormatter->new($settings);
    }

    if ($settings->{print_results}) {
        plfind::common::log_msg('');
        plsearch::Searcher::print_results($search_results, $formatter);
    }

    # print matching dirs
    if ($settings->{print_dirs}) {
        plsearch::Searcher::print_matching_dirs($search_results, $formatter);
    }

    # print matching files
    if ($settings->{print_files}) {
        plsearch::Searcher::print_matching_files($search_results, $formatter);
    }

    # print matching lines
    if ($settings->{print_lines}) {
        plsearch::Searcher::print_matching_lines($search_results, $formatter);
    }

    # print matches
    if ($settings->{print_matches}) {
        plsearch::Searcher::print_matches($search_results, $formatter);
    }
}

main();
