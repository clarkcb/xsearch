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

sub main {
    my $search_options = plsearch::SearchOptions->new();
    my ($settings, $errs) = $search_options->settings_from_args(\@ARGV);

    if (scalar @$errs) {
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

    if (scalar @$errs2) {
        plfind::common::log_msg('');
        plfind::common::log_err($errs2->[0]);
        plfind::common::log_msg('');
        $search_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my $search_results = $searcher->search();
    my $formatter = plsearch::SearchResultFormatter->new($settings);

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
}

main();
