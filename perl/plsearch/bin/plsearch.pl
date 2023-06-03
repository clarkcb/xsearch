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

use plsearch::common;
use plsearch::config;
use plsearch::Searcher;
use plsearch::SearchOptions;

sub log_error {
    my $err = shift;
    plsearch::common::log('ERROR: '.$err);
}

sub main {
    my $search_options = new plsearch::SearchOptions();
    my ($settings, $errs) = $search_options->settings_from_args(\@ARGV);

    if (scalar @{$errs}) {
        plsearch::common::log('');
        log_error($errs->[0]);
        plsearch::common::log('');
        $search_options->usage();
        plsearch::common::log('');
        exit;
    }

    if ($settings->{debug}) {
        print 'settings: ' . $settings->to_string() . "\n";
    }

    if ($settings->{print_usage}) {
        plsearch::common::log('');
        $search_options->usage();
        plsearch::common::log('');
        exit;
    }

    my ($searcher, $errs2) = new plsearch::Searcher($settings);

    if (scalar @{$errs2}) {
        plsearch::common::log('');
        log_error($errs2->[0]);
        plsearch::common::log('');
        $search_options->usage();
        plsearch::common::log('');
        exit;
    }

    $searcher->search();

    if ($settings->{print_results}) {
        plsearch::common::log('');
        $searcher->print_results();
    }

    # print matching dirs
    if ($settings->{list_dirs}) {
        $searcher->print_matching_dirs();
    }

    # print matching files
    if ($settings->{list_files}) {
        $searcher->print_matching_files();
    }

    # print matching lines
    if ($settings->{list_lines}) {
        $searcher->print_matching_lines();
    }
}

main();
