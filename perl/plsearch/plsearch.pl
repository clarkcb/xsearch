#!/usr/bin/perl -w
#
# plsearch.pl
#
#
use strict;
use warnings;

BEGIN {
  use lib "$ENV{HOME}/src/git/xsearch/perl";
}

use plsearch::common;
use plsearch::Searcher;
use plsearch::SearchOptions;

sub log_error {
    my $err = shift;
    plsearch::common::log('ERROR: '.$err);
}

sub main {
    my $searchoptions = new plsearch::SearchOptions();
    my ($settings, $errs) = $searchoptions->settings_from_args(\@ARGV);

    if (scalar @{$errs}) {
        plsearch::common::log('');
        log_error($errs->[0]);
        plsearch::common::log('');
        $searchoptions->usage();
        exit;
    }

    if ($settings->{debug}) {
        print 'settings: ' . $settings->to_string() . "\n";
    }

    if ($settings->{printusage}) {
        plsearch::common::log('');
        $searchoptions->usage();
        plsearch::common::log('');
        exit;
    }

    my ($searcher, $errs2) = new plsearch::Searcher($settings);

    if (scalar @{$errs2}) {
        plsearch::common::log('');
        log_error($errs2->[0]);
        plsearch::common::log('');
        $searchoptions->usage();
        plsearch::common::log('');
        exit;
    }

    $searcher->search();

    if ($settings->{printresults}) {
        plsearch::common::log('');
        $searcher->print_results();
    }

    # print matching dirs
    if ($settings->{listdirs}) {
        $searcher->print_matching_dirs();
    }

    # print matching files
    if ($settings->{listfiles}) {
        $searcher->print_matching_files();
    }

    # print matching lines
    if ($settings->{listlines}) {
        $searcher->print_matching_lines();
    }
}

main();
