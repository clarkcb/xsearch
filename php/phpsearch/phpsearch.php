#!/usr/bin/env php
<?php

namespace PhpSearch;

require_once 'common.php';
require_once 'searcher.php';
require_once 'searchexception.php';
require_once 'searchoptions.php';

function main($argv) {
    $searchoptions = new SearchOptions();
    try {
        $settings = $searchoptions->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            echo "settings: $settings\n";
        }

        $searcher = new Searcher($settings);
        $searcher->search();

        // print the results
        if ($settings->printresults) {
            $searcher->printresults();
        }

        // print matching dirs
        if ($settings->listdirs) {
            $searcher->print_matching_dirs();
        }

        // print matching files
        if ($settings->listfiles) {
            $searcher->print_matching_files();
        }

        // print matching lines
        if ($settings->listlines) {
            $searcher->print_matching_lines();
        }

    } catch (SearchException $e) {
        log_msg("\nError: " . $e->getMessage() . "\n");
        $searchoptions->usage();
    }
}

main($argv);

?>