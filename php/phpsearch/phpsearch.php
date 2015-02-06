#!/usr/bin/env php
<?php

require_once __DIR__ . '/autoload.php';
require_once __DIR__ . '/common.php';

function main($argv) {
    $searchoptions = new SearchOptions();
    try {
        $settings = $searchoptions->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            log_msg("settings: $settings");
        }

        if ($settings->printusage) {
            $searchoptions->usage();
            exit;
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