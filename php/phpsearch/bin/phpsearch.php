#!/usr/bin/env php
<?php

require_once __DIR__ . '/../src/autoload.php';
//require_once __DIR__ . '/../src/phpsearch/common.php';

use \phpsearch\Logger;
use \phpsearch\SearchOptions;
use \phpsearch\Searcher;
use \phpsearch\SearchException;

function main($argv): void
{
    $searchoptions = new SearchOptions();
    try {
        $settings = $searchoptions->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            Logger::log_msg("settings: $settings");
        }

        if ($settings->printusage) {
            Logger::log_msg('');
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
        Logger::log_msg("\nERROR: " . $e->getMessage() . "\n");
        $searchoptions->usage();
    }
}

main($argv);

?>