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
    $search_options = new SearchOptions();
    try {
        $settings = $search_options->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            Logger::log_msg("settings: $settings");
        }

        if ($settings->print_usage) {
            Logger::log_msg('');
            $search_options->usage();
            exit;
        }

        $searcher = new Searcher($settings);
        $searcher->search();

        // print the results
        if ($settings->print_results) {
            $searcher->print_results();
        }

        // print matching dirs
        if ($settings->list_dirs) {
            $searcher->print_matching_dirs();
        }

        // print matching files
        if ($settings->list_files) {
            $searcher->print_matching_files();
        }

        // print matching lines
        if ($settings->list_lines) {
            $searcher->print_matching_lines();
        }
    } catch (SearchException $e) {
        Logger::log_msg("\nERROR: " . $e->getMessage() . "\n");
        $search_options->usage();
    }
}

main($argv);

?>