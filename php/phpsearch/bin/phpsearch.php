#!/usr/bin/env php
<?php

declare(strict_types=1);

require_once __DIR__ . '/../src/autoload.php';

use phpfind\Logger;
use phpsearch\Searcher;
use phpsearch\SearchException;
use phpsearch\SearchOptions;
use phpsearch\SearchResultFormatter;

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
            $search_options->usage_and_exit(0);
        }

        $searcher = new Searcher($settings);
        $search_results = $searcher->search();
        $formatter = new SearchResultFormatter($settings);

        // print the results
        if ($settings->print_results) {
            $searcher->print_results($search_results, $formatter);
        }

        // print matching dirs
        if ($settings->print_dirs) {
            $searcher->print_matching_dirs($search_results, $formatter);
        }

        // print matching files
        if ($settings->print_files) {
            $searcher->print_matching_files($search_results, $formatter);
        }

        // print matching lines
        if ($settings->print_lines) {
            $searcher->print_matching_lines($search_results, $formatter);
        }
    } catch (SearchException $e) {
        Logger::log_msg('');
        Logger::log_err($e->getMessage() . "\n");
        $search_options->usage_and_exit(1);
    }
}

main($argv);

?>