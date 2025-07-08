<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileResultSorter;
use phpfind\SortBy;

class SearchResultSorter
{
    private SearchSettings $settings;
    private FileResultSorter $file_result_sorter;

    public function __construct(SearchSettings $settings)
    {
        $this->settings = $settings;
        $this->file_result_sorter = new FileResultSorter($settings);
    }

    private function cmp_by_search_fields(SearchResult $sr1, SearchResult $sr2): int
    {
        if ($sr1->line_num == $sr2->line_num) {
            if ($sr1->match_start_index == $sr2->match_start_index) {
                return ($sr1->match_end_index < $sr2->match_end_index) ? -1 : 1;
            }
            return ($sr1->match_start_index < $sr2->match_start_index) ? -1 : 1;
        }
        return ($sr1->line_num < $sr2->line_num) ? -1 : 1;
    }

    /**
     * @param SearchResult $sr1
     * @param SearchResult $sr2
     * @return int
     */
    private function cmp_file_result_path(SearchResult $sr1, SearchResult $sr2): int
    {
        $file_cmp = $this->file_result_sorter->cmp_file_result_path($sr1->file, $sr2->file);
        if ($file_cmp == 0) {
            return $this->cmp_by_search_fields($sr1, $sr2);
        }
        return $file_cmp;
    }

    /**
     * @param SearchResult $sr1
     * @param SearchResult $sr2
     * @return int
     */
    private function cmp_file_result_file_name(SearchResult $sr1, SearchResult $sr2): int
    {
        $file_cmp = $this->file_result_sorter->cmp_file_result_file_name($sr1->file, $sr2->file);
        if ($file_cmp == 0) {
            return $this->cmp_by_search_fields($sr1, $sr2);
        }
        return $file_cmp;
    }

    /**
     * @param SearchResult $sr1
     * @param SearchResult $sr2
     * @return int
     */
    private function cmp_file_result_file_size(SearchResult $sr1, SearchResult $sr2): int
    {
        $file_cmp = $this->file_result_sorter->cmp_file_result_file_size($sr1->file, $sr2->file);
        if ($file_cmp == 0) {
            return $this->cmp_by_search_fields($sr1, $sr2);
        }
        return $file_cmp;
    }

    /**
     * @param SearchResult $sr1
     * @param SearchResult $sr2
     * @return int
     */
    private function cmp_file_result_file_type(SearchResult $sr1, SearchResult $sr2): int
    {
        $file_cmp = $this->file_result_sorter->cmp_file_result_file_type($sr1->file, $sr2->file);
        if ($file_cmp == 0) {
            return $this->cmp_by_search_fields($sr1, $sr2);
        }
        return $file_cmp;
    }

    /**
     * @param SearchResult $sr1
     * @param SearchResult $sr2
     * @return int
     */
    private function cmp_file_result_last_mod(SearchResult $sr1, SearchResult $sr2): int
    {
        $file_cmp = $this->file_result_sorter->cmp_file_result_last_mod($sr1->file, $sr2->file);
        if ($file_cmp == 0) {
            return $this->cmp_by_search_fields($sr1, $sr2);
        }
        return $file_cmp;
    }

    public function get_cmp_function(): \Closure
    {
        if ($this->settings->sort_descending) {
            return match ($this->settings->sort_by) {
                SortBy::Filename => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_name(
                    $sr2,
                    $sr1
                ),
                SortBy::Filesize => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_size(
                    $sr2,
                    $sr1
                ),
                SortBy::Filetype => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_type(
                    $sr2,
                    $sr1
                ),
                SortBy::LastMod => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_last_mod($sr2, $sr1),
                default => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_path($sr2, $sr1),
            };
        }
        return match ($this->settings->sort_by) {
            SortBy::Filename => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_name($sr1, $sr2),
            SortBy::Filesize => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_size($sr1, $sr2),
            SortBy::Filetype => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_file_type($sr1, $sr2),
            SortBy::LastMod => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_last_mod($sr1, $sr2),
            default => fn(SearchResult $sr1, SearchResult $sr2) => $this->cmp_file_result_path($sr1, $sr2),
        };
    }

    /**
     * @param SearchResult[] $search_results
     * @return SearchResult[]
     */
    public function sort(array $search_results): array
    {
        $cmp_function = $this->get_cmp_function();
        usort($search_results, $cmp_function);
        return $search_results;
    }
}
