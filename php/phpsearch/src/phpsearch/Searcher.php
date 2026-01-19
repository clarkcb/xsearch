<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileResult;
use phpfind\FileType;
use phpfind\Finder;
use phpfind\FindException;
use phpfind\Logger;

/**
 * Class Searcher
 */
class Searcher
{
    private readonly SearchSettings $settings;
    private readonly Finder $finder;

    /**
     * @throws SearchException
     */
    public function __construct(SearchSettings $settings)
    {
        $this->settings = $settings;
        try {
            $this->finder = new Finder($settings);
        } catch (FindException $e) {
            throw new SearchException($e->getMessage());
        }
        // We can validate_settings after creating Finder since we want its validation to occur first
        $this->validate_settings();
    }

    /**
     * @throws SearchException
     */
    private function validate_settings(): void
    {
        if (count($this->settings->search_patterns) == 0) {
            throw new SearchException('No search patterns defined');
        }
        if ($this->settings->lines_after < 0) {
            throw new SearchException('Invalid lines_after');
        }
        if ($this->settings->lines_before < 0) {
            throw new SearchException('Invalid lines_before');
        }
        if ($this->settings->max_line_length < 0) {
            throw new SearchException('Invalid max_line_length');
        }
    }

    private function matches_any_pattern(string $s, array $patterns): bool
    {
        foreach ($patterns as $pattern) {
            $pattern = '/' . $pattern . '/';
            if (preg_match_all($pattern, $s)) {
                return true;
            }
        }
        return false;
    }

    private function any_matches_any_pattern(array $slist, array $patterns): bool
    {
        foreach ($slist as $s) {
            if ($this->matches_any_pattern($s, $patterns)) {
                return true;
            }
        }
        return false;
    }

    private function get_new_line_indices(string $s): array
    {
        $indices = array();
        $i = 0;
        while ($i < strlen($s)) {
            if ($s[$i] == "\n") {
                $indices[] = $i;
            }
            $i++;
        }
        return $indices;
    }

    private function get_before_indices(array $indices, int $after_index): array
    {
        $less_or_equal = function ($i) use ($after_index) {
            return $i <= $after_index;
        };
        return array_filter($indices, $less_or_equal);
    }

    private function get_after_indices(array $indices, int $before_index): array
    {
        $greater_than = function ($i) use ($before_index) {
            return $i > $before_index;
        };
        return array_filter($indices, $greater_than);
    }

    private function get_lines_before(
        string $s,
        array $before_start_indices,
        array $start_line_indices,
        array $end_line_indices
    ): array {
        $lines_before = array();
        $i = 0;
        while ($i < count($before_start_indices) && $i < $this->settings->lines_before) {
            $start_index = $before_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_before[] = $line;
            $i++;
        }
        return $lines_before;
    }

    private function get_lines_after(
        string $s,
        array $after_start_indices,
        array $start_line_indices,
        array $end_line_indices
    ): array {
        $lines_after = array();
        $i = 0;
        while ($i < count($after_start_indices) && $i < $this->settings->lines_after) {
            $start_index = $after_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_after[] = $line;
            $i++;
        }
        return $lines_after;
    }

    private function lines_match(array $lines, array $in_patterns, array $out_patterns): bool
    {
        if ((!$in_patterns || $this->any_matches_any_pattern($lines, $in_patterns))
            &&
            (!$out_patterns || !$this->any_matches_any_pattern($lines, $out_patterns))) {
            return true;
        }
        return false;
    }

    private function lines_before_match(array $lines_before): bool
    {
        return $this->lines_match(
            $lines_before,
            $this->settings->in_lines_before_patterns,
            $this->settings->out_lines_before_patterns
        );
    }

    private function lines_after_match(array $lines_after): bool
    {
        return $this->lines_match(
            $lines_after,
            $this->settings->in_lines_after_patterns,
            $this->settings->out_lines_after_patterns
        );
    }

    /**
     * @return SearchResult[]
     */
    public function search_multi_line_string(string $s): array
    {
        $results = array();
        $new_line_indices = $this->get_new_line_indices($s);
        $plus_one = function ($i) {
            return $i + 1;
        };
        $start_line_indices = array_merge(array(0), array_map($plus_one, $new_line_indices));
        $end_line_indices = array_merge($new_line_indices, array(strlen($s) - 1));
        foreach ($this->settings->search_patterns as $pattern) {
            $p = '/' . $pattern . '/';
            if (preg_match_all($p, $s, $matches, PREG_OFFSET_CAPTURE)) {
                foreach ($matches[0] as $m) {
                    $start_index = $m[1];
                    $end_index = $start_index + strlen($m[0]);
                    $before_start_indices = $this->get_before_indices($start_line_indices, $start_index);
                    $m_line_start_index = 0;
                    $before_line_count = 0;
                    if ($before_start_indices) {
                        $before_line_count = count($before_start_indices) - 1;
                        $m_line_start_index = $before_start_indices[$before_line_count];
                    }
                    $m_line_end_index = $end_line_indices[array_search($m_line_start_index, $start_line_indices)];
                    $line = substr($s, $m_line_start_index, $m_line_end_index - $m_line_start_index);
                    $lines_before = [];
                    if ($this->settings->lines_before) {
                        $lines_before = $this->get_lines_before(
                            $s,
                            array_slice(
                                $before_start_indices,
                                ($this->settings->lines_before + 1) * -1,
                                -1
                            ),
                            $start_line_indices,
                            $end_line_indices
                        );
                    }
                    $lines_after = [];
                    if ($this->settings->lines_after) {
                        $after_start_indices = $this->get_after_indices($start_line_indices, $start_index);
                        $lines_after = $this->get_lines_after(
                            $s,
                            array_slice(
                                $after_start_indices,
                                0,
                                $this->settings->lines_after
                            ),
                            $start_line_indices,
                            $end_line_indices
                        );
                    }
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after))) {
                        continue;
                    }
                    $r = new SearchResult(
                        $pattern,
                        null,
                        $before_line_count + 1,
                        $start_index - $m_line_start_index + 1,
                        $end_index - $m_line_start_index + 1,
                        $line,
                        $lines_before,
                        $lines_after
                    );
                    $results[] = $r;
                    if ($this->settings->first_match) {
                        break;
                    }
                }
            }
        }
        return $results;
    }

    /**
     * @return SearchResult[]
     */
    private function search_text_file_contents(FileResult $f): array
    {
        $contents = file_get_contents($f->file_path());
        $results = $this->search_multi_line_string($contents);
        foreach ($results as $r) {
            $r->file = $f;
        }
        return $results;
    }

    /**
     * @return SearchResult[]
     */
    public function search_lines(array $lines): array
    {
        $line_num = 0;
        $line = '';
        $lines_before = array();
        $lines_after = array();
        $pattern_match_map = array();
        $results = array();
        while (true) {
            if ($lines_after) {
                $line = array_shift($lines_after);
            } else {
                $line = array_shift($lines);
            }
            if ($line === null) {
                break;
            }
            $line_num++;
            if ($this->settings->lines_after) {
                while (count($lines_after) < $this->settings->lines_after) {
                    $line_after = array_shift($lines);
                    if ($line_after == null) {
                        break;
                    } else {
                        $lines_after[] = $line_after;
                    }
                }
            }
            foreach ($this->settings->search_patterns as $pattern) {
                if ($this->settings->first_match && array_key_exists($pattern, $pattern_match_map)) {
                    continue;
                }
                $p = '/' . $pattern . '/';
                if (preg_match_all($p, $line, $matches, PREG_OFFSET_CAPTURE)) {
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after))) {
                        continue;
                    }
                    foreach ($matches[0] as $m) {
                        $start_index = $m[1] + 1;
                        $end_index = $start_index + strlen($m[0]);
                        $r = new SearchResult(
                            $pattern,
                            null,
                            $line_num,
                            $start_index,
                            $end_index,
                            $line,
                            $lines_before,
                            $lines_after
                        );
                        $results[] = $r;
                        $pattern_match_map[$pattern] = 1;
                    }
                }
            }
            if ($this->settings->lines_before) {
                if (count($lines_before) == $this->settings->lines_before) {
                    array_shift($lines_before);
                }
                if (count($lines_before) < $this->settings->lines_before) {
                    $lines_before[] = $line;
                }
            }
        }
        return $results;
    }

    /**
     * @return SearchResult[]
     */
    private function search_text_file_lines(FileResult $f): array
    {
        $lines = file($f->file_path());
        $results = $this->search_lines($lines);
        foreach ($results as $r) {
            $r->file = $f;
        }
        return $results;
    }

    /**
     * @return SearchResult[]
     */
    private function search_text_file(FileResult $f): array
    {
        if ($this->settings->debug) {
            Logger::log_msg("Searching text file $f");
        }
        if ($this->settings->multi_line_search) {
            return $this->search_text_file_contents($f);
        } else {
            return $this->search_text_file_lines($f);
        }
    }

    /**
     * @return SearchResult[]
     */
    private function search_binary_file(FileResult $f): array
    {
        if ($this->settings->debug) {
            Logger::log_msg("Searching binary file $f");
        }
        $contents = file_get_contents($f->file_path());
        $results = [];
        foreach ($this->settings->search_patterns as $pattern) {
            $p = '/' . $pattern . '/';
            if (preg_match_all($p, $contents, $matches, PREG_OFFSET_CAPTURE)) {
                foreach ($matches[0] as $m) {
                    $start_index = $m[1] + 1;
                    $end_index = $start_index + strlen($m[0]);
                    $r = new SearchResult(
                        $pattern,
                        $f,
                        0,
                        $start_index,
                        $end_index,
                        null,
                        [],
                        []
                    );
                    $results[] = $r;
                    if ($this->settings->first_match) {
                        break;
                    }
                }
            }
        }
        return $results;
    }

    /**
     * @return SearchResult[]
     */
    public function search_file(FileResult $f): array
    {
        if ($f->file_type == FileType::Text || $f->file_type == FileType::Code ||
            $f->file_type == FileType::Xml) {
            return $this->search_text_file($f);
        } elseif ($f->file_type == FileType::Binary) {
            return $this->search_binary_file($f);
        }
        return [];
    }

    /**
     * @return SearchResult[]
     * @throws SearchException
     */
    public function search(): array
    {
        try {
            $file_results = $this->finder->find();
        } catch (FindException $e) {
            throw new SearchException($e->getMessage());
        }
        if ($this->settings->verbose) {
            $get_path = function ($sf) {
                return $sf->path;
            };
            $dir_results = array_unique(array_map($get_path, $file_results));
            Logger::log_msg(sprintf("\nDirectories to be searched (%d):", count($dir_results)));
            foreach ($dir_results as $d) {
                Logger::log_msg($d);
            }
            Logger::log_msg(sprintf("\n\nFiles to be searched (%d):", count($file_results)));
            foreach ($file_results as $f) {
                Logger::log_msg((string)$f);
            }
        }

        // search the files
        $search_results = [];
        foreach ($file_results as $f) {
            $search_results = array_merge($search_results, $this->search_file($f));
        }
        if (count($search_results) > 1) {
            $search_result_sorter = new SearchResultSorter($this->settings);
            return $search_result_sorter->sort($search_results);
        }
        return $search_results;
    }

    public function print_results(array $results, SearchResultFormatter $formatter): void
    {
        if (count($results) > 0) {
            Logger::log_msg(sprintf("\nSearch results (%d):", count($results)));
            foreach ($results as $r) {
                Logger::log_msg($formatter->format($r));
            }
        } else {
            Logger::log_msg("\nSearch results: 0");
        }
    }

    public function get_file_results(array $results): array
    {
        $file_hash = array();
        $file_results = array();
        foreach ($results as $r) {
            if (null != $r->file) {
                $f = $r->file;
                if (!array_key_exists($f->file_path(), $file_hash)) {
                    $file_results[] = $f;
                    $file_hash[$f->file_path()] = $f;
                }
            }
        }
        return $file_results;
    }

    public function print_matching_dirs(array $results, SearchResultFormatter $formatter): void
    {
        $file_results = $this->get_file_results($results);
        $this->finder->print_matching_dirs($file_results, $formatter->file_result_formatter);
    }

    public function print_matching_files(array $results, SearchResultFormatter $formatter): void
    {
        $file_results = $this->get_file_results($results);
        $this->finder->print_matching_files($file_results, $formatter->file_result_formatter);
    }

    public function get_matching_lines(array $results): array
    {
        $lines = array();
        foreach ($results as $r) {
            $l = trim($r->line);
            if (!$this->settings->unique_lines || !in_array($l, $lines)) {
                $lines[] = $l;
            }
        }
        if ($this->settings->sort_case_insensitive) {
            usort($lines, 'cmp_ignorecase');
        } else {
            sort($lines);
        }
        return $lines;
    }

    public function print_matching_lines(array $results, SearchResultFormatter $formatter): void
    {
        $lines = $this->get_matching_lines($results);
        $msg = "\nMatching lines";
        if ($this->settings->unique_lines) {
            $msg = "\nUnique matching lines";
        }
        if (count($lines) > 0) {
            Logger::log_msg(sprintf("%s (%d):", $msg, count($lines)));
            foreach ($lines as $l) {
                Logger::log_msg($formatter->format_line($l));
            }
        } else {
            Logger::log_msg(sprintf("%s: 0", $msg));
        }
    }

    public function get_matches(array $results): array
    {
        $matches = array();
        foreach ($results as $r) {
            $m = substr($r->line, $r->match_start_index - 1, $r->match_end_index - $r->match_start_index);
            if (!$this->settings->unique_lines || !in_array($m, $matches)) {
                $matches[] = $m;
            }
        }
        if ($this->settings->sort_case_insensitive) {
            usort($matches, 'cmp_ignorecase');
        } else {
            sort($matches);
        }
        return $matches;
    }

    public function print_matches(array $results, SearchResultFormatter $formatter): void
    {
        $matches = $this->get_matches($results);
        $msg = "\nMatches";
        if ($this->settings->unique_lines) {
            $msg = "\nUnique matches";
        }
        if (count($matches) > 0) {
            Logger::log_msg(sprintf("%s (%d):", $msg, count($matches)));
            foreach ($matches as $m) {
                Logger::log_msg($formatter->format_match($m));
            }
        } else {
            Logger::log_msg(sprintf("%s: 0", $msg));
        }
    }
}
