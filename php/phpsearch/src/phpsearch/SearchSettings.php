<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileTypes;
use phpfind\FindSettings;
use phpfind\StringUtil;

/**
 * Class SearchSettings
 */
class SearchSettings extends FindSettings
{
    public bool $first_match = false;
    public array $in_lines_after_patterns = array();
    public array $in_lines_before_patterns = array();
    public int $lines_after = 0;
    public array $lines_after_to_patterns = array();
    public array $lines_after_until_patterns = array();
    public int $lines_before = 0;
    public int $max_line_length = 150;
    public bool $multi_line_search = false;
    public array $out_lines_after_patterns = array();
    public array $out_lines_before_patterns = array();
    public bool $print_lines = false;
    public bool $print_results = true;
    public bool $search_archives = false;
    public array $search_patterns = array();
    public string $text_file_encoding = 'utf-8';
    public bool $unique_lines = false;

    public function add_exts($ext, &$exts): void
    {
        if (gettype($ext) == 'string') {
            $xs = explode(',', $ext);
            foreach ($xs as $x) {
                $exts[] = $x;
            }
        } elseif (gettype($ext) == 'array') {
            foreach ($ext as $x) {
                $exts[] = $x;
            }
        }
    }

    public function add_file_types($file_type, &$file_types): void
    {
        if (gettype($file_type) == 'string') {
            $fts = explode(',', $file_type);
            foreach ($fts as $ft) {
                $file_types[] = FileTypes::from_name($ft);
            }
        } elseif (gettype($file_type) == 'array') {
            foreach ($file_type as $ft) {
                $file_types[] = FileTypes::from_name($ft);
            }
        }
    }

    public function add_patterns($pattern, &$patterns): void
    {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    public function set_archives_only(bool $b): void
    {
        $this->archives_only = $b;
        if ($b) {
            $this->search_archives = $b;
        }
    }

    public function set_debug(bool $b): void
    {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    public function set_search_archives(bool $b): void
    {
        $this->search_archives = $b;
        if ($b) {
            $this->include_archives = $b;
        }
    }

    public function __toString(): string
    {
        return sprintf('SearchSettings(' .
            'archives_only: %s' .
            ', colorize: %s' .
            ', debug: %s' .
            ', first_match: %s' .
            ', follow_symlinks: %s' .
            ', in_archive_extensions: %s' .
            ', in_archive_file_patterns: %s' .
            ', include_hidden: %s' .
            ', in_dir_patterns: %s' .
            ', in_extensions: %s' .
            ', in_file_patterns: %s' .
            ', in_file_types: %s' .
            ', in_lines_after_patterns: %s' .
            ', in_lines_before_patterns: %s' .
            ', lines_after: %d' .
            ', lines_after_to_patterns: %s' .
            ', lines_after_until_patterns: %s' .
            ', lines_before: %d' .
            ', max_depth: %d' .
            ', max_last_mod: %s' .
            ', max_line_length: %d' .
            ', max_size: %d' .
            ', min_depth: %d' .
            ', min_last_mod: %s' .
            ', min_size: %d' .
            ', multi_line_search: %s' .
            ', out_archive_extensions: %s' .
            ', out_archive_file_patterns: %s' .
            ', out_dir_patterns: %s' .
            ', out_extensions: %s' .
            ', out_file_patterns: %s' .
            ', out_file_types: %s' .
            ', out_lines_after_patterns: %s' .
            ', out_lines_before_patterns: %s' .
            ', paths: %s' .
            ', print_dirs: %s' .
            ', print_files: %s' .
            ', print_lines: %s' .
            ', print_results: %s' .
            ', print_usage: %s' .
            ', print_version: %s' .
            ', recursive: %s' .
            ', search_archives: %s' .
            ', search_patterns: %s' .
            ', sort_by: %s' .
            ', sort_case_insensitive: %s' .
            ', sort_descending: %s' .
            ', text_file_encoding: "%s"' .
            ', unique_lines: %s' .
            ', verbose: %s' .
            ')',
            StringUtil::bool_to_string($this->archives_only),
            StringUtil::bool_to_string($this->colorize),
            StringUtil::bool_to_string($this->debug),
            StringUtil::bool_to_string($this->first_match),
            StringUtil::bool_to_string($this->follow_symlinks),
            StringUtil::string_array_to_string($this->in_archive_extensions),
            StringUtil::string_array_to_string($this->in_archive_file_patterns),
            StringUtil::bool_to_string($this->include_hidden),
            StringUtil::string_array_to_string($this->in_dir_patterns),
            StringUtil::string_array_to_string($this->in_extensions),
            StringUtil::string_array_to_string($this->in_file_patterns),
            StringUtil::file_type_array_to_string($this->in_file_types),
            StringUtil::string_array_to_string($this->in_lines_after_patterns),
            StringUtil::string_array_to_string($this->in_lines_before_patterns),
            $this->lines_after,
            StringUtil::string_array_to_string($this->lines_after_to_patterns),
            StringUtil::string_array_to_string($this->lines_after_until_patterns),
            $this->lines_before,
            StringUtil::datetime_to_string($this->max_last_mod),
            $this->max_depth,
            $this->max_line_length,
            $this->max_size,
            $this->min_depth,
            StringUtil::datetime_to_string($this->min_last_mod),
            $this->min_size,
            StringUtil::bool_to_string($this->multi_line_search),
            StringUtil::string_array_to_string($this->out_archive_extensions),
            StringUtil::string_array_to_string($this->out_archive_file_patterns),
            StringUtil::string_array_to_string($this->out_dir_patterns),
            StringUtil::string_array_to_string($this->out_extensions),
            StringUtil::string_array_to_string($this->out_file_patterns),
            StringUtil::file_type_array_to_string($this->out_file_types),
            StringUtil::string_array_to_string($this->out_lines_after_patterns),
            StringUtil::string_array_to_string($this->out_lines_before_patterns),
            StringUtil::string_array_to_string($this->paths),
            StringUtil::bool_to_string($this->print_dirs),
            StringUtil::bool_to_string($this->print_files),
            StringUtil::bool_to_string($this->print_lines),
            StringUtil::bool_to_string($this->print_results),
            StringUtil::bool_to_string($this->print_usage),
            StringUtil::bool_to_string($this->print_version),
            StringUtil::bool_to_string($this->recursive),
            StringUtil::bool_to_string($this->search_archives),
            StringUtil::string_array_to_string($this->search_patterns),
            $this->sort_by->name,
            StringUtil::bool_to_string($this->sort_case_insensitive),
            StringUtil::bool_to_string($this->sort_descending),
            $this->text_file_encoding,
            StringUtil::bool_to_string($this->unique_lines),
            StringUtil::bool_to_string($this->verbose)
        );
    }
}
