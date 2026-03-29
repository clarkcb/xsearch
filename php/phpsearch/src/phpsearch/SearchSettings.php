<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\Color;
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
    public Color $line_color = Color::Green;
    public int $lines_after = 0;
    public array $lines_after_to_patterns = array();
    public array $lines_after_until_patterns = array();
    public int $lines_before = 0;
    public int $max_line_length = 150;
    public bool $multi_line_search = false;
    public array $out_lines_after_patterns = array();
    public array $out_lines_before_patterns = array();
    public bool $print_lines = false;
    public bool $print_matches = false;
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
}
