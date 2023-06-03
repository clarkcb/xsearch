<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Class SearchSettings
 *
 * @property bool archives_only
 * @property bool colorize
 * @property bool debug
 * @property bool exclude_hidden
 * @property bool first_match
 * @property array in_archive_extensions
 * @property array in_archive_file_patterns
 * @property array in_dir_patterns
 * @property array in_extensions
 * @property array in_file_patterns
 * @property array in_file_types
 * @property array in_lines_after_patterns
 * @property array in_lines_before_patterns
 * @property int lines_after
 * @property array lines_after_to_patterns
 * @property array lines_after_until_patterns
 * @property int lines_before
 * @property bool list_dirs
 * @property bool list_files
 * @property bool list_lines
 * @property int max_line_length
 * @property bool multi_line_search
 * @property array out_archive_extensions
 * @property array out_archive_file_patterns
 * @property array out_dir_patterns
 * @property array out_extensions
 * @property array out_file_patterns
 * @property array out_file_types
 * @property array out_lines_after_patterns
 * @property array out_lines_before_patterns
 * @property array paths
 * @property bool print_results
 * @property bool print_usage
 * @property bool print_version
 * @property bool recursive
 * @property bool search_archives
 * @property array search_patterns
 * @property string text_file_encoding
 * @property bool unique_lines
 * @property bool verbose
 */
class SearchSettings
{
    public bool $archives_only = false;
    public bool $colorize = true;
    public bool $debug = false;
    public bool $exclude_hidden = true;
    public bool $first_match = false;
    public int $lines_after = 0;
    public int $lines_before = 0;
    public bool $list_dirs = false;
    public bool $list_files = false;
    public bool $list_lines = false;
    public int $max_line_length = 150;
    public bool $multi_line_search = false;
    public bool $print_results = true;
    public bool $print_usage = false;
    public bool $print_version = false;
    public bool $recursive = true;
    public bool $search_archives = false;
    public string $text_file_encoding = 'utf-8';
    public bool $unique_lines = false;
    public bool $verbose = false;

    public array $in_archive_extensions = array();
    public array $in_archive_file_patterns = array();
    public array $in_dir_patterns = array();
    public array $in_extensions = array();
    public array $in_file_patterns = array();
    public array $in_file_types = array();
    public array $in_lines_after_patterns = array();
    public array $in_lines_before_patterns = array();
    public array $lines_after_to_patterns = array();
    public array $lines_after_until_patterns = array();
    public array $out_archive_extensions = array();
    public array $out_archive_file_patterns = array();
    public array $out_dir_patterns = array();
    public array $out_extensions = array();
    public array $out_file_patterns = array();
    public array $out_file_types = array();
    public array $out_lines_after_patterns = array();
    public array $out_lines_before_patterns = array();
    public array $paths = array();
    public array $search_patterns = array();

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

    private function arr_to_string(array $arr): string
    {
        $s = '["' . implode('","', $arr) . '"]';
        return $s;
    }

    private function bool_to_string(bool $b): string
    {
        return $b ? 'true' : 'false';
    }

    public function __toString(): string
    {
        return sprintf('FindSettings(' .
            'archives_only: %s' .
            ', colorize: %s' .
            ', debug: %s' .
            ', exclude_hidden: %s' .
            ', first_match: %s' .
            ', in_archive_extensions: %s' .
            ', in_archive_file_patterns: %s' .
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
            ', list_dirs: %s' .
            ', list_files: %s' .
            ', list_lines: %s' .
            ', max_line_length: %d' .
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
            ', print_results: %s' .
            ', print_usage: %s' .
            ', print_version: %s' .
            ', recursive: %s' .
            ', search_archives: %s' .
            ', search_patterns: %s' .
            ', text_file_encoding: "%s"' .
            ', unique_lines: %s' .
            ', verbose: %s' .
            ')',
            StringUtil::bool_to_string($this->archives_only),
            StringUtil::bool_to_string($this->colorize),
            StringUtil::bool_to_string($this->debug),
            StringUtil::bool_to_string($this->exclude_hidden),
            StringUtil::bool_to_string($this->first_match),
            StringUtil::string_array_to_string($this->in_archive_extensions),
            StringUtil::string_array_to_string($this->in_archive_file_patterns),
            StringUtil::string_array_to_string($this->in_dir_patterns),
            StringUtil::string_array_to_string($this->in_extensions),
            StringUtil::string_array_to_string($this->in_file_patterns),
            StringUtil::string_array_to_string($this->in_file_types),
            StringUtil::string_array_to_string($this->in_lines_after_patterns),
            StringUtil::string_array_to_string($this->in_lines_before_patterns),
            $this->lines_after,
            StringUtil::string_array_to_string($this->lines_after_to_patterns),
            StringUtil::string_array_to_string($this->lines_after_until_patterns),
            $this->lines_before,
            StringUtil::bool_to_string($this->list_dirs),
            StringUtil::bool_to_string($this->list_files),
            StringUtil::bool_to_string($this->list_lines),
            $this->max_line_length,
            StringUtil::bool_to_string($this->multi_line_search),
            StringUtil::string_array_to_string($this->out_archive_extensions),
            StringUtil::string_array_to_string($this->out_archive_file_patterns),
            StringUtil::string_array_to_string($this->out_dir_patterns),
            StringUtil::string_array_to_string($this->out_extensions),
            StringUtil::string_array_to_string($this->out_file_patterns),
            StringUtil::string_array_to_string($this->out_file_types),
            StringUtil::string_array_to_string($this->out_lines_after_patterns),
            StringUtil::string_array_to_string($this->out_lines_before_patterns),
            StringUtil::string_array_to_string($this->paths),
            StringUtil::bool_to_string($this->print_results),
            StringUtil::bool_to_string($this->print_usage),
            StringUtil::bool_to_string($this->print_version),
            StringUtil::bool_to_string($this->recursive),
            StringUtil::bool_to_string($this->search_archives),
            StringUtil::string_array_to_string($this->search_patterns),
            $this->text_file_encoding,
            StringUtil::bool_to_string($this->unique_lines),
            StringUtil::bool_to_string($this->verbose)
        );
    }
}
