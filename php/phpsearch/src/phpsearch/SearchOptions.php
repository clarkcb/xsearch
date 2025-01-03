<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileUtil;

/**
 * Class SearchOptions
 */
class SearchOptions
{
    /**
     * @var SearchOption[] $options
     */
    private array $options;
    /**
     * @var array<string, \Closure> $bool_action_map
     */
    private readonly array $bool_action_map;
    /**
     * @var array<string, \Closure> $str_action_map
     */
    private readonly array $str_action_map;
    /**
     * @var array<string, \Closure> $int_action_map
     */
    private readonly array $int_action_map;
    /**
     * @var array<string, string> $long_arg_map
     */
    private array $long_arg_map;


    /**
     * @throws SearchException
     */
    public function __construct()
    {
        $this->options = [];

        $this->bool_action_map = [
            'allmatches' => fn (bool $b, SearchSettings $ss) => $ss->first_match = !$b,
            'archivesonly' => fn (bool $b, SearchSettings $ss) => $ss->set_archives_only($b),
            'colorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = $b,
            'debug' => fn (bool $b, SearchSettings $ss) => $ss->set_debug($b),
            'excludehidden' => fn (bool $b, SearchSettings $ss) => $ss->include_hidden = !$b,
            'firstmatch' => fn (bool $b, SearchSettings $ss) => $ss->first_match = $b,
            'followsymlinks' => fn (bool $b, SearchSettings $ss) => $ss->follow_symlinks = $b,
            'help' => fn (bool $b, SearchSettings $ss) => $ss->print_usage = $b,
            'includehidden' => fn (bool $b, SearchSettings $ss) => $ss->include_hidden = $b,
            'multilinesearch' => fn (bool $b, SearchSettings $ss) => $ss->multi_line_search = $b,
            'nocolorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = !$b,
            'nofollowsymlinks' => fn (bool $b, SearchSettings $ss) => $ss->follow_symlinks = !$b,
            'noprintdirs' => fn (bool $b, SearchSettings $ss) => $ss->print_dirs = !$b,
            'noprintfiles' => fn (bool $b, SearchSettings $ss) => $ss->print_files = !$b,
            'noprintlines' => fn (bool $b, SearchSettings $ss) => $ss->print_lines = !$b,
            'noprintmatches' => fn (bool $b, SearchSettings $ss) => $ss->print_results = !$b,
            'norecursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = !$b,
            'nosearcharchives' => fn (bool $b, SearchSettings $ss) => $ss->set_search_archives(!$b),
            'printdirs' => fn (bool $b, SearchSettings $ss) => $ss->print_dirs = $b,
            'printfiles' => fn (bool $b, SearchSettings $ss) => $ss->print_files = $b,
            'printlines' => fn (bool $b, SearchSettings $ss) => $ss->print_lines = $b,
            'printmatches' => fn (bool $b, SearchSettings $ss) => $ss->print_results = $b,
            'recursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = $b,
            'searcharchives' => fn (bool $b, SearchSettings $ss) => $ss->set_search_archives($b),
            'sort-ascending' => fn (bool $b, SearchSettings $ss) => $ss->sort_descending = !$b,
            'sort-caseinsensitive' => fn (bool $b, SearchSettings $ss) => $ss->sort_case_insensitive = $b,
            'sort-casesensitive' => fn (bool $b, SearchSettings $ss) => $ss->sort_case_insensitive = !$b,
            'sort-descending' => fn (bool $b, SearchSettings $ss) => $ss->sort_descending = $b,
            'uniquelines' => fn (bool $b, SearchSettings $ss) => $ss->unique_lines = $b,
            'verbose' => fn (bool $b, SearchSettings $ss) => $ss->verbose = $b,
            'version' => fn (bool $b, SearchSettings $ss) => $ss->print_version = $b
        ];

        $this->str_action_map = [
            'encoding' => fn (string $s, SearchSettings $ss) => $ss->text_file_encoding = $s,
            'in-archiveext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_archive_extensions),
            'in-archivefilepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_archive_file_patterns),
            'in-dirpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_dir_patterns),
            'in-ext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_extensions),
            'in-filepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_file_patterns),
            'in-filetype' => fn (string $s, SearchSettings $ss) => $ss->add_file_types($s, $ss->in_file_types),
            'in-linesafterpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_lines_after_patterns),
            'in-linesbeforepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_lines_before_patterns),
            'linesaftertopattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->lines_after_to_patterns),
            'linesafteruntilpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->lines_after_until_patterns),
            'maxlastmod' => fn (string $s, SearchSettings $ss) => $ss->max_last_mod = new \DateTime($s),
            'minlastmod' => fn (string $s, SearchSettings $ss) => $ss->min_last_mod = new \DateTime($s),
            'out-archiveext' =>
                fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->out_archive_extensions),
            'out-archivefilepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_archive_file_patterns),
            'out-dirpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_dir_patterns),
            'out-ext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->out_extensions),
            'out-filepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_file_patterns),
            'out-filetype' => fn (string $s, SearchSettings $ss) => $ss->add_file_types($s, $ss->out_file_types),
            'out-linesafterpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_lines_after_patterns),
            'out-linesbeforepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_lines_before_patterns),
            'path' => fn (string $s, SearchSettings $ss) => $ss->paths[] = $s,
            'searchpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->search_patterns),
            'settings-file' => fn (string $s, SearchSettings $ss) => $this->update_settings_from_file($s, $ss),
            'sort-by' => fn (string $s, SearchSettings $ss) => $ss->set_sort_by($s)
        ];

        $this->int_action_map = [
            'linesafter' => fn (int $i, SearchSettings $ss) => $ss->lines_after = $i,
            'linesbefore' => fn (int $i, SearchSettings $ss) => $ss->lines_before = $i,
            'maxdepth' => fn (int $i, SearchSettings $ss) => $ss->max_depth = $i,
            'maxlinelength' => fn (int $i, SearchSettings $ss) => $ss->max_line_length = $i,
            'maxsize' => fn (int $i, SearchSettings $ss) => $ss->max_size = $i,
            'mindepth' => fn (int $i, SearchSettings $ss) => $ss->min_depth = $i,
            'minsize' => fn (int $i, SearchSettings $ss) => $ss->min_size = $i
        ];

        $this->long_arg_map = ['path' => 'path'];
        $this->set_options_from_json();
    }

    /**
     * @throws SearchException
     */
    private function set_options_from_json(): void
    {
        $search_options_path = FileUtil::expand_path(Config::SEARCH_OPTIONS_PATH);
        if (file_exists($search_options_path)) {
            $contents = file_get_contents($search_options_path);
            if ($contents === false || trim($contents) === '') {
                return;
            }

            try {
                $json_obj = json_decode(trim($contents), true, 512, JSON_THROW_ON_ERROR);
                $search_options = $json_obj['searchoptions'];
                if ($search_options) {
                    foreach ((array)$search_options as $so) {
                        $so = (array)$so;
                        $short = '';
                        $long = (string)$so['long'];
                        $desc = (string)$so['desc'];
                        $this->long_arg_map[$long] = $long;
                        if (array_key_exists('short', $so)) {
                            $short = (string)$so['short'];
                            $this->long_arg_map[$short] = $long;
                        }
                        $this->options[] = new SearchOption($short, $long, $desc);
                    }
                    usort($this->options, array('phpsearch\SearchOptions', 'cmp_search_options'));
                }

            } catch (\JsonException $e) {
                throw new SearchException($e->getMessage());
            }
        } else {
            throw new SearchException('File not found: ' . $search_options_path);
        }
    }

    /**
     * @param string $json
     * @param SearchSettings $settings
     * @return void
     * @throws SearchException|\JsonException
     */
    public function update_settings_from_json(string $json, SearchSettings $settings): void
    {
        if (trim($json) === '') {
            return;
        }

        try {
            $json_obj = (array)json_decode(trim($json), true, 512, JSON_THROW_ON_ERROR);
            $keys = array_keys($json_obj);
            # keys are sorted so that output is consistent across all versions
            sort($keys);
            $is_invalid_key = fn (string $k) => !array_key_exists($k, $this->long_arg_map);
            $invalid_keys = array_filter($keys, $is_invalid_key);
            if ($invalid_keys) {
                throw new SearchException("Invalid option: " . array_values($invalid_keys)[0]);
            }
            foreach ($keys as $k) {
                if (array_key_exists($k, $this->bool_action_map)) {
                    if (gettype($json_obj[$k]) == 'boolean') {
                        $this->bool_action_map[$k]($json_obj[$k], $settings);
                    } else {
                        throw new SearchException("Invalid value for option: $k");
                    }
                } elseif (array_key_exists($k, $this->str_action_map)) {
                    if (gettype($json_obj[$k]) == 'string') {
                        $this->str_action_map[$k]($json_obj[$k], $settings);
                    } elseif (gettype($json_obj[$k]) == 'array') {
                        foreach ($json_obj[$k] as $s) {
                            $this->str_action_map[$k]($s, $settings);
                        }
                    } else {
                        throw new SearchException("Invalid setting type: $k");
                    }
                } elseif (array_key_exists($k, $this->int_action_map)) {
                    if (gettype($json_obj[$k]) == 'integer') {
                        $this->int_action_map[$k]($json_obj[$k], $settings);
                    } else {
                        throw new SearchException("Invalid value for option: $k");
                    }
                } else {
                    # Should never reach here
                    throw new SearchException("Invalid option: $k");
                }
            }
        } catch (\JsonException $e) {
            throw $e;
        }
    }

    /**
     * @param string $file_path
     * @param SearchSettings $settings
     * @return void
     * @throws SearchException
     */
    private function update_settings_from_file(string $file_path, SearchSettings $settings): void
    {
        $expanded_path = FileUtil::expand_path($file_path);
        if (!file_exists($expanded_path)) {
            throw new SearchException("Settings file not found: $file_path");
        }
        if (!str_ends_with($expanded_path, '.json')) {
            throw new SearchException("Invalid settings file (must be JSON): $file_path");
        }
        try {
            $json = file_get_contents($expanded_path);
            if ($json) {
                $this->update_settings_from_json($json, $settings);
            }
        } catch (\JsonException $e) {
            throw new SearchException("Unable to parse JSON in settings file: $file_path");
        }
    }

    /**
     * @param string[] $args
     * @return SearchSettings $settings
     * @throws SearchException
     */
    public function settings_from_args(array $args): SearchSettings
    {
        $settings = new SearchSettings();
        // default print_results to true since running as cli
        $settings->print_results = true;
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg[0] == '-') {
                while ($arg[0] == '-') {
                    $arg = substr($arg, 1);
                }
                if (!array_key_exists($arg, $this->long_arg_map)) {
                    throw new SearchException("Invalid option: $arg");
                }
                $long_arg = $this->long_arg_map[$arg];
                if (array_key_exists($long_arg, $this->bool_action_map)) {
                    $this->bool_action_map[$long_arg](true, $settings);
                    if (in_array($long_arg, array("help", "version"))) {
                        break;
                    }
                } elseif (array_key_exists($long_arg, $this->str_action_map)
                          || array_key_exists($long_arg, $this->int_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        if (array_key_exists($long_arg, $this->str_action_map)) {
                            $this->str_action_map[$long_arg]($val, $settings);
                        } elseif (array_key_exists($long_arg, $this->int_action_map)) {
                            $this->int_action_map[$long_arg](intval($val), $settings);
                        }
                    } else {
                        throw new SearchException("Missing value for $arg");
                    }
                } else {
                    throw new SearchException("Invalid option: $arg");
                }
            } else {
                $settings->paths[] = $arg;
            }
        }
        return $settings;
    }

    private static function cmp_search_options(SearchOption $o1, SearchOption $o2): int
    {
        return strcmp($o1->sort_arg, $o2->sort_arg);
    }

    /**
     * @return string
     */
    private function get_usage_string(): string
    {
        $usage = "Usage:\n phpsearch [options] -s <searchpattern>";
        $usage .= " <path> [<path> ...]\n\nOptions:\n";
        $opt_map = [];
        $longest = 0;
        foreach ($this->options as $option) {
            $opt_str = '';
            if ($option->short_arg) {
                $opt_str = '-' . $option->short_arg . ',';
            }
            $opt_str .= '--' . $option->long_arg;
            if (strlen($opt_str) > $longest) {
                $longest = strlen($opt_str);
            }
            $opt_map[$opt_str] = $option->desc;
        }
        $format_str = " %-" . $longest . "s  %s\n";
        foreach ($opt_map as $opt => $desc) {
            $usage .= sprintf($format_str, $opt, $desc);
        }
        return $usage;
    }

    /**
     * @return void
     */
    public function usage(): void
    {
        echo $this->get_usage_string() . "\n";
    }

    /**
     * @param int $exit_code
     * @return never
     */
    public function usage_and_exit(int $exit_code = 0): never
    {
        $this->usage();
        exit($exit_code);
    }
}
