<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\ArgTokenizer;
use phpfind\ArgTokenType;
use phpfind\FileUtil;
use phpfind\FindException;

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
     * @var ArgTokenizer $arg_tokenizer
     */
    private ArgTokenizer $arg_tokenizer;


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
            'noprintmatches' => fn (bool $b, SearchSettings $ss) => $ss->print_matches = !$b,
            'noprintresults' => fn (bool $b, SearchSettings $ss) => $ss->print_results = !$b,
            'norecursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = !$b,
            'nosearcharchives' => fn (bool $b, SearchSettings $ss) => $ss->set_search_archives(!$b),
            'printdirs' => fn (bool $b, SearchSettings $ss) => $ss->print_dirs = $b,
            'printfiles' => fn (bool $b, SearchSettings $ss) => $ss->print_files = $b,
            'printlines' => fn (bool $b, SearchSettings $ss) => $ss->print_lines = $b,
            'printmatches' => fn (bool $b, SearchSettings $ss) => $ss->print_matches = $b,
            'printresults' => fn (bool $b, SearchSettings $ss) => $ss->print_results = $b,
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
            'settings-file' => fn (string $s, SearchSettings $ss) => $this->update_settings_from_file($ss, $s),
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

        $this->set_options_from_json();
        $this->arg_tokenizer = new ArgTokenizer($this->options);
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
                        if (array_key_exists('short', $so)) {
                            $short = (string)$so['short'];
                        }
                        $long = (string)$so['long'];
                        $desc = (string)$so['desc'];
                        if (array_key_exists($long, $this->bool_action_map)) {
                            $arg_type = ArgTokenType::Bool;
                        } elseif (array_key_exists($long, $this->str_action_map)) {
                            $arg_type = ArgTokenType::Str;
                        } elseif (array_key_exists($long, $this->int_action_map)) {
                            $arg_type = ArgTokenType::Int;
                        } else {
                            throw new SearchException('Invalid option: ' . $long);
                        }
                        $this->options[] = new SearchOption($short, $long, $desc, $arg_type);
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
     * @param SearchSettings $settings
     * @param array $arg_tokens
     * @return void
     * @throws SearchException
     */
    private function update_settings_from_arg_tokens(SearchSettings $settings, array $arg_tokens): void
    {
        foreach ($arg_tokens as $arg_token) {
            if ($arg_token->type == ArgTokenType::Bool) {
                if (is_bool($arg_token->value)) {
                    $this->bool_action_map[$arg_token->name]($arg_token->value, $settings);
                    if (in_array($arg_token->name, array("help", "version"))) {
                        return;
                    }
                } else {
                    throw new SearchException("Invalid value for option: $arg_token->name");
                }
            } elseif ($arg_token->type == ArgTokenType::Str) {
                if (is_array($arg_token->value)) {
                    foreach ($arg_token->value as $val) {
                        if (is_string($val)) {
                            $this->str_action_map[$arg_token->name]($val, $settings);
                        } else {
                            throw new SearchException("Invalid value for option: $arg_token->name");
                        }
                    }
                } elseif (is_string($arg_token->value)) {
                    $this->str_action_map[$arg_token->name]($arg_token->value, $settings);
                } else {
                    throw new SearchException("Invalid value for option: $arg_token->name");
                }
            } elseif ($arg_token->type == ArgTokenType::Int) {
                if (is_int($arg_token->value)) {
                    $this->int_action_map[$arg_token->name]($arg_token->value, $settings);
                } else {
                    throw new SearchException("Invalid value for option: $arg_token->name");
                }
            } else {
                throw new SearchException("Invalid option: $arg_token->name");
            }
        }
    }

    /**
     * @param SearchSettings $settings
     * @param string $json
     * @return void
     * @throws SearchException
     */
    public function update_settings_from_json(SearchSettings $settings, string $json): void
    {
        try {
            $arg_tokens = $this->arg_tokenizer->tokenize_json($json);
            $this->update_settings_from_arg_tokens($settings, $arg_tokens);
        } catch (FindException $e) {
            throw new SearchException($e->getMessage());
        }
    }

    /**
     * @param SearchSettings $settings
     * @param string $file_path
     * @return void
     * @throws SearchException
     */
    private function update_settings_from_file(SearchSettings $settings, string $file_path): void
    {
        try {
            $arg_tokens = $this->arg_tokenizer->tokenize_file($file_path);
            $this->update_settings_from_arg_tokens($settings, $arg_tokens);
        } catch (FindException $e) {
            throw new SearchException($e->getMessage());
        }
    }

    /**
     * @param SearchSettings $settings
     * @param string[] $args
     * @return void
     * @throws SearchException
     */
    public function update_settings_from_args(SearchSettings $settings, array $args): void
    {
        try {
            $arg_tokens = $this->arg_tokenizer->tokenize_args($args);
            $this->update_settings_from_arg_tokens($settings, $arg_tokens);
        } catch (FindException $e) {
            throw new SearchException($e->getMessage());
        }
    }

    /**
     * @param string[] $args
     * @return SearchSettings
     * @throws SearchException
     */
    public function settings_from_args(array $args): SearchSettings
    {
        $settings = new SearchSettings();
        // default print_results to true since running as cli
        $settings->print_results = true;
        $this->update_settings_from_args($settings, $args);
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
