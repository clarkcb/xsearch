<?php declare(strict_types=1);

namespace phpsearch;

use phpfind\FileUtil;

/**
 * Class SearchOptions
 */
class SearchOptions
{
    private array $options;
    private readonly array $arg_action_map;
    private readonly array $bool_flag_action_map;
    private array $long_arg_map;


    /**
     * @throws SearchException
     */
    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'encoding' => fn (string $s, SearchSettings $ss) => $ss->text_file_encoding = $s,
            'in-archiveext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_archive_extensions),
            'in-archivefilepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_archive_file_patterns),
            'in-dirpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_dir_patterns),
            'in-ext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_extensions),
            'in-filepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_file_patterns),
            'in-file_type' => fn (string $s, SearchSettings $ss) => $ss->add_file_types($s, $ss->in_file_types),
            'in-linesafterpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_lines_after_patterns),
            'in-linesbeforepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_lines_before_patterns),
            'linesafter' => fn (string $s, SearchSettings $ss) => $ss->lines_after = intval($s),
            'linesaftertopattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->lines_after_to_patterns),
            'linesafteruntilpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->lines_after_until_patterns),
            'linesbefore' => fn (string $s, SearchSettings $ss) => $ss->lines_before = intval($s),
            'maxlastmod' => fn (string $s, SearchSettings $ss) => $ss->max_last_mod = new \DateTime($s),
            'maxlinelength' => fn (string $s, SearchSettings $ss) => $ss->max_line_length = intval($s),
            'maxsize' => fn (string $s, SearchSettings $ss) => $ss->max_size = intval($s),
            'minlastmod' => fn (string $s, SearchSettings $ss) => $ss->min_last_mod = new \DateTime($s),
            'minsize' => fn (string $s, SearchSettings $ss) => $ss->min_size = intval($s),
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
            'settings-file' => fn (string $s, SearchSettings $ss) => $this->settings_from_file($s, $ss),
            'sort-by' => fn (string $s, SearchSettings $ss) => $ss->set_sort_by($s)
        ];

        $this->bool_flag_action_map = [
            'allmatches' => fn (bool $b, SearchSettings $ss) => $ss->first_match = !$b,
            'archivesonly' => fn (bool $b, SearchSettings $ss) => $ss->set_archives_only($b),
            'colorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = $b,
            'debug' => fn (bool $b, SearchSettings $ss) => $ss->set_debug($b),
            'excludehidden' => fn (bool $b, SearchSettings $ss) => $ss->exclude_hidden = $b,
            'firstmatch' => fn (bool $b, SearchSettings $ss) => $ss->first_match = $b,
            'help' => fn (bool $b, SearchSettings $ss) => $ss->print_usage = $b,
            'includehidden' => fn (bool $b, SearchSettings $ss) => $ss->exclude_hidden = !$b,
            'listdirs' => fn (bool $b, SearchSettings $ss) => $ss->list_dirs = $b,
            'listfiles' => fn (bool $b, SearchSettings $ss) => $ss->list_files = $b,
            'listlines' => fn (bool $b, SearchSettings $ss) => $ss->list_lines = $b,
            'multilinesearch' => fn (bool $b, SearchSettings $ss) => $ss->multi_line_search = $b,
            'nocolorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = !$b,
            'noprintmatches' => fn (bool $b, SearchSettings $ss) => $ss->print_results = !$b,
            'norecursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = !$b,
            'nosearcharchives' => fn (bool $b, SearchSettings $ss) => $ss->set_search_archives(!$b),
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
        $this->long_arg_map = array();
        $this->set_options_from_json();
    }

    /**
     * @throws SearchException
     */
    private function set_options_from_json(): void
    {
        $search_options_path = FileUtil::expand_user_home_path(Config::SEARCHOPTIONSPATH);
        if (file_exists($search_options_path)) {
            $json_obj = json_decode(file_get_contents($search_options_path), true);
            foreach ($json_obj['searchoptions'] as $so) {
                $short = array_key_exists('short', $so) ? sprintf('%s', $so['short']) : '';
                $long = sprintf('%s', $so['long']);
                $desc = $so['desc'];
                $func = null;
                if (array_key_exists($long, $this->arg_action_map)) {
                    $func = $this->arg_action_map[$long];
                } elseif (array_key_exists($long, $this->bool_flag_action_map)) {
                    $func = $this->bool_flag_action_map[$long];
                }
                $option = new SearchOption($short, $long, $desc, $func);
                $this->options[] = $option;
                $this->long_arg_map[$long] = $long;
                if ($short) {
                    $this->long_arg_map[$short] = $long;
                }
            }
            usort($this->options, array('phpsearch\SearchOptions', 'cmp_search_options'));
        } else {
            throw new SearchException('File not found: ' . $search_options_path);
        }
    }

    /**
     * @throws SearchException
     */
    private function settings_from_file(string $file_path, SearchSettings $settings): void
    {
        if (!file_exists($file_path)) {
            throw new SearchException('Settings file not found');
        }
        $json = file_get_contents($file_path);
        $this->settings_from_json($json, $settings);
    }

    /**
     * @throws SearchException
     */
    public function settings_from_json(string $json, SearchSettings $settings): void
    {
        $json_obj = json_decode($json, true);
        foreach (array_keys($json_obj) as $k) {
            if (array_key_exists($k, $this->arg_action_map)) {
                if (gettype($json_obj[$k]) == 'string') {
                    $this->arg_action_map[$k]($json_obj[$k], $settings);
                } elseif (gettype($json_obj[$k]) == 'integer') {
                    $this->arg_action_map[$k]((string)$json_obj[$k], $settings);
                } elseif (gettype($json_obj[$k]) == 'array') {
                    foreach ($json_obj[$k] as $s) {
                        $this->arg_action_map[$k]($s, $settings);
                    }
                } else {
                    throw new SearchException("Invalid setting type: $k");
                }
            } elseif (array_key_exists($k, $this->bool_flag_action_map)) {
                $this->bool_flag_action_map[$k]($json_obj[$k], $settings);
            } else {
                throw new SearchException("Invalid option: $k");
            }
        }
    }

    /**
     * @throws SearchException
     */
    public function settings_from_args(array $args): SearchSettings
    {
        $settings = new SearchSettings();
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
                if (array_key_exists($long_arg, $this->arg_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        $this->arg_action_map[$long_arg]($val, $settings);
                    } else {
                        throw new SearchException("Missing value for $arg");
                    }
                } elseif (array_key_exists($long_arg, $this->bool_flag_action_map)) {
                    $this->bool_flag_action_map[$long_arg](true, $settings);
                    if (in_array($long_arg, array("help", "version"))) {
                        break;
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

    public function usage(): void
    {
        echo $this->get_usage_string() . "\n";
    }

    private function get_usage_string(): string
    {
        $usage = "Usage:\n phpsearch [options] -s <searchpattern>";
        $usage .= " <path> [<path> ...]\n\nOptions:\n";
        $opt_map = array();
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

    private static function cmp_search_options(SearchOption $o1, SearchOption $o2): int
    {
        return strcmp($o1->sort_arg, $o2->sort_arg);
    }
}
