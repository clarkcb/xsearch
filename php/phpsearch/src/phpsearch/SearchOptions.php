<?php declare(strict_types=1);

namespace phpsearch;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class SearchOptions
 *
 * @property array options
 * @property array arg_action_map
 * @property array bool_flag_action_map
 * @property array longarg_map
 */
class SearchOptions
{
    private array $options;
    private readonly array $arg_action_map;
    private readonly array $bool_flag_action_map;
    private array $longarg_map;


    /**
     * @throws SearchException
     */
    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'encoding' => fn (string $s, SearchSettings $ss) => $ss->textfileencoding = $s,
            'in-archiveext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_archiveextensions),
            'in-archivefilepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_archivefilepatterns),
            'in-dirpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_dirpatterns),
            'in-ext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->in_extensions),
            'in-filepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_filepatterns),
            'in-filetype' => fn (string $s, SearchSettings $ss) => $ss->add_filetypes($s, $ss->in_filetypes),
            'in-linesafterpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_linesafterpatterns),
            'in-linesbeforepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->in_linesbeforepatterns),
            'linesafter' => fn (string $s, SearchSettings $ss) => $ss->linesafter = intval($s),
            'linesaftertopattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->linesaftertopatterns),
            'linesafteruntilpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->linesafteruntilpatterns),
            'linesbefore' => fn (string $s, SearchSettings $ss) => $ss->linesbefore = intval($s),
            'maxlinelength' => fn (string $s, SearchSettings $ss) => $ss->maxlinelength = intval($s),
            'out-archiveext' =>
                fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->out_archiveextensions),
            'out-archivefilepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_archivefilepatterns),
            'out-dirpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_dirpatterns),
            'out-ext' => fn (string $s, SearchSettings $ss) => $ss->add_exts($s, $ss->out_extensions),
            'out-filepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_filepatterns),
            'out-filetype' => fn (string $s, SearchSettings $ss) => $ss->add_filetypes($s, $ss->out_filetypes),
            'out-linesafterpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_linesafterpatterns),
            'out-linesbeforepattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->out_linesbeforepatterns),
            'path' => fn (string $s, SearchSettings $ss) => $ss->paths[] = $s,
            'searchpattern' =>
                fn (string $s, SearchSettings $ss) => $ss->add_patterns($s, $ss->searchpatterns),
            'settings-file' => fn (string $s, SearchSettings $ss) => $this->settings_from_file($s, $ss)
        ];

        $this->bool_flag_action_map = [
            'allmatches' => fn (bool $b, SearchSettings $ss) => $ss->firstmatch = !$b,
            'archivesonly' => fn (bool $b, SearchSettings $ss) => $ss->set_archivesonly($b),
            'colorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = $b,
            'debug' => fn (bool $b, SearchSettings $ss) => $ss->set_debug($b),
            'excludehidden' => fn (bool $b, SearchSettings $ss) => $ss->excludehidden = $b,
            'firstmatch' => fn (bool $b, SearchSettings $ss) => $ss->firstmatch = $b,
            'help' => fn (bool $b, SearchSettings $ss) => $ss->printusage = $b,
            'includehidden' => fn (bool $b, SearchSettings $ss) => $ss->excludehidden = !$b,
            'listdirs' => fn (bool $b, SearchSettings $ss) => $ss->listdirs = $b,
            'listfiles' => fn (bool $b, SearchSettings $ss) => $ss->listfiles = $b,
            'listlines' => fn (bool $b, SearchSettings $ss) => $ss->listlines = $b,
            'multilinesearch' => fn (bool $b, SearchSettings $ss) => $ss->multilinesearch = $b,
            'nocolorize' => fn (bool $b, SearchSettings $ss) => $ss->colorize = !$b,
            'noprintmatches' => fn (bool $b, SearchSettings $ss) => $ss->printresults = !$b,
            'norecursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = !$b,
            'nosearcharchives' => fn (bool $b, SearchSettings $ss) => $ss->searcharchives = !$b,
            'printmatches' => fn (bool $b, SearchSettings $ss) => $ss->printresults = $b,
            'recursive' => fn (bool $b, SearchSettings $ss) => $ss->recursive = $b,
            'searcharchives' => fn (bool $b, SearchSettings $ss) => $ss->searcharchives = $b,
            'uniquelines' => fn (bool $b, SearchSettings $ss) => $ss->uniquelines = $b,
            'verbose' => fn (bool $b, SearchSettings $ss) => $ss->verbose = $b,
            'version' => fn (bool $b, SearchSettings $ss) => $ss->printversion = $b
        ];
        $this->longarg_map = array();
        $this->set_options_from_json();
    }

    /**
     * @throws SearchException
     */
    private function set_options_from_json()
    {
        $searchoptionspath = FileUtil::expand_user_home_path(Config::SEARCHOPTIONSPATH);
        if (file_exists($searchoptionspath)) {
            $json_obj = json_decode(file_get_contents($searchoptionspath), true);
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
                $this->longarg_map[$long] = $long;
                if ($short) {
                    $this->longarg_map[$short] = $long;
                }
            }
            usort($this->options, array('phpsearch\SearchOptions', 'cmp_searchoptions'));
        } else {
            throw new SearchException('File not found: ' . $searchoptionspath);
        }
    }

    /**
     * @throws SearchException
     */
    private function settings_from_file(string $filepath, SearchSettings $settings)
    {
        if (!file_exists($filepath)) {
            throw new SearchException('Settings file not found');
        }
        $json = file_get_contents($filepath);
        $this->settings_from_json($json, $settings);
    }

    /**
     * @throws SearchException
     */
    public function settings_from_json(string $json, SearchSettings $settings)
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
                if (!array_key_exists($arg, $this->longarg_map)) {
                    throw new SearchException("Invalid option: $arg");
                }
                $longarg = $this->longarg_map[$arg];
                if (array_key_exists($longarg, $this->arg_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        $this->arg_action_map[$longarg]($val, $settings);
                    } else {
                        throw new SearchException("Missing value for $arg");
                    }
                } elseif (array_key_exists($longarg, $this->bool_flag_action_map)) {
                    $this->bool_flag_action_map[$longarg](true, $settings);
                    if (in_array($longarg, array("help", "version"))) {
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
            if ($option->shortarg) {
                $opt_str = '-' . $option->shortarg . ',';
            }
            $opt_str .= '--' . $option->longarg;
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

    private static function cmp_searchoptions(SearchOption $o1, SearchOption $o2): int
    {
        return strcmp($o1->sortarg, $o2->sortarg);
    }
}
