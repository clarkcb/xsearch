<?php declare(strict_types=1);

namespace phpsearch;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class SearchOptions
 */
class SearchOptions
{
    private $options;

    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'encoding' => function (string $s, SearchSettings $settings) {
                $settings->textfileencoding = $s;
            },
            'in-archiveext' => function (string $s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_archiveextensions);
            },
            'in-archivefilepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_archivefilepatterns);
            },
            'in-dirpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_dirpatterns);
            },
            'in-ext' => function (string $s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_extensions);
            },
            'in-filepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_filepatterns);
            },
            'in-filetype' => function (string $s, SearchSettings $settings) {
                $settings->add_filetypes($s, $settings->in_filetypes);
            },
            'in-linesafterpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_linesafterpatterns);
            },
            'in-linesbeforepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_linesbeforepatterns);
            },
            'linesafter' => function (string $s, SearchSettings $settings) {
                $settings->linesafter = intval($s);
            },
            'linesaftertopattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->linesaftertopatterns);
            },
            'linesafteruntilpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->linesafteruntilpatterns);
            },
            'linesbefore' => function (string $s, SearchSettings $settings) {
                $settings->linesbefore = intval($s);
            },
            'maxlinelength' => function (string $s, SearchSettings $settings) {
                $settings->maxlinelength = intval($s);
            },
            'out-archiveext' => function (string $s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->out_archiveextensions);
            },
            'out-archivefilepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_archivefilepatterns);
            },
            'out-dirpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_dirpatterns);
            },
            'out-ext' => function (string $s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->out_extensions);
            },
            'out-filepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_filepatterns);
            },
            'out-filetype' => function (string $s, SearchSettings $settings) {
                $settings->add_filetypes($s, $settings->out_filetypes);
            },
            'out-linesafterpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_linesafterpatterns);
            },
            'out-linesbeforepattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_linesbeforepatterns);
            },
            'searchpattern' => function (string $s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->searchpatterns);
            },
            'settings-file' => function (string $s, SearchSettings $settings) {
                $this->settings_from_file($s, $settings);
            }
        ];

        $this->bool_flag_action_map = [
            'allmatches' => function (bool $b, SearchSettings $settings) {
                $settings->firstmatch = !$b;
            },
            'archivesonly' => function (bool $b, SearchSettings $settings) {
                $settings->set_archivesonly($b);
            },
            'colorize' => function (bool $b, SearchSettings $settings) {
                $settings->colorize = $b;
            },
            'debug' => function (bool $b, SearchSettings $settings) {
                $settings->set_debug($b);
            },
            'excludehidden' => function (bool $b, SearchSettings $settings) {
                $settings->excludehidden = $b;
            },
            'firstmatch' => function (bool $b, SearchSettings $settings) {
                $settings->firstmatch = $b;
            },
            'help' => function (bool $b, SearchSettings $settings) {
                $settings->printusage = $b;
            },
            'includehidden' => function (bool $b, SearchSettings $settings) {
                $settings->excludehidden = !$b;
            },
            'listdirs' => function (bool $b, SearchSettings $settings) {
                $settings->listdirs = $b;
            },
            'listfiles' => function (bool $b, SearchSettings $settings) {
                $settings->listfiles = $b;
            },
            'listlines' => function (bool $b, SearchSettings $settings) {
                $settings->listlines = $b;
            },
            'multilinesearch' => function (bool $b, SearchSettings $settings) {
                $settings->multilinesearch = $b;
            },
            'nocolorize' => function (bool $b, SearchSettings $settings) {
                $settings->colorize = !$b;
            },
            'noprintmatches' => function (bool $b, SearchSettings $settings) {
                $settings->printresults = !$b;
            },
            'norecursive' => function (bool $b, SearchSettings $settings) {
                $settings->recursive = !$b;
            },
            'nosearcharchives' => function (bool $b, SearchSettings $settings) {
                $settings->searcharchives = !$b;
            },
            'printmatches' => function (bool $b, SearchSettings $settings) {
                $settings->printresults = $b;
            },
            'recursive' => function (bool $b, SearchSettings $settings) {
                $settings->recursive = $b;
            },
            'searcharchives' => function (bool $b, SearchSettings $settings) {
                $settings->searcharchives = $b;
            },
            'uniquelines' => function (bool $b, SearchSettings $settings) {
                $settings->uniquelines = $b;
            },
            'verbose' => function (bool $b, SearchSettings $settings) {
                $settings->verbose = $b;
            },
            'version' => function (bool $b, SearchSettings $settings) {
                $settings->printversion = $b;
            }
        ];
        $this->longarg_map = array();
        $this->set_options_from_json();
    }

    private function set_options_from_json()
    {
        $searchoptionspath = FileUtil::expand_user_home_path(Config::SEARCHOPTIONSPATH);
        if (file_exists($searchoptionspath)) {
            $json_obj = json_decode(file_get_contents($searchoptionspath), true);
            foreach ($json_obj['searchoptions'] as $so) {
                $short = sprintf($so['short']);
                $long = sprintf($so['long']);
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
//            usort($this->options, array('common', 'cmp_searchoptions'));
//            usort($this->options, 'cmp_searchoptions');
        } else {
            throw new SearchException('File not found: ' . $searchoptionspath);
        }
    }

    private function settings_from_file(string $filepath, SearchSettings $settings)
    {
        if (!file_exists($filepath)) {
            throw new SearchException('Settings file not found');
        }
        $json = file_get_contents($filepath);
        $this->settings_from_json($json, $settings);
    }

    public function settings_from_json(string $json, SearchSettings $settings)
    {
        $json_obj = json_decode($json, true);
        foreach (array_keys($json_obj) as $k) {
            if (array_key_exists($k, $this->arg_action_map)) {
                if (gettype($json_obj[$k]) == 'string') {
                    $this->arg_action_map[$k]($json_obj[$k], $settings);
                } elseif (gettype($json_obj[$k]) == 'integer') {
                    $this->arg_action_map[$k](sprintf($json_obj[$k]), $settings);
                } elseif (gettype($json_obj[$k]) == 'array') {
                    foreach ($json_obj[$k] as $s) {
                        $this->arg_action_map[$k]($s, $settings);
                    }
                } else {
                    throw new SearchException("Invalid setting type: $k");
                }
            } elseif (array_key_exists($k, $this->bool_flag_action_map)) {
                $this->bool_flag_action_map[$k]($json_obj[$k], $settings);
            } elseif ($k == 'startpath') {
                $settings->startpath = $json_obj[$k];
            } else {
                throw new SearchException("Invalid option: $k");
            }
        }
    }

    public function settings_from_args(array $args): SearchSettings
    {
        $settings = new SearchSettings();
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg{0} == '-') {
                while ($arg{0} == '-') {
                    $arg = substr($arg, 1);
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
                $settings->startpath = $arg;
            }
        }
        return $settings;
    }

    public function usage()
    {
        echo $this->get_usage_string() . "\n";
    }

    private function get_usage_string(): string
    {
        $usage = "Usage:\n phpsearch [options] -s <searchpattern>";
        $usage .= " <startpath>\n\nOptions:\n";
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
