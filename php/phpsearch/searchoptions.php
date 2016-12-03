<?php

require_once __DIR__ . '/autoload.php';

class SearchOptions {
    function __construct() {
        $this->options = array();

        $this->arg_action_map = [
            'in-archiveext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_archiveextensions);
            },
            'in-archivefilepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_archivefilepatterns);
            },
            'in-dirpattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_dirpatterns);
            },
            'in-ext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_extensions);
            },
            'in-filepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_filepatterns);
            },
            'in-filetype' => function($s, SearchSettings $settings) {
                $settings->add_filetypes($s, $settings->in_filetypes);
            },
            'in-linesafterpattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_linesafterpatterns);
            },
            'in-linesbeforepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->in_linesbeforepatterns);
            },
            'linesafter' => function($s, SearchSettings $settings) {
                $settings->linesafter = intval($s);
            },
            'linesaftertopattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->linesaftertopatterns);
            },
            'linesafteruntilpattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->linesafteruntilpatterns);
            },
            'linesbefore' => function($s, SearchSettings $settings) {
                $settings->linesbefore = intval($s);
            },
            'maxlinelength' => function($s, SearchSettings $settings) {
                $settings->maxlinelength = intval($s);
            },
            'out-archiveext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->out_archiveextensions);
            },
            'out-archivefilepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_archivefilepatterns);
            },
            'out-dirpattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_dirpatterns);
            },
            'out-ext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->out_extensions);
            },
            'out-filepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_filepatterns);
            },
            'out-filetype' => function($s, SearchSettings $settings) {
                $settings->add_filetype($s, $settings->out_filetypes);
            },
            'out-linesafterpattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_linesafterpatterns);
            },
            'out-linesbeforepattern' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->out_linesbeforepatterns);
            },
            'search' => function($s, SearchSettings $settings) {
                $settings->add_patterns($s, $settings->searchpatterns);
            },
            'settings-file' => function($s, SearchSettings $settings) {
                $this->settings_from_file($s, $settings);
            }
        ];

        $this->bool_flag_action_map = [
            'allmatches' => function($b, SearchSettings $settings) {
                $settings->firstmatch = !$b;
            },
            'archivesonly' => function($b, SearchSettings $settings) {
                $settings->set_archivesonly($b);
            },
            'debug' => function($b, SearchSettings $settings) {
                $settings->set_debug($b);
            },
            'excludehidden' => function($b, SearchSettings $settings) {
                $settings->excludehidden = $b;
            },
            'firstmatch' => function($b, SearchSettings $settings) {
                $settings->firstmatch = $b;
            },
            'help' => function($b, SearchSettings $settings) {
                $settings->printusage = $b;
            },
            'includehidden' => function($b, SearchSettings $settings) {
                $settings->excludehidden = !$b;
            },
            'listdirs' => function($b, SearchSettings $settings) {
                $settings->listdirs = $b;
            },
            'listfiles' => function($b, SearchSettings $settings) {
                $settings->listfiles = $b;
            },
            'listlines' => function($b, SearchSettings $settings) {
                $settings->listlines = $b;
            },
            'multilinesearch' => function($b, SearchSettings $settings) {
                $settings->multilinesearch = $b;
            },
            'noprintmatches' => function($b, SearchSettings $settings) {
                $settings->printresults = !$b;
            },
            'norecursive' => function($b, SearchSettings $settings) {
                $settings->recursive = !$b;
            },
            'nosearcharchives' => function($b, SearchSettings $settings) {
                $settings->searcharchives = !$b;
            },
            'printmatches' => function($b, SearchSettings $settings) {
                $settings->printresults = $b;
            },
            'recursive' => function($b, SearchSettings $settings) {
                $settings->recursive = $b;
            },
            'searcharchives' => function($b, SearchSettings $settings) {
                $settings->searcharchives = $b;
            },
            'uniquelines' => function($b, SearchSettings $settings) {
                $settings->uniquelines = $b;
            },
            'verbose' => function($b, SearchSettings $settings) {
                $settings->verbose = $b;
            },
            'version' => function($b, SearchSettings $settings) {
                $settings->printversion = $b;
            }
        ];
        $this->set_options_from_xml();
    }

    private function set_options_from_xml() {
        $searchoptionspath = FileUtil::expand_user_home_path(Config::SEARCHOPTIONSPATH);
        if (file_exists($searchoptionspath)) {
            $searchoptions = simplexml_load_file($searchoptionspath);
            foreach ($searchoptions->searchoption as $searchoption) {
                $short = sprintf($searchoption['short']);
                $long = sprintf($searchoption['long']);
                $desc = trim($searchoption);
                $func = NULL;
                if (array_key_exists($long, $this->arg_action_map)) {
                    $func = $this->arg_action_map[$long];
                    if ($short) {
                        $this->arg_action_map[$short] = $func;
                    }
                }
                else if (array_key_exists($long, $this->bool_flag_action_map)) {
                    $func = $this->bool_flag_action_map[$long];
                    if ($short) {
                        $this->flag_action_map[$short] = $func;
                    }
                }
                $option = new SearchOption($short, $long, $desc, $func);
                $this->options[] = $option;
            }
            usort($this->options, 'cmp_searchoptions');

        } else {
            throw new Exception('File not found: ' . $searchoptionspath);
        }
    }

    private function settings_from_file($filepath, SearchSettings $settings) {
        if (!file_exists($filepath)) {
            throw new SearchException('Settings file not found');
        }
        $json = file_get_contents($filepath);
        $this->settings_from_json($json, $settings);
    }

    public function settings_from_json($json, SearchSettings $settings) {
        $json_obj = json_decode($json, true);
        foreach (array_keys($json_obj) as $k) {
            if (array_key_exists($k, $this->arg_action_map)) {
                $this->arg_action_map[$k]($json_obj[$k], $settings);
            } else if (array_key_exists($k, $this->bool_flag_action_map)) {
                $this->bool_flag_action_map[$k]($json_obj[$k], $settings);
            } else if ($k == 'startpath') {
                $settings->startpath = $json_obj[$k];
            } else {
                throw new SearchException("Invalid option: $k");
            }
        }
    }

    public function settings_from_args($args) {
        $settings = new SearchSettings();
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg{0} == '-') {
                while($arg{0} == '-') {
                    $arg = substr($arg, 1);
                }
                if (array_key_exists($arg, $this->arg_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        $this->arg_action_map[$arg]($val, $settings);
                    } else {
                        throw new SearchException("Missing value for $arg");
                    }
                } else if (array_key_exists($arg, $this->bool_flag_action_map)) {
                    $this->bool_flag_action_map[$arg](true, $settings);
                } else {
                    throw new SearchException("Invalid option: $arg");
                }
            } else {
                $settings->startpath = $arg;
            }
        }
        return $settings;
    }

    public function usage() {
        echo $this->get_usage_string() . "\n";
    }

    private function get_usage_string() {
        $usage = "Usage:\n phpsearch.php [options] -s <searchpattern>";
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
}

function cmp_searchoptions($o1, $o2) {
    return strcmp($o1->sortarg, $o2->sortarg);
}

?>
