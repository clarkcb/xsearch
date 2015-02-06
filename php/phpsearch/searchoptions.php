<?php

require_once __DIR__ . '/autoload.php';

class SearchOptions {
    const SEARCHOPTIONSPATH = '~/src/git/xsearch/shared/searchoptions.xml';

    function __construct() {
        $this->options = array();

        $this->arg_action_map = [
            'in-archiveext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_archiveextensions);
            },
            'in-archivefilepattern' => function($s, SearchSettings $settings) {
                $settings->in_archivefilepatterns[] = $s;
            },
            'in-dirpattern' => function($s, SearchSettings $settings) {
                $settings->in_dirpatterns[] = $s;
            },
            'in-ext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->in_extensions);
            },
            'in-filepattern' => function($s, SearchSettings $settings) {
                $settings->in_filepatterns[] = $s;
            },
            'in-linesafterpattern' => function($s, SearchSettings $settings) {
                $settings->in_linesafterpatterns[] = $s;
            },
            'in-linesbeforepattern' => function($s, SearchSettings $settings) {
                $settings->in_linesbeforepatterns[] = $s;
            },
            'linesafter' => function($s, SearchSettings $settings) {
                $settings->linesafter = intval($s);
            },
            'linesaftertopattern' => function($s, SearchSettings $settings) {
                $settings->linesaftertopatterns[] = $s;
            },
            'linesafteruntilpattern' => function($s, SearchSettings $settings) {
                $settings->linesafteruntilpatterns[] = $s;
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
                $settings->out_archivefilepatterns[] = $s;
            },
            'out-dirpattern' => function($s, SearchSettings $settings) {
                $settings->out_dirpatterns[] = $s;
            },
            'out-ext' => function($s, SearchSettings $settings) {
                $settings->add_exts($s, $settings->out_extensions);
            },
            'out-filepattern' => function($s, SearchSettings $settings) {
                $settings->out_filepatterns[] = $s;
            },
            'out-linesafterpattern' => function($s, SearchSettings $settings) {
                $settings->out_linesafterpatterns[] = $s;
            },
            'out-linesbeforepattern' => function($s, SearchSettings $settings) {
                $settings->out_linesbeforepatterns[] = $s;
            },
            'search' => function($s, SearchSettings $settings) {
                $settings->searchpatterns[] = $s;
            }
        ];

        $this->flag_action_map = [
            'allmatches' => function(SearchSettings $settings) {
                $settings->firstmatch = false;
            },
            'archivesonly' => function(SearchSettings $settings) {
                $settings->archivesonly = true;
                $settings->searcharchives = true;
            },
            'debug' => function(SearchSettings $settings) {
                $settings->debug = true;
                $settings->verbose = true;
            },
            'dotiming' => function(SearchSettings $settings) {
                $settings->dotiming = true;
            },
            'excludehidden' => function(SearchSettings $settings) {
                $settings->excludehidden = true;
            },
            'firstmatch' => function(SearchSettings $settings) {
                $settings->firstmatch = true;
            },
            'help' => function(SearchSettings $settings) {
                $settings->printusage = true;
            },
            'includehidden' => function(SearchSettings $settings) {
                $settings->excludehidden = false;
            },
            'listdirs' => function(SearchSettings $settings) {
                $settings->listdirs = true;
            },
            'listfiles' => function(SearchSettings $settings) {
                $settings->listfiles = true;
            },
            'listlines' => function(SearchSettings $settings) {
                $settings->listlines = true;
            },
            'multilinesearch' => function(SearchSettings $settings) {
                $settings->multilinesearch = true;
            },
            'noprintmatches' => function(SearchSettings $settings) {
                $settings->printresults = false;
            },
            'norecursive' => function(SearchSettings $settings) {
                $settings->recursive = false;
            },
            'nosearcharchives' => function(SearchSettings $settings) {
                $settings->searcharchives = false;
            },
            'printmatches' => function(SearchSettings $settings) {
                $settings->printresults = true;
            },
            'recursive' => function(SearchSettings $settings) {
                $settings->recursive = true;
            },
            'searcharchives' => function(SearchSettings $settings) {
                $settings->searcharchives = true;
            },
            'uniquelines' => function(SearchSettings $settings) {
                $settings->uniquelines = true;
            },
            'verbose' => function(SearchSettings $settings) {
                $settings->verbose = true;
            },
            'version' => function(SearchSettings $settings) {
                $settings->printversion = true;
            }
        ];
        $this->set_options_from_xml();
    }

    private function set_options_from_xml() {
        $searchoptionspath = FileUtil::expand_user_home_path(SearchOptions::SEARCHOPTIONSPATH);
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
                else if (array_key_exists($long, $this->flag_action_map)) {
                    $func = $this->flag_action_map[$long];
                    if ($short) {
                        $this->flag_action_map[$short] = $func;
                    }
                }
                $option = new SearchOption($short, $long, $desc, $func);
                $this->options[] = $option;
            }

        } else {
            throw new Exception('File not found: ' . $searchoptionspath);
        }
    }

    public function settings_from_args($args) {
        $settings = new SearchSettings();
        $i = 0;
        while ($i < count($args)) {
            $arg = $args[$i];
            if ($arg{0} == '-') {
                while($arg{0} == '-') {
                    $arg = substr($arg, 1);
                }
                if (array_key_exists($arg, $this->arg_action_map)) {
                    $val = $args[$i+1];
                    $this->arg_action_map[$arg]($val, $settings);
                    $i += 2;
                } else if (array_key_exists($arg, $this->flag_action_map)) {
                    $this->flag_action_map[$arg]($settings);
                    $i++;
                } else {
                    throw new SearchException("Unknown option: $arg");
                }
            } else {
                $settings->startpath = $arg;
                $i++;
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

?>
