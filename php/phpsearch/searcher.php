<?php

require_once __DIR__ . '/autoload.php';
require_once __DIR__ . '/common.php';

class Searcher {
    function __construct(SearchSettings $settings) {
        $this->settings = $settings;
        $this->filetypes = new FileTypes();
        $this->results = array();
        $this->timers = array();
        $this->totalElapsed = 0;
        $this->validate_settings();
    }

    private function validate_settings() {
        if (strlen($this->settings->startpath) == 0) {
            throw new SearchException('Startpath not defined');
        }
        if (!file_exists($this->settings->startpath)) {
            throw new SearchException('Startpath not found');
        }
        if (count($this->settings->searchpatterns) == 0) {
            throw new SearchException('No search patterns defined');
        }
    }

    private function matches_any_pattern($s, $patterns) {
        foreach ($patterns as $pattern) {
            $pattern = '/' . $pattern . '/';
            if (preg_match_all($pattern, $s))
                return true;
        }
        return false;
    }

    private function any_matches_any_pattern($slist, $patterns) {
        foreach ($slist as $s) {
            if ($this->matches_any_pattern($s, $patterns))
                return true;
        }
        return false;
    }

    public function is_search_dir($d) {
        if (FileUtil::is_dot_dir($d))
            return true;
        if ($this->settings->excludehidden && FileUtil::is_hidden($d))
            return false;
        $path_elems = FileUtil::split_path($d);
        if ($this->settings->in_dirpatterns &&
            !$this->any_matches_any_pattern($path_elems, $this->settings->in_dirpatterns))
            return false;
        if ($this->settings->out_dirpatterns &&
            $this->any_matches_any_pattern($path_elems, $this->settings->out_dirpatterns))
            return false;
        return true;
    }

    public function is_search_file($f) {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_extensions && !in_array($ext, $this->settings->in_extensions))
            return false;
        if ($this->settings->out_extensions && in_array($ext, $this->settings->out_extensions))
            return false;
        if ($this->settings->in_filepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_filepatterns))
            return false;
        if ($this->settings->out_filepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_filepatterns))
            return false;
        return true;
    }

    public function is_archive_search_file($f) {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_archiveextensions &&
            !in_array($ext, $this->settings->in_archiveextensions))
            return false;
        if ($this->settings->out_archiveextensions &&
            in_array($ext, $this->settings->out_archiveextensions))
            return false;
        if ($this->settings->in_archivefilepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_archivefilepatterns))
            return false;
        if ($this->settings->out_archivefilepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_archivefilepatterns))
            return false;
        return true;
    }

    private function get_non_dot_dirs($d) {
        $filter_non_dot_dirs = function($f) use ($d) {
            return (is_dir(FileUtil::join_path($d, $f)) && !FileUtil::is_dot_dir($f));
        };
        return array_filter(scandir($d), $filter_non_dot_dirs);
    }

    private function rec_get_search_dirs($d) {
        $join_path = function($f) use ($d) {
            return FileUtil::join_path($d, $f);
        };
        $filter_search_dirs = function($f) use ($join_path) {
            return $this->is_search_dir($join_path($f));
        };
        $searchdirs = array_filter($this->get_non_dot_dirs($d), $filter_search_dirs);
        $searchdirs = array_map($join_path, $searchdirs);
        foreach ($searchdirs as $searchdir) {
            $searchdirs = array_merge($searchdirs, $this->rec_get_search_dirs($searchdir));
        }
        return $searchdirs;
    }

    private function get_search_dirs() {
        $searchdirs = array();
        if (is_dir($this->settings->startpath)) {
            if ($this->is_search_dir($this->settings->startpath)) {
                $searchdirs[] = $this->settings->startpath;
                if ($this->settings->recursive) {
                    $searchdirs = array_merge($searchdirs,
                        $this->rec_get_search_dirs($this->settings->startpath));
                }
            } else {
                throw new SearchException("Startpath does not match search settings");
            }
        } else if (is_file($this->settings->startpath)) {
            if ($this->filter_file($this->settings->startpath))
                $searchdirs[] = dirname($this->settings->startpath);
            else
                throw new SearchException("Startpath does not match search settings");
        }
        sort($searchdirs);
        return $searchdirs;
    }

    public function filter_file($f) {
        if ($this->settings->excludehidden && FileUtil::is_hidden($f))
            return false;
        if ($this->filetypes->is_archive($f)) {
            return $this->settings->searcharchives && $this->is_archive_search_file($f);
        }
        return !$this->settings->archivesonly && $this->is_search_file($f);
    }

    private function get_search_files_for_directory($d) {
        $filter_search_files = function($f) use ($d) {
            $p = FileUtil::join_path($d, $f);
            return is_file($p) && $this->filter_file($f);
        };
        $searchfiles = array_filter(scandir($d), $filter_search_files);
        $join_path = function($f) use ($d) {
            return FileUtil::join_path($d, $f);
        };
        $searchfiles = array_map($join_path, $searchfiles);
        return $searchfiles;
    }

    private function get_search_files($searchdirs) {
        $searchfiles = array();
        if (is_dir($this->settings->startpath)) {
            foreach ($searchdirs as $searchdir) {
                $searchfiles = array_merge($searchfiles, 
                    $this->get_search_files_for_directory($searchdir));
            }
        } else if (is_file($this->settings->startpath)) {
            if ($this->filter_file($this->settings->startpath))
                $searchfiles[] = $this->settings->startpath;
            else
                throw new SearchException("Startpath does not match search settings");
        }
        return $searchfiles;
    }

    private function add_timer($name, $action) {
        $this->timers["$name:$action"] = microtime(true);
    }

    private function start_timer($name) {
        $this->add_timer($name, 'start');
    }

    private function stop_timer($name) {
        $this->add_timer($name, 'stop');
        $this->add_elapsed($name);
    }

    private function get_elapsed($name) {
        $start = $this->timers["$name:start"];
        $stop = $this->timers["$name:stop"];
        return $stop - $start;
    }

    private function add_elapsed($name) {
        $this->totalElapsed += $this->get_elapsed($name);
    }

    private function print_elapsed($name) {
        log_msg(sprintf("\nElapsed time for %s: %d", $name, $this->get_elapsed($name)));
    }

    private function print_total_elapsed($name) {
        log_msg(sprintf("\nTotal elapsed time: %d", $this->totalElapsed));
    }

    public function search() {
        if ($this->settings->dotiming)
            $this->start_timer('get_search_dirs');
        $searchdirs = $this->get_search_dirs();
        if ($this->settings->dotiming) {
            $this->stop_timer('get_search_dirs');
            if ($this->settings->printresults)
                $this->print_elapsed('get_search_dirs');
        }
        if ($this->settings->verbose) {
            log_msg(sprintf("\nDirectories to be searched (%d):", count($searchdirs)));
            foreach ($searchdirs as $d) {
                log_msg($d);
            }
        }

        if ($this->settings->dotiming)
            $this->start_timer('get_search_files');
        $searchfiles = $this->get_search_files($searchdirs);
        if ($this->settings->dotiming) {
            $this->stop_timer('get_search_files');
            if ($this->settings->printresults)
                $this->print_elapsed('get_search_files');
        }
        if ($this->settings->verbose) {
            log_msg(sprintf("\nFiles to be searched (%d):", count($searchfiles)));
            foreach ($searchfiles as $f) {
                log_msg($f);
            }
        }

        // search the files
        if ($this->settings->dotiming)
            $this->start_timer('search_files');
        foreach ($searchfiles as $f) {
            $this->search_file($f);
        }
        if ($this->settings->dotiming) {
            $this->stop_timer('search_files');
            if ($this->settings->printresults)
                $this->print_elapsed('search_files');
        }
    }

    public function search_file($f) {
        $type = $this->filetypes->get_filetype($f);
        if ($type == FileType::Text) {
            $this->search_text_file($f);
        } else if ($type == FileType::Binary) {
            $this->search_binary_file($f);
        }
    }

    private function search_text_file($f) {
        if ($this->settings->debug)
            log_msg("Searching text file $f");
        if ($this->settings->multilinesearch) {
            $this->search_text_file_contents($f);
        } else {
            $this->search_text_file_lines($f);
        }
    }

    private function get_new_line_indices($s) {
        $indices = array();
        $i = 0;
        while ($i < strlen($s)) {
            if ($s{$i} == "\n")
                $indices[] = $i;
            $i++;
        }
        return $indices;
    }

    private function search_text_file_contents($f) {
        $contents = file_get_contents($f);
        $results = $this->search_multiline_string($contents);
        foreach ($results as $r) {
            $fr = new SearchResult(
                $r->pattern,
                $f,
                $r->linenum,
                $r->match_start_index,
                $r->match_end_index,
                $r->line,
                $r->lines_before,
                $r->lines_after);
            $this->add_search_result($fr);
        }
    }

    private function get_before_indices($indices, $after_index) {
        $less_or_equal = function($i) use ($after_index) { return $i <= $after_index; };
        return array_filter($indices, $less_or_equal);
    }

    private function get_after_indices($indices, $before_index) {
        $greater_than = function($i) use ($before_index) { return $i > $before_index; };
        return array_filter($indices, $greater_than);
    }

    private function get_lines_before($s, $before_start_indices, $start_line_indices,
        $end_line_indices) {
        $lines_before = array();
        $i = 0;
        while($i < count($before_start_indices) && $i < $this->settings->linesbefore) {
            $start_index = $before_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_before[] = $line;
            $i++;
        }
        return $lines_before;
    }

    private function get_lines_after($s, $after_start_indices, $start_line_indices,
        $end_line_indices) {
        $lines_after = array();
        $i = 0;
        while($i < count($after_start_indices) && $i < $this->settings->linesafter) {
            $start_index = $after_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_after[] = $line;
            $i++;
        }
        return $lines_after;
    }

    public function search_multiline_string($s) {
        $results = array();
        $new_line_indices = $this->get_new_line_indices($s);
        $plus_one = function($i) { return $i + 1; };
        $start_line_indices = array_merge(array(0), array_map($plus_one, $new_line_indices));
        $end_line_indices = array_merge($new_line_indices, array(strlen($s) - 1));
        foreach ($this->settings->searchpatterns as $pattern) {
            $p = '/' . $pattern . '/';
            $has_matches = preg_match_all($p, $s, $matches, PREG_OFFSET_CAPTURE);
            if ($has_matches) {
                foreach ($matches[0] as $m) {
                    $start_index = $m[1];
                    $end_index = $start_index + strlen($m[0]);
                    $before_start_indices = $this->get_before_indices($start_line_indices, $start_index);
                    $m_line_start_index = 0;
                    $before_line_count = 0;
                    if ($before_start_indices) {
                        $before_line_count = count($before_start_indices) - 1;
                        $m_line_start_index = $before_start_indices[$before_line_count];
                    }
                    $m_line_end_index = $end_line_indices[array_search($m_line_start_index, $start_line_indices)];
                    $line = substr($s, $m_line_start_index, $m_line_end_index - $m_line_start_index);
                    $lines_before = [];
                    if ($this->settings->linesbefore) {
                        $lines_before = $this->get_lines_before(
                            $s,
                            array_slice($before_start_indices,
                                ($this->settings->linesbefore + 1) * -1, -1),
                            $start_line_indices,
                            $end_line_indices);
                    }
                    $lines_after = [];
                    if ($this->settings->linesafter) {
                        $after_start_indices = $this->get_after_indices($start_line_indices, $start_index);
                        $lines_after = $this->get_lines_after(
                            $s,
                            array_slice($after_start_indices, 0,
                                $this->settings->linesafter),
                            $start_line_indices,
                            $end_line_indices);
                    }
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after)))
                        continue;
                    $r = new SearchResult(
                        $pattern,
                        '',
                        $before_line_count + 1,
                        $start_index - $m_line_start_index + 1,
                        $end_index - $m_line_start_index + 1,
                        $line,
                        $lines_before,
                        $lines_after);
                    $results[] = $r;
                    if ($this->settings->firstmatch)
                        break;
                }
            }
        }
        return $results;
    }

    private function search_text_file_lines($f) {
        $lines = file($f);
        $results = $this->search_lines($lines);
        foreach ($results as $r) {
            $fr = new SearchResult(
                $r->pattern,
                $f,
                $r->linenum,
                $r->match_start_index,
                $r->match_end_index,
                $r->line,
                $r->lines_before,
                $r->lines_after);
            $this->add_search_result($fr);
        }
    }

    private function lines_match($lines, $in_patterns, $out_patterns) {
        if ((!$in_patterns || $this->any_matches_any_pattern($lines, $in_patterns))
            &&
            (!$out_patterns || !$this->any_matches_any_pattern($lines, $out_patterns)))
            return true;
        return false;
    }

    private function lines_before_match($lines_before) {
        return $this->lines_match($lines_before, $this->settings->in_linesbeforepatterns,
            $this->settings->out_linesbeforepatterns);
    }

    private function lines_after_match($lines_after) {
        return $this->lines_match($lines_after, $this->settings->in_linesafterpatterns,
            $this->settings->out_linesafterpatterns);
    }

    public function search_lines($lines) {
        $linenum = 0;
        $line = '';
        $lines_before = array();
        $lines_after = array();
        $pattern_match_map = array();
        $results = array();
        while (true) {
            if ($lines_after)
                $line = array_shift($lines_after);
            else
                $line = array_shift($lines);
            if ($line === NULL)
                break;
            $linenum++;
            if ($this->settings->linesafter) {
                while(count($lines_after) < $this->settings->linesafter) {
                    $line_after = array_shift($lines);
                    if ($line_after == NULL)
                        break;
                    else
                        $lines_after[] = $line_after;
                }
            }
            foreach ($this->settings->searchpatterns as $pattern) {
                if ($this->settings->firstmatch && array_key_exists($pattern, $pattern_match_map))
                    continue;
                $p = '/' . $pattern . '/';
                $has_matches = preg_match_all($p, $line, $matches, PREG_OFFSET_CAPTURE);
                if ($has_matches) {
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after)))
                        continue;
                    foreach ($matches[0] as $m) {
                        $start_index = $m[1] + 1;
                        $end_index = $start_index + strlen($m[0]);
                        $r = new SearchResult(
                            $pattern,
                            '',
                            $linenum,
                            $start_index,
                            $end_index,
                            $line,
                            $lines_before,
                            $lines_after);
                        $results[] = $r;
                        $pattern_match_map[$pattern] = 1;
                    }
                }
            }
            if ($this->settings->linesbefore) {
                if (count($lines_before) == $this->settings->linesbefore)
                    array_shift($lines_before);
                if (count($lines_before) < $this->settings->linesbefore)
                    $lines_before[] = $line;
            }
        }
        return $results;
    }

    private function search_binary_file($f) {
        if ($this->settings->debug)
            log_msg("Searching binary file $f");
        $contents = file_get_contents($f);
        foreach ($this->settings->searchpatterns as $pattern) {
            $p = '/' . $pattern . '/';
            if (preg_match_all($p, $contents)) {
                $r = new SearchResult($pattern, $f, 0, 0, 0, '', [], []);
                $this->add_search_result($r);
            }
        }
    }

    private function add_search_result(SearchResult $r) {
        $this->results[] = $r;
    }

    public function printresults() {
        log_msg(sprintf("\nSearch results (%d):", count($this->results)));
        foreach ($this->results as $r) {
            log_msg($r);
        }
    }

    public function get_matching_dirs() {
        $dirs = array();
        foreach($this->results as $r) {
            $d = dirname($r->file);
            if (!in_array($d, $dirs))
                $dirs[] = $d;
        }
        sort($dirs);
        return $dirs;
    }

    public function print_matching_dirs() {
        $dirs = $this->get_matching_dirs();
        log_msg(sprintf("\nDirectories with matches (%d):", count($dirs)));
        foreach ($dirs as $d) {
            log_msg($d);
        }
    }

    public function get_matching_files() {
        $files = array();
        foreach($this->results as $r) {
            $f = $r->file;
            if (!in_array($f, $files))
                $files[] = $f;
        }
        sort($files);
        return $files;
    }

    public function print_matching_files() {
        $files = $this->get_matching_files();
        log_msg(sprintf("\nFiles with matches (%d):", count($files)));
        foreach ($files as $f) {
            log_msg($f);
        }
    }

    public function get_matching_lines() {
        $lines = array();
        foreach($this->results as $r) {
            $l = trim($r->line);
            if (!$this->settings->uniquelines || !in_array($l, $lines))
                $lines[] = $l;
        }
        usort($lines, 'cmp_ignorecase');
        return $lines;
    }

    public function print_matching_lines() {
        $lines = $this->get_matching_lines();
        $msg = "\nLines with matches (%d):";
        if ($this->settings->uniquelines)
            $msg = "\nUnique lines with matches (%d):";
        log_msg(sprintf($msg, count($lines)));
        foreach ($lines as $l) {
            log_msg($l);
        }
    }
}

function cmp_ignorecase($s1, $s2) {
    return strcmp(strtoupper($s1), strtoupper($s2));
}


?>
