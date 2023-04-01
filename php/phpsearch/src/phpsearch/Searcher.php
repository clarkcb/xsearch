<?php declare(strict_types=1);

namespace phpsearch;

//require_once __DIR__ . '/../autoload.php';
//require_once __DIR__ . '/common.php';

/**
 * Class Searcher
 *
 * @property SearchSettings $settings
 * @property FileTypes $filetypes
 */
class Searcher
{
    private readonly SearchSettings $settings;
    private readonly FileTypes $filetypes;
    private $results;

    /**
     * @throws SearchException
     */
    public function __construct(SearchSettings $settings)
    {
        $this->settings = $settings;
        $this->filetypes = new FileTypes();
        $this->results = array();
        $this->validate_settings();
    }

    /**
     * @throws SearchException
     */
    private function validate_settings(): void
    {
        if (!$this->settings->paths) {
            throw new SearchException('Startpath not defined');
        }
        foreach ($this->settings->paths as $p) {
            if (!file_exists($p)) {
                throw new SearchException('Startpath not found');
            }
            if (!is_readable($p)) {
                throw new SearchException('Startpath not readable');
            }
        }
        if (count($this->settings->searchpatterns) == 0) {
            throw new SearchException('No search patterns defined');
        }
        if ($this->settings->linesafter < 0) {
            throw new SearchException('Invalid linesafter');
        }
        if ($this->settings->linesbefore < 0) {
            throw new SearchException('Invalid linesbefore');
        }
        if ($this->settings->maxlinelength < 0) {
            throw new SearchException('Invalid maxlinelength');
        }
    }

    private function matches_any_pattern(string $s, array $patterns): bool
    {
        foreach ($patterns as $pattern) {
            $pattern = '/' . $pattern . '/';
            if (preg_match_all($pattern, $s)) {
                return true;
            }
        }
        return false;
    }

    private function any_matches_any_pattern(array $slist, array $patterns): bool
    {
        foreach ($slist as $s) {
            if ($this->matches_any_pattern($s, $patterns)) {
                return true;
            }
        }
        return false;
    }

    public function is_search_dir(string $d): bool
    {
        if (FileUtil::is_dot_dir($d)) {
            return true;
        }
        if ($this->settings->excludehidden && FileUtil::is_hidden($d)) {
            return false;
        }
        $path_elems = FileUtil::split_path($d);
        if ($this->settings->in_dirpatterns &&
            !$this->any_matches_any_pattern($path_elems, $this->settings->in_dirpatterns)) {
            return false;
        }
        if ($this->settings->out_dirpatterns &&
            $this->any_matches_any_pattern($path_elems, $this->settings->out_dirpatterns)) {
            return false;
        }
        return true;
    }

    public function is_search_file(string $f): bool
    {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_extensions && !in_array($ext, $this->settings->in_extensions)) {
            return false;
        }
        if ($this->settings->out_extensions && in_array($ext, $this->settings->out_extensions)) {
            return false;
        }
        if ($this->settings->in_filepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_filepatterns)) {
            return false;
        }
        if ($this->settings->out_filepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_filepatterns)) {
            return false;
        }
        $type = $this->filetypes->get_filetype($f);
        if ($this->settings->in_filetypes && !in_array($type, $this->settings->in_filetypes)) {
            return false;
        }
        if ($this->settings->out_filetypes && in_array($type, $this->settings->out_filetypes)) {
            return false;
        }
        return true;
    }

    public function is_archive_search_file(string $f): bool
    {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_archiveextensions &&
            !in_array($ext, $this->settings->in_archiveextensions)) {
            return false;
        }
        if ($this->settings->out_archiveextensions &&
            in_array($ext, $this->settings->out_archiveextensions)) {
            return false;
        }
        if ($this->settings->in_archivefilepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_archivefilepatterns)) {
            return false;
        }
        if ($this->settings->out_archivefilepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_archivefilepatterns)) {
            return false;
        }
        return true;
    }

    private function get_non_dot_dirs(string $d): array
    {
        $filter_non_dot_dirs = function ($f) use ($d) {
            return (is_dir(FileUtil::join_path($d, $f)) && !FileUtil::is_dot_dir($f));
        };
        return array_filter(scandir($d), $filter_non_dot_dirs);
    }

    private function get_dir_search_dirs(string $d): array
    {
        $filter_dirs = function ($f) use ($d) {
            return is_dir(FileUtil::join_path($d, $f)) && $this->is_search_dir($f);
        };
        $join_path = function ($f) use ($d) {
            return FileUtil::join_path($d, $f);
        };
        $searchdirs = array_filter($this->get_non_dot_dirs($d), $filter_dirs);
        return array_map($join_path, $searchdirs);
    }

    private function get_dir_search_files(string $d): array
    {
        $filter_files = function ($f) use ($d) {
            return is_file(FileUtil::join_path($d, $f)) && $this->filter_file($f);
        };
        $to_search_file = function ($f) use ($d) {
            return new SearchFile($d, $f, $this->filetypes->get_filetype($f));
        };
        $searchfiles = array_filter(scandir($d), $filter_files);
        return array_map($to_search_file, $searchfiles);
    }

    private function rec_get_search_files(string $d): array
    {
        $searchdirs = $this->get_dir_search_dirs($d);
        $searchfiles = $this->get_dir_search_files($d);
        foreach ($searchdirs as $searchdir) {
            $searchfiles = array_merge($searchfiles, $this->rec_get_search_files($searchdir));
        }
        return $searchfiles;
    }

    /**
     * @throws SearchException
     */
    private function get_search_files(): array
    {
        $searchfiles = array();
        foreach ($this->settings->paths as $p) {
            if (is_dir($p)) {
                if ($this->is_search_dir($p)) {
                    if ($this->settings->recursive) {
                        $searchfiles = array_merge($searchfiles, $this->rec_get_search_files($p));
                    } else {
                        $searchfiles = array_merge($searchfiles, $this->get_dir_search_files($p));
                    }
                } else {
                    throw new SearchException("Startpath does not match search settings");
                }
            } elseif (is_file($p)) {
                if ($this->filter_file($p)) {
                    $searchfiles[] = $p;
                } else {
                    throw new SearchException("Startpath does not match search settings");
                }
            }
        }
        sort($searchfiles);
        return $searchfiles;
    }

    public function filter_file(string $f): bool
    {
        if ($this->settings->excludehidden && FileUtil::is_hidden($f)) {
            return false;
        }
        if ($this->filetypes->is_archive($f)) {
            return $this->settings->searcharchives && $this->is_archive_search_file($f);
        }
        return !$this->settings->archivesonly && $this->is_search_file($f);
    }

    /**
     * @throws SearchException
     */
    public function search()
    {
        $searchfiles = $this->get_search_files();
        if ($this->settings->verbose) {
            $get_path = function ($sf) {
                return $sf->path;
            };
            $searchdirs = array_unique(array_map($get_path, $searchfiles));
            Logger::log_msg(sprintf("\nDirectories to be searched (%d):", count($searchdirs)));
            foreach ($searchdirs as $d) {
                Logger::log_msg($d);
            }
            Logger::log_msg(sprintf("\n\nFiles to be searched (%d):", count($searchfiles)));
            foreach ($searchfiles as $f) {
                Logger::log_msg((string)$f);
            }
        }

        // search the files
        // TODO: use Fiber?
        foreach ($searchfiles as $f) {
            $this->search_file($f);
        }
    }

    public function search_file(SearchFile $f)
    {
        if ($f->filetype == FileType::Text || $f->filetype == FileType::Code ||
            $f->filetype == FileType::Xml) {
            $this->search_text_file($f);
        } elseif ($f->filetype == FileType::Binary) {
            $this->search_binary_file($f);
        }
    }

    private function search_text_file(SearchFile $f)
    {
        if ($this->settings->debug) {
            Logger::log_msg("Searching text file $f");
        }
        if ($this->settings->multilinesearch) {
            $this->search_text_file_contents($f);
        } else {
            $this->search_text_file_lines($f);
        }
    }

    private function get_new_line_indices(string $s): array
    {
        $indices = array();
        $i = 0;
        while ($i < strlen($s)) {
            if ($s[$i] == "\n") {
                $indices[] = $i;
            }
            $i++;
        }
        return $indices;
    }

    private function search_text_file_contents(SearchFile $f)
    {
        $contents = file_get_contents($f->filepath());
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
                $r->lines_after
            );
            $this->add_search_result($fr);
        }
    }

    private function get_before_indices(array $indices, int $after_index): array
    {
        $less_or_equal = function ($i) use ($after_index) {
            return $i <= $after_index;
        };
        return array_filter($indices, $less_or_equal);
    }

    private function get_after_indices(array $indices, int $before_index): array
    {
        $greater_than = function ($i) use ($before_index) {
            return $i > $before_index;
        };
        return array_filter($indices, $greater_than);
    }

    private function get_lines_before(
        string $s,
        array $before_start_indices,
        array $start_line_indices,
        array $end_line_indices
    ): array {
        $lines_before = array();
        $i = 0;
        while ($i < count($before_start_indices) && $i < $this->settings->linesbefore) {
            $start_index = $before_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_before[] = $line;
            $i++;
        }
        return $lines_before;
    }

    private function get_lines_after(
        string $s,
        array $after_start_indices,
        array $start_line_indices,
        array $end_line_indices
    ): array {
        $lines_after = array();
        $i = 0;
        while ($i < count($after_start_indices) && $i < $this->settings->linesafter) {
            $start_index = $after_start_indices[$i];
            $end_index = $end_line_indices[array_search($start_index, $start_line_indices)];
            $line = substr($s, $start_index, $end_index - $start_index);
            $lines_after[] = $line;
            $i++;
        }
        return $lines_after;
    }

    public function search_multiline_string(string $s): array
    {
        $results = array();
        $new_line_indices = $this->get_new_line_indices($s);
        $plus_one = function ($i) {
            return $i + 1;
        };
        $start_line_indices = array_merge(array(0), array_map($plus_one, $new_line_indices));
        $end_line_indices = array_merge($new_line_indices, array(strlen($s) - 1));
        foreach ($this->settings->searchpatterns as $pattern) {
            $p = '/' . $pattern . '/';
            if (preg_match_all($p, $s, $matches, PREG_OFFSET_CAPTURE)) {
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
                            array_slice(
                                $before_start_indices,
                                ($this->settings->linesbefore + 1) * -1,
                                -1
                            ),
                            $start_line_indices,
                            $end_line_indices
                        );
                    }
                    $lines_after = [];
                    if ($this->settings->linesafter) {
                        $after_start_indices = $this->get_after_indices($start_line_indices, $start_index);
                        $lines_after = $this->get_lines_after(
                            $s,
                            array_slice(
                                $after_start_indices,
                                0,
                                $this->settings->linesafter
                            ),
                            $start_line_indices,
                            $end_line_indices
                        );
                    }
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after))) {
                        continue;
                    }
                    $r = new SearchResult(
                        $pattern,
                        null,
                        $before_line_count + 1,
                        $start_index - $m_line_start_index + 1,
                        $end_index - $m_line_start_index + 1,
                        $line,
                        $lines_before,
                        $lines_after
                    );
                    $results[] = $r;
                    if ($this->settings->firstmatch) {
                        break;
                    }
                }
            }
        }
        return $results;
    }

    private function search_text_file_lines(SearchFile $f)
    {
        $lines = file($f->filepath());
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
                $r->lines_after
            );
            $this->add_search_result($fr);
        }
    }

    private function lines_match(array $lines, array $in_patterns, array $out_patterns): bool
    {
        if ((!$in_patterns || $this->any_matches_any_pattern($lines, $in_patterns))
            &&
            (!$out_patterns || !$this->any_matches_any_pattern($lines, $out_patterns))) {
            return true;
        }
        return false;
    }

    private function lines_before_match(array $lines_before): bool
    {
        return $this->lines_match(
            $lines_before,
            $this->settings->in_linesbeforepatterns,
            $this->settings->out_linesbeforepatterns
        );
    }

    private function lines_after_match(array $lines_after): bool
    {
        return $this->lines_match(
            $lines_after,
            $this->settings->in_linesafterpatterns,
            $this->settings->out_linesafterpatterns
        );
    }

    public function search_lines(array $lines): array
    {
        $linenum = 0;
        $line = '';
        $lines_before = array();
        $lines_after = array();
        $pattern_match_map = array();
        $results = array();
        while (true) {
            if ($lines_after) {
                $line = array_shift($lines_after);
            } else {
                $line = array_shift($lines);
            }
            if ($line === null) {
                break;
            }
            $linenum++;
            if ($this->settings->linesafter) {
                while (count($lines_after) < $this->settings->linesafter) {
                    $line_after = array_shift($lines);
                    if ($line_after == null) {
                        break;
                    } else {
                        $lines_after[] = $line_after;
                    }
                }
            }
            foreach ($this->settings->searchpatterns as $pattern) {
                if ($this->settings->firstmatch && array_key_exists($pattern, $pattern_match_map)) {
                    continue;
                }
                $p = '/' . $pattern . '/';
                if (preg_match_all($p, $line, $matches, PREG_OFFSET_CAPTURE)) {
                    if (($lines_before && !$this->lines_before_match($lines_before))
                        ||
                        ($lines_after && !$this->lines_after_match($lines_after))) {
                        continue;
                    }
                    foreach ($matches[0] as $m) {
                        $start_index = $m[1] + 1;
                        $end_index = $start_index + strlen($m[0]);
                        $r = new SearchResult(
                            $pattern,
                            null,
                            $linenum,
                            $start_index,
                            $end_index,
                            $line,
                            $lines_before,
                            $lines_after
                        );
                        $results[] = $r;
                        $pattern_match_map[$pattern] = 1;
                    }
                }
            }
            if ($this->settings->linesbefore) {
                if (count($lines_before) == $this->settings->linesbefore) {
                    array_shift($lines_before);
                }
                if (count($lines_before) < $this->settings->linesbefore) {
                    $lines_before[] = $line;
                }
            }
        }
        return $results;
    }

    private function search_binary_file(SearchFile $f)
    {
        if ($this->settings->debug) {
            Logger::log_msg("Searching binary file $f");
        }
        $contents = file_get_contents($f->filepath());
        foreach ($this->settings->searchpatterns as $pattern) {
            $p = '/' . $pattern . '/';
            if (preg_match_all($p, $contents, $matches, PREG_OFFSET_CAPTURE)) {
                foreach ($matches[0] as $m) {
                    $start_index = $m[1] + 1;
                    $end_index = $start_index + strlen($m[0]);
                    $r = new SearchResult(
                        $pattern,
                        $f,
                        0,
                        $start_index,
                        $end_index,
                        null,
                        [],
                        []
                    );
                    $this->add_search_result($r);
                    if ($this->settings->firstmatch) {
                        break;
                    }
                }
            }
        }
    }

    private function add_search_result(SearchResult $r)
    {
        $this->results[] = $r;
    }

    private function cmp_ignorecase(string $s1, string $s2): int
    {
        return strcmp(strtolower($s1), strtolower($s2));
    }

    private function cmp_searchresults(SearchResult $r1, SearchResult $r2): int
    {
        $dircmp = $this->cmp_ignorecase($r1->file->path, $r2->file->path);
        if ($dircmp !== 0) {
            return $dircmp;
        }
        $filecmp = $this->cmp_ignorecase($r1->file->filename, $r2->file->filename);
        if ($filecmp !== 0) {
            return $filecmp;
        }
        if ($r1->linenum === $r2->linenum) {
            return $r1->match_start_index - $r2->match_start_index;
        }
        return $r1->linenum - $r2->linenum;
    }

    public function printresults(): void
    {
        $sorted_results = $this->results;
        usort($sorted_results, array($this, 'cmp_searchresults'));
        $formatter = new SearchResultFormatter($this->settings);
        Logger::log_msg(sprintf("\nSearch results (%d):", count($sorted_results)));
        foreach ($sorted_results as $r) {
            Logger::log_msg($formatter->format($r));
        }
    }

    public function get_matching_dirs(): array
    {
        $dirs = array();
        foreach ($this->results as $r) {
            $d = dirname($r->file);
            if (!in_array($d, $dirs)) {
                $dirs[] = $d;
            }
        }
        sort($dirs);
        return $dirs;
    }

    public function print_matching_dirs(): void
    {
        $dirs = $this->get_matching_dirs();
        Logger::log_msg(sprintf("\nDirectories with matches (%d):", count($dirs)));
        foreach ($dirs as $d) {
            Logger::log_msg($d);
        }
    }

    public function get_matching_files(): array
    {
        $files = array();
        foreach ($this->results as $r) {
            $f = $r->file;
            if (!in_array($f, $files)) {
                $files[] = $f;
            }
        }
        sort($files);
        return $files;
    }

    public function print_matching_files(): void
    {
        $files = $this->get_matching_files();
        Logger::log_msg(sprintf("\nFiles with matches (%d):", count($files)));
        foreach ($files as $f) {
            Logger::log_msg((string)$f);
        }
    }

    public function get_matching_lines(): array
    {
        $lines = array();
        foreach ($this->results as $r) {
            $l = trim($r->line);
            if (!$this->settings->uniquelines || !in_array($l, $lines)) {
                $lines[] = $l;
            }
        }
        usort($lines, 'cmp_ignorecase');
        return $lines;
    }

    public function print_matching_lines(): void
    {
        $lines = $this->get_matching_lines();
        $msg = "\nLines with matches (%d):";
        if ($this->settings->uniquelines) {
            $msg = "\nUnique lines with matches (%d):";
        }
        Logger::log_msg(sprintf($msg, count($lines)));
        foreach ($lines as $l) {
            Logger::log_msg($l);
        }
    }
}
