<?php

/**
 * Class SearchResult
 *
 * @property string pattern
 * @property object file
 * @property int linenum
 * @property int match_start_index
 * @property int match_end_index
 * @property string line
 * @property array lines_before
 * @property array lines_after
 * @property int maxlinelength
 * @property bool colorize
 */
class SearchResult
{
    public function __construct(
        string $pattern,
        $file,
        int $linenum,
        int $match_start_index,
        int $match_end_index,
        string $line,
        array $lines_before,
        array $lines_after
    ) {
        $this->pattern = $pattern;
        $this->file = $file;
        $this->linenum = $linenum;
        $this->match_start_index = $match_start_index;
        $this->match_end_index = $match_end_index;
        $this->line = $line;
        $this->lines_before = $lines_before;
        $this->lines_after = $lines_after;
    }
}
