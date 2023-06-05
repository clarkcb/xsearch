<?php declare(strict_types=1);

namespace phpsearch;

use phpfind\FileResult;

/**
 * Class SearchResult
 */
class SearchResult
{
    public string $pattern;
    public ?FileResult $file;
    public int $line_num;
    public int $match_start_index;
    public int $match_end_index;
    public ?string $line;
    public array $lines_before;
    public array $lines_after;

    public function __construct(
        string $pattern,
        ?FileResult $file,
        int $line_num,
        int $match_start_index,
        int $match_end_index,
        ?string $line,
        array $lines_before,
        array $lines_after
    ) {
        $this->pattern = $pattern;
        $this->file = $file;
        $this->line_num = $line_num;
        $this->match_start_index = $match_start_index;
        $this->match_end_index = $match_end_index;
        $this->line = $line;
        $this->lines_before = $lines_before;
        $this->lines_after = $lines_after;
    }
}
