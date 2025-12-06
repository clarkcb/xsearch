<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\Color;
use phpfind\FileResultFormatter;

/**
 * Class SearchResultFormatter
 *
 * @property SearchSettings settings
 */
class SearchResultFormatter
{
    const int SEPARATOR_LEN = 80;

    public SearchSettings $settings;
    public FileResultFormatter $file_result_formatter;
    private \Closure $cl_format_line;

    public function __construct(SearchSettings $settings)
    {
        $this->settings = $settings;
        $this->file_result_formatter = new FileResultFormatter($settings);
        if ($settings->colorize) {
            $this->cl_format_line = function (string $line): string { return $this->format_line_with_color($line); };
        } else {
            $this->cl_format_line = function (string $line): string { return $line; };
        }
    }
    private function format_line_with_color(string $line): string
    {
        $formatted_line = $line;
        foreach ($this->settings->search_patterns as $p) {
            $pattern = '/' . $p . '/';
            preg_match($pattern, $formatted_line, $matches, PREG_OFFSET_CAPTURE);
            if (!empty($matches)) {
                $start_index = (int)$matches[0][1];
                $end_index = $start_index + strlen($matches[0][0]);
                $formatted_line = $this->colorize($formatted_line, $start_index, $end_index, $this->settings->line_color);
                break;
            }
        }
        return $formatted_line;
    }

    public function format_line(string $line): string
    {
        // call the closure as defined in constructor
        return ($this->cl_format_line)($line);
    }

    public function format(SearchResult $result): string
    {
        if ($result->lines_before || $result->lines_after) {
            return $this->multi_line_format($result);
        }
        return $this->single_line_format($result);
    }

    private function trim_newline(string $s): string
    {
        return rtrim($s, "\r\n");
    }

    private function colorize(string $s, int $match_start_index, int $match_end_index, Color $color): string
    {
        return $this->file_result_formatter->colorize($s, $match_start_index, $match_end_index, $color);
    }

    private function format_matching_line(SearchResult $result): string
    {
        $formatted = $this->trim_newline($result->line);
        $leading_whitespace_count = 0;
        $whitespace_chars = array(" ", "\t", "\n", "\r");
        while (in_array($formatted[$leading_whitespace_count], $whitespace_chars)) {
            $leading_whitespace_count++;
        }
        $formatted = trim($formatted);
        $formatted_length = strlen($formatted);
        $max_line_end_index = $formatted_length - 1;
        $match_length = $result->match_end_index - $result->match_start_index;

        # track where match start and end indices end up
        $match_start_index = $result->match_start_index - 1 - $leading_whitespace_count;
        $match_end_index = $match_start_index + $match_length;

        # if longer than max_line_length, walk out from matching indices
        if ($formatted_length > $this->settings->max_line_length) {
            $line_start_index = $match_start_index;
            $line_end_index = $line_start_index + $match_length;
            $match_start_index = 0;
            $match_end_index = $match_length;

            # adjust left if/until line_end_index < $formatted_length
            while ($line_end_index > $formatted_length - 1) {
                $line_start_index--;
                $line_end_index--;
                $match_start_index++;
                $match_end_index++;
            }

            $formatted_length = $line_end_index - $line_start_index;

            while ($formatted_length < $this->settings->max_line_length) {
                if ($line_start_index > 0) {
                    $line_start_index--;
                    $match_start_index++;
                    $match_end_index++;
                    $formatted_length = $line_end_index - $line_start_index;
                }
                if ($formatted_length < $this->settings->max_line_length && $line_end_index < $max_line_end_index) {
                    $line_end_index++;
                }
                $formatted_length = $line_end_index - $line_start_index;
            }
            $formatted = substr($formatted, $line_start_index, $formatted_length);

            if ($line_start_index > 2) {
                $formatted = '...' . substr($formatted, 3);
            }
            if ($line_end_index < $max_line_end_index - 3) {
                $formatted = substr($formatted, 0, $formatted_length - 3) . '...';
            }
        }

        if ($this->settings->colorize) {
            $formatted = $this->colorize($formatted, $match_start_index, $match_end_index, $this->settings->line_color);
        }
        return $formatted;
    }

    private function single_line_format(SearchResult $result): string
    {
        $s = $this->file_result_formatter->format_file_result($result->file);
        if ($result->line_num) {
            $s .= ': ' . $result->line_num . ': ';
            $s .= "[{$result->match_start_index}:{$result->match_end_index}]: ";
            $s .= $this->format_matching_line($result);
        } else {
            $s .= ' matches at ';
            $s .= "[{$result->match_start_index}:{$result->match_end_index}]";
        }
        return $s;
    }

    private function line_num_padding(SearchResult $result): int
    {
        return strlen(sprintf("%d", $result->line_num + count($result->lines_after)));
    }

    private function multi_line_format(SearchResult $result): string
    {
        $s = str_repeat('=', self::SEPARATOR_LEN) . "\n";
        $s .= $this->file_result_formatter->format_file_result($result->file) . ': ' . $result->line_num . ': ';
        $s .= '[' . $result->match_start_index . ':' . $result->match_end_index . ']';
        $s .= "\n" . str_repeat('-', self::SEPARATOR_LEN) . "\n";
        $line_format = sprintf(" %%%dd | %%s\n", $this->line_num_padding($result));
        $current_line_num = $result->line_num;
        if ($result->lines_before) {
            $current_line_num -= count($result->lines_before);
            foreach ($result->lines_before as $line_before) {
                $s .= sprintf(' '.$line_format, $current_line_num, $this->trim_newline($line_before));
                $current_line_num++;
            }
        }
        $line = $this->trim_newline($result->line);
        if ($this->settings->colorize) {
            $line = $this->colorize(
                $line,
                $result->match_start_index - 1,
                $result->match_end_index - 1,
                $this->settings->line_color
            );
        }
        $s .= sprintf('>'.$line_format, $current_line_num, $line);
        if ($result->lines_after) {
            $current_line_num++;
            foreach ($result->lines_after as $line_after) {
                $s .= sprintf(' '.$line_format, $current_line_num, $this->trim_newline($line_after));
                $current_line_num++;
            }
        }
        return $s;
    }
}
