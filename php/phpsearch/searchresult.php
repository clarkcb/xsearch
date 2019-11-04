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
 */
class SearchResult {
    const SEPARATOR_LEN = 80;

    function __construct(string $pattern, $file, int $linenum, int $match_start_index,
        int $match_end_index, string $line, array $lines_before, array $lines_after) {
        $this->pattern = $pattern;
        $this->file = $file;
        $this->linenum = $linenum;
        $this->match_start_index = $match_start_index;
        $this->match_end_index = $match_end_index;
        $this->line = $line;
        $this->lines_before = $lines_before;
        $this->lines_after = $lines_after;
    }

    public function __toString(): string {
        if ($this->lines_before || $this->lines_after) {
            return $this->multiline_tostring();
        }
        return $this->singleline_tostring();
    }

    private function singleline_tostring(): string {
        $s = $this->file;
        if ($this->linenum) {
            $s .= ': ' . $this->linenum . ': ';
            $s .= "[{$this->match_start_index}:{$this->match_end_index}]: ";
            $s .= trim($this->line);
        } else {
            $s .= ' matches at ';
            $s .= "[{$this->match_start_index}:{$this->match_end_index}]";
        }
        return $s;
    }

    private function linenum_padding(): int {
        return strlen(sprintf("%d", $this->linenum + count($this->lines_after)));
    }

    private function trim_newline(string $s) {
        return rtrim($s, "\r\n");
    }

    private function multiline_tostring(): string {
        $s = str_repeat('=', self::SEPARATOR_LEN) . "\n";
        $s .= $this->file . ': ' . $this->linenum . ': ';
        $s .= '[' . $this->match_start_index . ':' . $this->match_end_index . ']';
        $s .= "\n" . str_repeat('-', self::SEPARATOR_LEN) . "\n";
        $lineformat = sprintf(" %%%dd | %%s\n", $this->linenum_padding());
        $current_linenum = $this->linenum;
        if ($this->lines_before) {
            $current_linenum -= count($this->lines_before);
            foreach ($this->lines_before as $line_before) {
                $s .= sprintf(' '.$lineformat, $current_linenum, $this->trim_newline($line_before));
                $current_linenum++;
            }
        }
        $s .= sprintf('>'.$lineformat, $current_linenum, $this->trim_newline($this->line));
        if ($this->lines_after) {
            $current_linenum++;
            foreach ($this->lines_after as $line_after) {
                $s .= sprintf(' '.$lineformat, $current_linenum, $this->trim_newline($line_after));
                $current_linenum++;
            }
        }
        return $s;
    }
}

?>
