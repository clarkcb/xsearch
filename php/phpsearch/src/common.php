<?php declare(strict_types=1);

function log_msg(string $msg)
{
    echo "$msg\n";
}

function cmp_ignorecase(string $s1, string $s2): int
{
    return strcmp(strtolower($s1), strtolower($s2));
}

function cmp_searchresults(SearchResult $r1, SearchResult $r2): int
{
    $dircmp = cmp_ignorecase($r1->file->path, $r2->file->path);
    if ($dircmp !== 0) {
        return $dircmp;
    }
    $filecmp = cmp_ignorecase($r1->file->filename, $r2->file->filename);
    if ($filecmp !== 0) {
        return $filecmp;
    }
    if ($r1->linenum === $r2->linenum) {
        return $r1->match_start_index - $r2->match_start_index;
    }
    return $r1->linenum - $r2->linenum;
}
