<?php

function log_msg(string $msg)
{
    echo "$msg\n";
}

function cmp_ignorecase(string $s1, string $s2)
{
    return strcmp(strtolower($s1), strtolower($s2));
}

function cmp_searchresults(SearchResult $r1, SearchResult $r2)
{
    $dircmp = cmp_ignorecase(dirname($r1->file), dirname($r2->file));
    if ($dircmp !== 0) {
        return $dircmp;
    }
    $filecmp = cmp_ignorecase(basename($r1->file), basename($r2->file));
    if ($filecmp !== 0) {
        return $filecmp;
    }
    if ($r1->linenum === $r2->linenum) {
        return $r1->match_start_index - $r2->match_start_index;
    }
    return $r1->linenum - $r2->linenum;
}
