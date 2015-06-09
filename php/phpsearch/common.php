<?php

function log_msg($msg) {
	echo "$msg\n";
}

function cmp_ignorecase($s1, $s2) {
    return strcmp(strtolower($s1), strtolower($s2));
}

function cmp_searchresults($r1, $r2) {
    $dircmp = cmp_ignorecase(dirname($r1->file), dirname($r2->file));
    if ($dircmp === 0) {
        $filecmp = cmp_ignorecase(basename($r1->file), basename($r2->file));
        if ($filecmp === 0) {
            $linenumcmp = $r1->linenum - $r2->linenum;
        	if ($linenumcmp === 0) {
        		return $r1->match_start_index - $r2->match_start_index;
            }
        	return $linenumcmp;
        }
    	return $filecmp;
    }
    return $dircmp;
}
?>
