<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileUtil;

// We have to include this here because Config.php is the only file defined before autoload.php
include __DIR__ . '/../../../../../xfind/php/phpfind/src/phpfind/FileUtil.php';

$xsearch_path = getenv('XSEARCH_PATH');
if (!$xsearch_path) {
    $home = getenv('HOME');
    if ($home) {
        $xsearch_path = FileUtil::join_paths($home, 'src', 'xsearch');
    } else {
        $xsearch_path = FileUtil::join_paths(__DIR__, '..', '..', '..', '..');
    }
}
$xsearch_shared_path = FileUtil::join_paths($xsearch_path, 'shared');

$resources_path = __DIR__ . '/../../resources';
$search_options_path = FileUtil::join_paths($resources_path, 'searchoptions.json');

define('Z_XSEARCH_PATH', $xsearch_path);
define('Z_XSEARCH_SHARED_PATH', $xsearch_shared_path);
define('Z_SEARCH_OPTIONS_PATH', $search_options_path);

class Config
{
    const string XSEARCH_PATH = Z_XSEARCH_PATH;
    const string XSEARCH_SHARED_PATH = Z_XSEARCH_SHARED_PATH;
    const string SEARCH_OPTIONS_PATH = Z_SEARCH_OPTIONS_PATH;
}
