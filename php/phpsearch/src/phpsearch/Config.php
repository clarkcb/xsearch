<?php declare(strict_types=1);

namespace phpsearch;

$config_json_path = __DIR__ . '/../../config/config.json';
$config = json_decode(file_get_contents($config_json_path));

$resources_path = __DIR__ . '/../../resources';

define('Z_XSEARCHPATH', $config->{'xsearchpath'});
define('Z_SEARCHOPTIONSPATH', $resources_path . '/searchoptions.json');

class Config
{
    const XSEARCHPATH = Z_XSEARCHPATH;
    const SHAREDPATH = Z_SHAREDPATH;
    const SEARCHOPTIONSPATH = Z_SEARCHOPTIONSPATH;
}
