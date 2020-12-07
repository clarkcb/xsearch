<?php declare(strict_types=1);

namespace phpsearch;

$config_json_path = __DIR__ . '/../../config/config.json';
$config = json_decode(file_get_contents($config_json_path));

$sharedpath = $config->{'xsearchpath'} . '/shared';

$resources_path = __DIR__ . '/../../resources';

define('Z_XSEARCHPATH', $config->{'xsearchpath'});
define('Z_SHAREDPATH', $sharedpath);
define('Z_FILETYPESPATH', $resources_path . '/filetypes.json');
define('Z_SEARCHOPTIONSPATH', $resources_path . '/searchoptions.json');

class Config
{
    const XSEARCHPATH = Z_XSEARCHPATH;
    const SHAREDPATH = Z_SHAREDPATH;
    const FILETYPESPATH = Z_FILETYPESPATH;
    const SEARCHOPTIONSPATH = Z_SEARCHOPTIONSPATH;
}
