<?php

$currdir = realpath(dirname(__FILE__));
$config_rel_path = '../resources/config.json';
$config_json_path = $currdir . '/' . $config_rel_path;
$config = json_decode(file_get_contents($config_json_path));

$sharedpath = $config->{'xsearchpath'} . '/shared';

define('Z_XSEARCHPATH', $config->{'xsearchpath'});
define('Z_SHAREDPATH', $sharedpath);
define('Z_FILETYPESPATH', $sharedpath . '/filetypes.xml');
define('Z_SEARCHOPTIONSPATH', $sharedpath . '/searchoptions.xml');

class Config
{
    const XSEARCHPATH = Z_XSEARCHPATH;
    const SHAREDPATH = Z_SHAREDPATH;
    const FILETYPESPATH = Z_FILETYPESPATH;
    const SEARCHOPTIONSPATH = Z_SEARCHOPTIONSPATH;
}
