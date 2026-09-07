<?php

declare(strict_types=1);

namespace phpsearch;

use phpfind\FileUtil;
use phpfind\FindConfig;

/**
 * Class SearchConfig
 */
readonly class SearchConfig extends FindConfig
{
    public string $search_options_path;
    public string $default_search_settings_path;

    public function __construct()
    {
        parent::__construct();

        $home = getenv('HOME');
        $xsearch_config_dir = getenv('XSEARCH_CONFIG_DIR');
        if (!$xsearch_config_dir) {
            $xsearch_config_dir = FileUtil::join_paths($home, '.config', 'xsearch');
        }

        $resources_path = FileUtil::join_paths(__DIR__, '..', '..', 'resources');
        $search_options_path = FileUtil::join_paths($resources_path, 'searchoptions.json');
        $default_search_settings_path = FileUtil::join_paths($xsearch_config_dir, 'settings.json');

        $this->search_options_path = $search_options_path;
        $this->default_search_settings_path = $default_search_settings_path;
    }
}
