use std::{env, fs};
use rsfind::findconfig::FindConfig;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct SearchConfig {
    pub find_config: FindConfig,
    pub xsearch_path: String,
    pub shared_path: String,
    pub search_options_path: String,
    pub default_search_settings_path: String,
    pub version: String,
}

#[derive(Serialize, Deserialize)]
pub struct JsonSearchConfig {
    xsearchconfigdir: String,
    xsearchpath: String,
    version: String,
}

pub const VERSION: &str = "1.0.0";

impl SearchConfig {
    pub fn new() -> SearchConfig {
        let default_xsearch_config_dir: String = env::var("HOME").unwrap() + "/.config/xsearch";
        let xsearch_config_dir: String = env::var("XSEARCH_CONFIG_DIR")
            .unwrap_or_else(|_error| default_xsearch_config_dir);
        let default_xsearch_path: String = env::var("HOME").unwrap() + "/src/xsearch";
        let xsearch_path: String = env::var("XSEARCH_PATH")
            .unwrap_or_else(|_error| default_xsearch_path);
        let version = String::from(VERSION);
        SearchConfig::for_values(xsearch_config_dir, xsearch_path, version)
    }

    pub fn for_values(xsearch_config_dir: String, xsearch_path: String, version: String) -> SearchConfig {
        let shared_path = xsearch_path.clone() + "/shared";
        SearchConfig {
            find_config: FindConfig::new(),
            xsearch_path: xsearch_path.clone(),
            shared_path: shared_path.clone(),
            search_options_path: shared_path.clone() + "/searchoptions.json",
            default_search_settings_path: xsearch_config_dir.clone() + "/settings.json",
            version,
        }
    }

    pub fn from_json_file(json_file_path: String) -> SearchConfig {
        let contents = fs::read_to_string(json_file_path)
            .expect("Something went wrong reading the config file");
        let json_config: JsonSearchConfig = serde_json::from_str(&contents).unwrap();
        SearchConfig::for_values(json_config.xsearchconfigdir, json_config.xsearchpath, json_config.version)
    }
}
