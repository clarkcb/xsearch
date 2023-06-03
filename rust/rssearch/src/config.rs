use std::fs;

use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct Config {
    pub xsearch_path: String,
    pub shared_path: String,
    pub file_types_path: String,
    pub search_options_path: String,
    pub version: String,
}

#[derive(Serialize, Deserialize)]
pub struct JsonConfig {
    xsearchpath: String,
    version: String,
}

pub const XSEARCH_PATH: &str = "/Users/cary/src/xsearch";
pub const CONFIG_FILE_PATH: &str = "/Users/cary/src/xsearch/shared/config.json";
pub const VERSION: &str = "1.0.0";

impl Config {
    pub fn new() -> Config {
        let xsearch_path = String::from(XSEARCH_PATH);
        let version = String::from(VERSION);
        Config::for_values(xsearch_path, version)
    }

    pub fn for_values(xsearch_path: String, version: String) -> Config {
        let shared_path = xsearch_path.clone() + "/shared";
        Config {
            xsearch_path: xsearch_path.clone(),
            shared_path: shared_path.clone(),
            file_types_path: shared_path.clone() + "/filetypes.json",
            search_options_path: shared_path.clone() + "/searchoptions.json",
            version,
        }
    }

    pub fn from_json_file(json_file_path: String) -> Config {
        let contents = fs::read_to_string(json_file_path)
            .expect("Something went wrong reading the config file");
        let json_config: JsonConfig = serde_json::from_str(&contents).unwrap();
        Config::for_values(json_config.xsearchpath, json_config.version)
    }
}
