use core::slice::Iter;
use std::env;
use std::process;

extern crate rsfind;

use crate::common::{log, log_err};
use crate::searcher::{print_results, print_result_dirs, print_result_files, print_result_lines};
use crate::searcherror::SearchError;
use crate::searchresultformatter::SearchResultFormatter;

pub mod common;
pub mod config;
pub mod searcher;
pub mod searcherror;
pub mod searchoptions;
pub mod searchresult;
pub mod searchsettings;
pub mod searchresultformatter;
pub mod searchresultsorter;

fn print_error(error: SearchError, options: &searchoptions::SearchOptions) {
    log("");
    log_err(error.description.as_str());
    options.print_usage();
}

fn error_and_exit(error: SearchError, options: &searchoptions::SearchOptions) {
    print_error(error, options);
    process::exit(1);
}

fn search(args: Iter<String>) {
    let options = match searchoptions::SearchOptions::new() {
        Ok(options) => options,
        Err(error) => {
            log(format!("\nERROR: {}", error.description).as_str());
            process::exit(1);
        }
    };

    match options.settings_from_args(args) {
        Ok(settings) => {
            if settings.debug() {
                log(format!("settings: {:?}", settings).as_str());
            }
            if settings.print_usage() {
                options.print_usage();
                process::exit(0);
            }
            if settings.print_version() {
                options.print_version();
                process::exit(0);
            }

            let searcher = match searcher::Searcher::new(settings) {
                Ok(searcher) => searcher,
                Err(error) => {
                    print_error(error, &options);
                    process::exit(1);
                }
            };

            match searcher.search() {
                Ok(results) => {
                    let formatter = SearchResultFormatter::new(
                        searcher.settings.clone());
                    if searcher.settings.print_results() {
                        print_results(&results, &formatter);
                    }
                    if searcher.settings.print_dirs() {
                        print_result_dirs(&results, &formatter);
                    }
                    if searcher.settings.print_files() {
                        print_result_files(&results, &formatter);
                    }
                    if searcher.settings.print_lines() {
                        print_result_lines(&results, &formatter);
                    }
                },
                Err(error) => error_and_exit(error, &options),
            }
        }
        Err(error) => {
            error_and_exit(error, &options);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    search(args.iter());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_code_files() {
        let startpath = "/Users/cary/src/xsearch/rust";

        let args: Vec<String> = vec![
            "rssearch", "-x", "rs", "-s", "search", "-D", "debug", "-f", "search", "--debug",
            startpath,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        search(args.iter());
    }

    #[test]
    fn test_search_binary_files() {
        let startpath = "/Users/cary/src/xsearch/rust";

        let args: Vec<String> = vec![
            "rssearch", "-x", "rlib", "-s", "search", "-f", "search", "--debug", startpath,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        search(args.iter());
    }
}
