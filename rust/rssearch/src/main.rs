use core::slice::Iter;
use std::env;
use std::process;

extern crate rsfind;

use crate::common::{log, log_err_color};
use crate::searcher::{print_results, print_result_dirs, print_result_files, print_result_lines,
                      print_result_matches};
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

fn print_error(error: SearchError, colorize: bool, options: &searchoptions::SearchOptions) {
    log("");
    log_err_color(error.description.as_str(), colorize);
    options.print_usage();
}

fn error_and_exit(error: SearchError, colorize: bool, options: &searchoptions::SearchOptions) {
    print_error(error, colorize, options);
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

            let colorize = settings.colorize();

            let searcher = match searcher::Searcher::new(settings) {
                Ok(searcher) => searcher,
                Err(error) => {
                    print_error(error, colorize, &options);
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
                    if searcher.settings.print_matches() {
                        print_result_matches(&results, &formatter);
                    }
                },
                Err(error) => error_and_exit(error, colorize, &options),
            }
        }
        Err(error) => {
            error_and_exit(error, true, &options);
        }
    }
}

// This is to try to skip the first arg if it is the executable
fn update_args(args: Iter<String>) -> Iter<String> {
    let mut updated_args = args.clone().into_iter();
    match updated_args.next() {
        Some(next_arg) if next_arg.ends_with("rssearch") => updated_args,
        _ => args
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let updated_args = update_args(args.iter());
    search(updated_args);
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
