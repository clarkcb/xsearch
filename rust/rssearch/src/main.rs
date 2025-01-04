use core::slice::Iter;
use std::env;
use std::process;

extern crate rsfind;

use crate::common::{log, log_err};
use crate::searcher::{get_result_dirs, get_result_files, get_result_lines};
use crate::searcherror::SearchError;
use crate::searchresultformatter::SearchResultFormatter;

pub mod color;
pub mod common;
pub mod config;
pub mod searcher;
pub mod searcherror;
pub mod searchoptions;
pub mod searchresult;
pub mod searchsettings;
pub mod searchresultformatter;

fn print_error(error: SearchError, options: &searchoptions::SearchOptions) {
    log("");
    log_err(error.description.as_str());
    options.print_usage();
}

fn error_and_exit(error: SearchError, options: &searchoptions::SearchOptions) {
    print_error(error, options);
    process::exit(1);
}

fn print_result_dirs(results: &Vec<searchresult::SearchResult>) {
    let dirs = get_result_dirs(results);
    if dirs.is_empty() {
        log("\nMatching directories: 0");
    } else {
        log(format!("\nMatching directories ({}):", dirs.len()).as_str());
        for dir in dirs.iter() {
            log(format!("{}", dir).as_str());
        }
    }
}

fn print_result_files(results: &Vec<searchresult::SearchResult>) {
    let files = get_result_files(results);
    if files.is_empty() {
        log("\nMatching files: 0");
    } else {
        log(format!("\nMatching files ({}):", files.len()).as_str());
        for file in files.iter() {
            log(format!("{}", file).as_str());
        }
    }
}

fn print_result_lines(results: &Vec<searchresult::SearchResult>, unique: bool) {
    let lines = get_result_lines(results, unique);
    let lines_title = if unique { "Unique matching lines" } else { "Matching lines" };
    if lines.is_empty() {
        log(format!("\n{}: 0", lines_title).as_str());
    } else {
        log(format!("\n{} ({}):", lines_title, lines.len()).as_str());
        for line in lines.iter() {
            log(format!("{}", line).as_str());
        }
    }
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
                    if searcher.settings.print_results() {
                        let formatter = SearchResultFormatter::new(
                            searcher.settings.colorize(),
                            searcher.settings.max_line_length() as usize);
                        log(format!("\nSearch results ({}):", results.len()).as_str());
                        for r in results.iter() {
                            log(formatter.format(r).as_str());
                        }
                    }
                    if searcher.settings.print_dirs() {
                        print_result_dirs(&results);
                    }
                    if searcher.settings.print_files() {
                        print_result_files(&results);
                    }
                    if searcher.settings.print_lines() {
                        print_result_lines(&results, searcher.settings.unique_lines());
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
