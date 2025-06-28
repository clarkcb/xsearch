use std::cmp::Ordering;
use rsfind::fileresult::FileResult;

#[derive(Debug, Eq)]
pub struct SearchResult {
    pub pattern: String,
    pub file: Option<FileResult>,
    pub line_num: usize,
    pub match_start_index: usize,
    pub match_end_index: usize,
    pub line: String,
    pub lines_before: Vec<String>,
    pub lines_after: Vec<String>,
}

impl SearchResult {
    pub fn new(
        pattern: String,
        file: Option<FileResult>,
        line_num: usize,
        match_start_index: usize,
        match_end_index: usize,
        line: String,
        lines_before: Vec<String>,
        lines_after: Vec<String>,
    ) -> SearchResult {
        SearchResult {
            pattern,
            file,
            line_num,
            match_start_index,
            match_end_index,
            line,
            lines_before,
            lines_after,
        }
    }
}

impl Ord for SearchResult {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.file, &other.file) {
            (Some(f), Some(o)) => match f.file_path().cmp(&o.file_path()) {
                Ordering::Equal => match self.line_num.cmp(&other.line_num) {
                    Ordering::Equal => self.match_start_index.cmp(&other.match_start_index),
                    other => other,
                },
                other => other,
            },
            _ => match self.line_num.cmp(&other.line_num) {
                Ordering::Equal => self.match_start_index.cmp(&other.match_start_index),
                other => other,
            },
        }
    }
}

impl PartialOrd for SearchResult {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for SearchResult {
    fn eq(&self, other: &Self) -> bool {
        match (&self.file, &other.file) {
            (Some(f), Some(o)) => f.file_path() == o.file_path(),
            (None, None) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::common::log;
    use crate::searchresultformatter::SearchResultFormatter;
    use rsfind::color::{GREEN, RESET};
    use std::path::Path;

    use rsfind::filetypes::FileType;
    use crate::searchsettings::SearchSettings;
    use super::*;

    #[test]
    fn test_single_line_search_result() {
        let settings = SearchSettings::default();
        let formatter = SearchResultFormatter::new(settings);
        let pattern = String::from("Searcher");
        let file_path = Path::new("~/src/xsearch/rust/rssearch/src/searcher.rs");

        let file = FileResult::new(file_path.to_path_buf(), FileType::Code, 0, 0);
        let line_num = 10;
        let match_start_index = 15;
        let match_end_index = 23;
        let line = String::from("\tpublic class Searcher\n");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            line.trim()
        );
        let result = SearchResult::new(
            pattern,
            Some(file),
            line_num,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_single_line_longer_than_max_line_length_search_result() {
        let settings = SearchSettings::default();
        let formatter = SearchResultFormatter::new(settings);
        let pattern = String::from("maxlen");
        let file_path = Path::new("./maxlen.txt");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Code, 0, 0);
        let line_num = 1;
        let match_start_index = 53;
        let match_end_index = 59;
        let line = String::from("0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            line_num,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_line = String::from("...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...");
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            expected_line
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_single_line_colorize_search_result() {
        let settings = SearchSettings::default();
        let formatter = SearchResultFormatter::new(settings);
        let pattern = String::from("maxlen");
        let file_path = Path::new("./maxlen.txt");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Code, 0, 0);
        let line_num = 1;
        let match_start_index = 53;
        let match_end_index = 59;
        let line = String::from("0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            line_num,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_line = String::from(format!("...89012345678901234567890123456789012345678901{}maxlen{}89012345678901234567890123456789012345678901...", GREEN, RESET));
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            expected_line
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_binary_search_result() {
        let settings = SearchSettings::default();
        let formatter = SearchResultFormatter::new(settings);
        let pattern = String::from("Searcher");
        let file_path = Path::new("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Code, 0, 0);
        let line_num = 0;
        let match_start_index = 15;
        let match_end_index = 23;
        let line = String::from("");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            line_num,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_output = format!(
            "{} matches at [{}:{}]",
            &file.file_path(),
            match_start_index,
            match_end_index
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_multi_line_search_result() {
        let settings = SearchSettings::default();
        let formatter = SearchResultFormatter::new(settings);
        let pattern = String::from("Searcher");
        let file_path = Path::new("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Code, 0, 0);
        let line_num = 10;
        let match_start_index = 15;
        let match_end_index = 23;
        let line = String::from("\tpublic class Searcher");
        let lines_before: Vec<String> = vec!["namespace CsSearch", "{"]
            .into_iter()
            .map(|l| l.to_string())
            .collect();
        let lines_after: Vec<String> = vec!["\t{", "\t\tprivate readonly FileTypes _fileTypes;"]
            .into_iter()
            .map(|l| l.to_string())
            .collect();
        let result = SearchResult::new(
            pattern,
            Some(file),
            line_num,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_output = "\
================================================================================
~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs: 10: [15:23]
--------------------------------------------------------------------------------
   8 | namespace CsSearch
   9 | {
> 10 | \tpublic class Searcher
  11 | \t{
  12 | \t\tprivate readonly FileTypes _fileTypes;
";
        log(format!("\nexpected_output:\n{}", expected_output).as_str());
        let result_output = formatter.format(&result);
        log(format!("\nresult_output:\n{}", result_output).as_str());
        assert_eq!(result_output, expected_output);
    }
}
