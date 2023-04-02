use std::cmp::Ordering;

use crate::searchfile::SearchFile;

#[derive(Debug, Eq)]
pub struct SearchResult {
    pub pattern: String,
    pub file: Option<SearchFile>,
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
        file: Option<SearchFile>,
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
            (Some(f), Some(o)) => match f.filepath().cmp(&o.filepath()) {
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
            (Some(f), Some(o)) => f.filepath() == o.filepath(),
            (None, None) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::common::log;
    use crate::filetypes::FileType;
    use crate::searchresultformatter::SearchResultFormatter;
    use crate::color::{GREEN, RESET};

    use super::*;

    #[test]
    fn test_singleline_searchresult() {
        let formatter = SearchResultFormatter::new(false, 100);
        let pattern = String::from("Searcher");
        let path = String::from("~/src/xsearch/csharp/CsSearch/CsSearch");
        let filename = String::from("Searcher.cs");
        let file = SearchFile::new(path, filename, FileType::Code);
        let linenum = 10;
        let match_start_index = 15;
        let match_end_index = 23;
        let line = String::from("\tpublic class Searcher\n");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            linenum,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.filepath(),
            linenum,
            match_start_index,
            match_end_index,
            line.trim()
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_singleline_longer_than_maxlinelength_searchresult() {
        let formatter = SearchResultFormatter::new(false, 100);
        let pattern = String::from("maxlen");
        let path = String::from(".");
        let filename = String::from("maxlen.txt");
        let file = SearchFile::new(path, filename, FileType::Code);
        let linenum = 1;
        let match_start_index = 53;
        let match_end_index = 59;
        let line = String::from("0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            linenum,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_line = String::from("...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...");
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.filepath(),
            linenum,
            match_start_index,
            match_end_index,
            expected_line
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_singleline_colorize_searchresult() {
        let formatter = SearchResultFormatter::new(true, 100);
        let pattern = String::from("maxlen");
        let path = String::from(".");
        let filename = String::from("maxlen.txt");
        let file = SearchFile::new(path, filename, FileType::Code);
        let linenum = 1;
        let match_start_index = 53;
        let match_end_index = 59;
        let line = String::from("0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            linenum,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_line = String::from(format!("...89012345678901234567890123456789012345678901{}maxlen{}89012345678901234567890123456789012345678901...", GREEN, RESET));
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.filepath(),
            linenum,
            match_start_index,
            match_end_index,
            expected_line
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_binary_searchresult() {
        let formatter = SearchResultFormatter::new(false, 100);
        let pattern = String::from("Searcher");
        let path = String::from("~/src/xsearch/csharp/CsSearch/CsSearch");
        let filename = String::from("Searcher.exe");
        let file = SearchFile::new(path, filename, FileType::Code);
        let linenum = 0;
        let match_start_index = 15;
        let match_end_index = 23;
        let line = String::from("");
        let lines_before: Vec<String> = Vec::new();
        let lines_after: Vec<String> = Vec::new();
        let result = SearchResult::new(
            pattern,
            Some(file.clone()),
            linenum,
            match_start_index,
            match_end_index,
            line.clone(),
            lines_before,
            lines_after,
        );
        let expected_output = format!(
            "{} matches at [{}:{}]",
            &file.filepath(),
            match_start_index,
            match_end_index
        );
        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_multiline_searchresult() {
        let formatter = SearchResultFormatter::new(false, 100);
        let pattern = String::from("Searcher");
        let path = String::from("~/src/xsearch/csharp/CsSearch/CsSearch");
        let filename = String::from("Searcher.cs");
        let file = SearchFile::new(path, filename, FileType::Code);
        let linenum = 10;
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
            Some(file.clone()),
            linenum,
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
