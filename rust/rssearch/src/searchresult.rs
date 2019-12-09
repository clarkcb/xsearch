use std::cmp::Ordering;
use std::fmt;

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
            pattern: pattern,
            file: file,
            line_num: line_num,
            match_start_index: match_start_index,
            match_end_index: match_end_index,
            line: line,
            lines_before: lines_before,
            lines_after: lines_after,
        }
    }

    fn linenum_padding(&self) -> usize {
        format!("{}", self.line_num + self.lines_after.len()).len()
    }
}

const SEPARATOR_LEN: usize = 80;

impl fmt::Display for SearchResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.lines_before.is_empty() || !self.lines_after.is_empty() {
            let mut buffer = String::new();
            buffer.push_str("=".repeat(SEPARATOR_LEN).as_str());
            match &self.file {
                Some(f) => {
                    buffer.push_str(
                        format!(
                            "\n{}: {}: [{}:{}]\n",
                            f.filepath(),
                            self.line_num,
                            self.match_start_index,
                            self.match_end_index
                        )
                        .as_str(),
                    );
                }
                None => {
                    buffer.push_str(
                        format!(
                            "\n{}: [{}:{}]\n",
                            self.line_num, self.match_start_index, self.match_end_index
                        )
                        .as_str(),
                    );
                }
            }
            buffer.push_str("-".repeat(SEPARATOR_LEN).as_str());
            buffer.push_str("\n");
            let linenum_padding = self.linenum_padding();
            let mut current_linenum = self.line_num;
            if !self.lines_before.is_empty() {
                current_linenum -= self.lines_before.len();
                for line_before in self.lines_before.iter() {
                    let lb = format!(
                        "  {:linenum_padding$} | {}\n",
                        current_linenum,
                        line_before,
                        linenum_padding = linenum_padding
                    );
                    buffer.push_str(lb.as_str());
                    current_linenum += 1;
                }
            }
            let l = format!(
                "> {:linenum_padding$} | {}\n",
                current_linenum,
                self.line,
                linenum_padding = linenum_padding
            );
            buffer.push_str(l.as_str());
            if !self.lines_after.is_empty() {
                current_linenum += 1;
                for line_after in self.lines_after.iter() {
                    let la = format!(
                        "  {:linenum_padding$} | {}\n",
                        current_linenum,
                        line_after,
                        linenum_padding = linenum_padding
                    );
                    buffer.push_str(la.as_str());
                    current_linenum += 1;
                }
            }
            write!(f, "{}", buffer)
        } else {
            let file_prefix = match &self.file {
                Some(f) => f.filepath(),
                None => "<text>".to_string(),
            };
            if self.line_num > 0 {
                return write!(
                    f,
                    "{}: {}: [{}:{}]: {}",
                    file_prefix,
                    self.line_num,
                    self.match_start_index,
                    self.match_end_index,
                    self.line.trim()
                );
            }
            write!(
                f,
                "{} matches at [{}:{}]",
                file_prefix, self.match_start_index, self.match_end_index
            )
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

    use super::*;

    #[test]
    fn test_singleline_searchresult() {
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
        let result_output = format!("{}", result);
        assert_eq!(result_output, expected_output);
    }

    #[test]
    fn test_binary_searchresult() {
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
        let result_output = format!("{}", result);
        assert_eq!(result_output, expected_output);
    }

    #[test]
    fn test_multiline_searchresult() {
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
        let result_output = format!("{}", result);
        log(format!("\nresult_output:\n{}", result_output).as_str());
        assert_eq!(result_output, expected_output);
    }
}
