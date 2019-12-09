use std::collections::HashSet;
use std::collections::VecDeque;
use std::error::Error;
use std::fs;
use std::io;
use std::io::Read;
use std::str::Lines;

use encoding::{DecoderTrap, Encoding, label};
use regex::{Match, Regex};
use walkdir::WalkDir;

use crate::common::log;
use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::searcherror::SearchError;
use crate::searchfile::SearchFile;
use crate::searchresult::SearchResult;
use crate::searchsettings::SearchSettings;

pub struct Searcher {
    pub filetypes: FileTypes,
    pub settings: SearchSettings,
}

impl Searcher {
    /// Create a new Searcher instance for the given settings, if valid
    pub fn new(settings: SearchSettings) -> Result<Searcher, SearchError> {
        let filetypes = match FileTypes::new() {
            Ok(filetypes) => filetypes,
            Err(error) => return Err(error),
        };

        if let Err(error) = Searcher::validate_settings(&settings) {
            return Err(error);
        }

        Ok(Searcher {
            filetypes: filetypes,
            settings: settings,
        })
    }

    fn validate_settings(settings: &SearchSettings) -> Result<(), SearchError> {
        if settings.startpath == "" {
            return Err(SearchError::new("Startpath not defined"));
        }
        let metadata = fs::metadata(&settings.startpath);
        if metadata.is_err() {
            match metadata.err().unwrap().kind() {
                io::ErrorKind::NotFound => return Err(SearchError::new("Startpath not found")),
                io::ErrorKind::PermissionDenied => {
                    return Err(SearchError::new("Startpath not readable"))
                }
                _ => {
                    return Err(SearchError::new(
                        "An unknown error occurred trying to read startpath",
                    ))
                }
            }
        }
        if settings.search_patterns.is_empty() {
            return Err(SearchError::new("No search patterns defined"));
        }
        if let None = label::encoding_from_whatwg_label(&settings.text_file_encoding) {
            return Err(SearchError::new(
                "Invalid or unsupported text file encoding",
            ))
        }
        Ok(())
    }

    fn matches_any_string(&self, string: &str, strings: &[String]) -> bool {
        strings.iter().any(|s| s == string)
    }

    fn matches_any_pattern(&self, s: &String, patterns: &[Regex]) -> bool {
        for p in patterns.iter() {
            if p.is_match(s) {
                return true;
            }
        }
        false
    }

    fn any_matches_any_pattern(&self, ss: &[String], patterns: &[Regex]) -> bool {
        for s in ss.iter() {
            if self.matches_any_pattern(s, patterns) {
                return true;
            }
        }
        false
    }

    fn is_search_dir(&self, dir: &String) -> bool {
        if FileUtil::is_hidden(&dir) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_dir_patterns.is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns))
            && (self.settings.out_dir_patterns.is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns))
    }

    fn is_archive_search_file(&self, searchfile: &SearchFile) -> bool {
        if !self.is_search_dir(&searchfile.path) {
            return false;
        }
        if FileUtil::is_hidden(&searchfile.name) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_archive_file_patterns.is_empty()
            || self.matches_any_pattern(&searchfile.name, &self.settings.in_archive_file_patterns))
            && (self.settings.out_archive_file_patterns.is_empty()
                || !self.matches_any_pattern(
                    &searchfile.name,
                    &self.settings.out_archive_file_patterns,
                ))
    }

    fn is_search_file(&self, searchfile: &SearchFile) -> bool {
        if !self.is_search_dir(&searchfile.path) {
            return false;
        }
        if FileUtil::is_hidden(&searchfile.name) && self.settings.exclude_hidden {
            return false;
        }
        if let Some(ext) = FileUtil::get_extension(&searchfile.name) {
            if (!self.settings.in_extensions.is_empty()
                && !self.matches_any_string(ext, &self.settings.in_extensions))
                || (!self.settings.out_extensions.is_empty()
                    && self.matches_any_string(ext, &self.settings.out_extensions))
            {
                return false;
            }
        }
        (self.settings.in_file_patterns.is_empty()
            || self.matches_any_pattern(&searchfile.name, &self.settings.in_file_patterns))
            && (self.settings.in_file_types.is_empty()
                || self.settings.in_file_types.contains(&searchfile.filetype))
            && (self.settings.out_file_patterns.is_empty()
                || !self.matches_any_pattern(&searchfile.name, &self.settings.out_file_patterns))
            && (self.settings.out_file_types.is_empty()
                || !self.settings.out_file_types.contains(&searchfile.filetype))
    }

    fn filter_file(&self, searchfile: &SearchFile) -> bool {
        if searchfile.filetype == FileType::Archive {
            return self.settings.search_archives && self.is_archive_search_file(searchfile);
        }
        !self.settings.archives_only && self.is_search_file(searchfile)
    }

    /// Initiate a searcher search for the given settings and get the results
    pub fn search(&self) -> Result<Vec<SearchResult>, SearchError> {
        let mut searchfiles: Vec<SearchFile> = Vec::new();
        for entry in WalkDir::new(&self.settings.startpath)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
        {
            let path = match entry.path().parent() {
                Some(parent) => parent.to_str().unwrap().to_string(),
                None => ".".to_string(),
            };
            let filename = entry.file_name().to_str().unwrap().to_string();
            let filetype = self.filetypes.get_file_type(&filename);
            if filetype == FileType::Unknown {
                continue;
            }
            let searchfile = SearchFile::new(path, filename, filetype);
            if self.filter_file(&searchfile) {
                searchfiles.push(searchfile)
            }
        }

        if self.settings.verbose && !searchfiles.is_empty() {
            let mut dirs: Vec<String> = searchfiles.iter().map(|f| f.path.clone()).collect();
            dirs.sort_unstable();
            dirs.dedup();
            log(format!("\nDirectories to be searched ({}):", dirs.len()).as_str());
            for dir in dirs.iter() {
                log(format!("{}", dir).as_str());
            }

            let mut files: Vec<String> = searchfiles.iter().map(|f| f.filepath()).collect();
            files.sort_unstable();
            log(format!("\nFiles to be searched ({}):", files.len()).as_str());
            for file in files.iter() {
                log(format!("{}", file).as_str());
            }
        }

        self.search_files(&searchfiles)
    }

    fn search_files(&self, searchfiles: &[SearchFile]) -> Result<Vec<SearchResult>, SearchError> {
        let mut results: Vec<SearchResult> = Vec::new();
        for sf in searchfiles.iter() {
            match self.search_file(&sf) {
                Ok(mut file_results) => {
                    results.append(&mut file_results);
                }
                Err(error) => {
                    log(format!("{}", error).as_str());
                    return Err(error);
                }
            }
        }
        results.sort_unstable();
        Ok(results)
    }

    /// Search an individual file and get the results
    pub fn search_file(&self, searchfile: &SearchFile) -> Result<Vec<SearchResult>, SearchError> {
        if searchfile.filetype == FileType::Text
            || searchfile.filetype == FileType::Code
            || searchfile.filetype == FileType::Xml
        {
            return self.search_text_file(searchfile);
        } else if searchfile.filetype == FileType::Binary {
            return self.search_binary_file(searchfile);
        }
        Ok(vec![])
    }

    fn get_encoded_byte_string(
        &self,
        searchfile: &SearchFile,
        enc: &dyn Encoding,
    ) -> Result<String, SearchError> {
        match fs::File::open(searchfile.filepath()) {
            Ok(mut f) => {
                let mut buffer: Vec<u8> = Vec::new();
                match f.read_to_end(&mut buffer) {
                    Ok(_) => match enc.decode(&buffer, DecoderTrap::Strict) {
                        Ok(bytestring) => Ok(bytestring),
                        Err(cow) => return Err(SearchError::new(&cow.to_string())),
                    },
                    Err(error) => {
                        return Err(SearchError::new(error.description()));
                    }
                }
            }
            Err(error) => {
                return Err(SearchError::new(error.description()));
            }
        }
    }

    fn get_byte_string(&self, searchfile: &SearchFile) -> Result<String, SearchError> {
        self.get_encoded_byte_string(searchfile, encoding::all::ISO_8859_1)
    }

    fn search_binary_file(
        &self,
        searchfile: &SearchFile,
    ) -> Result<Vec<SearchResult>, SearchError> {
        if self.settings.verbose {
            log(format!("Searching binary file {}", searchfile.filepath()).as_str());
        }
        let mut results: Vec<SearchResult> = Vec::new();
        match self.get_byte_string(searchfile) {
            Ok(bytestring) => match self.search_binary_byte_string(&bytestring) {
                Ok(rs) => {
                    for r in rs.iter() {
                        results.push(SearchResult::new(
                            r.pattern.clone(),
                            Some(searchfile.clone()),
                            0,
                            r.match_start_index,
                            r.match_end_index,
                            "".to_string(),
                            Vec::new(),
                            Vec::new(),
                        ));
                    }
                }
                Err(error) => return Err(error),
            },
            Err(error) => return Err(error),
        }
        Ok(results)
    }

    fn search_binary_byte_string(
        &self,
        bytestring: &String,
    ) -> Result<Vec<SearchResult>, SearchError> {
        let mut results: Vec<SearchResult> = Vec::new();
        for p in &self.settings.search_patterns {
            let matches: Vec<Match> = if self.settings.first_match {
                match p.find(&bytestring) {
                    Some(m) => vec![m],
                    None => vec![],
                }
            } else {
                p.find_iter(&bytestring).collect()
            };
            for m in matches {
                let r = SearchResult::new(
                    p.as_str().to_string(),
                    None,
                    0,
                    m.start() + 1,
                    m.end() + 1,
                    "".to_string(),
                    Vec::new(),
                    Vec::new(),
                );
                results.push(r);
            }
        }
        Ok(results)
    }

    /// Try to get the file contents for the given encoding (UTF-8 by default)
    fn get_text_file_contents(
        &self,
        searchfile: &SearchFile,
        encoding: &'static dyn Encoding,
    ) -> Result<String, SearchError> {
        let contents = match encoding.name() {
            "utf-8" => match fs::read_to_string(searchfile.filepath()) {
                Ok(contents) => contents,
                Err(error) if error.kind() == io::ErrorKind::InvalidData => {
                    match self.get_byte_string(searchfile) {
                        Ok(bytestring) => bytestring,
                        Err(error) => return Err(error),
                    }
                }
                Err(error) => {
                    let msg = format!("{} (file: {}", error.description(), searchfile.filepath());
                    return Err(SearchError::new(&msg));
                }
            },
            _ => match self.get_encoded_byte_string(searchfile, encoding) {
                Ok(bytestring) => bytestring,
                Err(error) => return Err(error),
            },
        };
        Ok(contents)
    }

    fn search_text_file(&self, searchfile: &SearchFile) -> Result<Vec<SearchResult>, SearchError> {
        let encoding =
            label::encoding_from_whatwg_label(&self.settings.text_file_encoding).unwrap();
        if self.settings.verbose {
            log(format!("Searching text file {}", searchfile.filepath()).as_str());
        }
        if self.settings.multiline_search {
            return self.search_text_file_contents(searchfile, encoding);
        }
        return self.search_text_file_lines(searchfile, encoding);
    }

    fn search_text_file_lines<'a>(
        &self,
        searchfile: &'a SearchFile,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<SearchResult>, SearchError> {
        let contents = match self.get_text_file_contents(searchfile, encoding) {
            Ok(contents) => contents,
            Err(error) => return Err(error),
        };
        let mut lines = contents.lines();
        Ok(self
            .search_text_lines(&mut lines)
            .iter()
            .map(|r| {
                SearchResult::new(
                    r.pattern.clone(),
                    Some(searchfile.clone()),
                    r.line_num,
                    r.match_start_index,
                    r.match_end_index,
                    r.line.clone(),
                    r.lines_before.clone(),
                    r.lines_after.clone(),
                )
            })
            .collect())
    }

    fn lines_match(&self, lines: &[String], in_patterns: &[Regex], out_patterns: &[Regex]) -> bool {
        (in_patterns.is_empty() || self.any_matches_any_pattern(lines, in_patterns))
            && (out_patterns.is_empty() || !self.any_matches_any_pattern(lines, out_patterns))
    }

    /// Search a Lines iterator
    pub fn search_text_lines<'a>(&self, lines: &mut Lines) -> Vec<SearchResult> {
        let mut results: Vec<SearchResult> = Vec::new();
        let mut current_linenum = 1usize;
        let mut lines_before: VecDeque<&str> = VecDeque::new();
        let mut lines_after: VecDeque<&str> = VecDeque::new();
        let mut matched_patterns: HashSet<&str> = HashSet::new();
        loop {
            let line = if lines_after.len() > 0 {
                lines_after.pop_front().unwrap()
            } else if let Some(line) = lines.next() {
                line
            } else {
                break;
            };
            while lines_after.len() < self.settings.lines_after {
                match lines.next() {
                    Some(l) => lines_after.push_back(l),
                    None => break,
                }
            }
            for p in &self.settings.search_patterns {
                for m in p.find_iter(line) {
                    let mut v_lines_before: Vec<String> = Vec::new();
                    if !lines_before.is_empty() {
                        for line_before in lines_before.iter() {
                            v_lines_before.push(line_before.to_string());
                        }
                        if !self.lines_match(
                            &v_lines_before,
                            &self.settings.in_lines_before_patterns,
                            &self.settings.out_lines_before_patterns,
                        ) {
                            continue;
                        }
                    }
                    let mut v_lines_after: Vec<String> = Vec::new();
                    if !lines_after.is_empty() {
                        for line_after in lines_after.iter() {
                            v_lines_after.push(line_after.to_string());
                        }
                        if !self.lines_match(
                            &v_lines_after,
                            &self.settings.in_lines_after_patterns,
                            &self.settings.out_lines_after_patterns,
                        ) {
                            continue;
                        }
                    }

                    let has_lines_after_to_patterns =
                        !self.settings.lines_after_to_patterns.is_empty();
                    let has_lines_after_until_patterns =
                        !self.settings.lines_after_until_patterns.is_empty();
                    if has_lines_after_to_patterns || has_lines_after_until_patterns {
                        let lines_after_patterns = if has_lines_after_to_patterns {
                            &self.settings.lines_after_to_patterns
                        } else {
                            &self.settings.lines_after_until_patterns
                        };

                        let mut lines_after_match =
                            self.any_matches_any_pattern(&v_lines_after, &lines_after_patterns);
                        while !lines_after_match {
                            match lines.next() {
                                Some(line) => {
                                    if self.matches_any_pattern(
                                        &line.to_string(),
                                        &lines_after_patterns,
                                    ) {
                                        if has_lines_after_to_patterns {
                                            lines_after.push_back(line);
                                            v_lines_after.push(line.to_string());
                                        }
                                        lines_after_match = true;
                                    } else {
                                        lines_after.push_back(line);
                                        v_lines_after.push(line.to_string());
                                    }
                                }
                                None => break,
                            }
                        }
                        if !lines_after_match {
                            continue;
                        }
                    }

                    let r = SearchResult::new(
                        p.as_str().to_string(),
                        None,
                        current_linenum,
                        m.start() + 1,
                        m.end() + 1,
                        line.to_string().clone(),
                        v_lines_before,
                        v_lines_after,
                    );
                    results.push(r);
                    matched_patterns.insert(p.as_str());
                    if self.settings.first_match
                        && matched_patterns.len() == self.settings.search_patterns.len()
                    {
                        return results;
                    }
                }
            }
            if self.settings.lines_before > 0 {
                if lines_before.len() == self.settings.lines_before {
                    lines_before.pop_front();
                }
                if lines_before.len() < self.settings.lines_before {
                    lines_before.push_back(line);
                }
            }
            current_linenum += 1;
        }
        results
    }

    fn search_text_file_contents(
        &self,
        searchfile: &SearchFile,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<SearchResult>, SearchError> {
        let contents = match self.get_text_file_contents(searchfile, encoding) {
            Ok(contents) => contents,
            Err(error) => return Err(error),
        };
        Ok(self
            .search_multiline_string(&contents)
            .iter()
            .map(|r| {
                SearchResult::new(
                    r.pattern.clone(),
                    Some(searchfile.clone()),
                    r.line_num,
                    r.match_start_index,
                    r.match_end_index,
                    r.line.clone(),
                    r.lines_before.clone(),
                    r.lines_after.clone(),
                )
            })
            .collect())
    }

    fn get_lines_from_contents(&self, contents: &str, newline_indices: Vec<usize>) -> Vec<String> {
        if newline_indices.is_empty() {
            vec![]
        } else {
            let mut lines: Vec<String> = vec![];
            let mut i = 0usize;
            while i < newline_indices.len() - 1 {
                lines.push(contents[newline_indices[i] + 1..newline_indices[i + 1]].to_string());
                i += 1;
            }
            lines
        }
    }

    /// Search a file's contents as a multiline string
    pub fn search_multiline_string<'a>(&self, contents: &str) -> Vec<SearchResult> {
        let mut results: Vec<SearchResult> = Vec::new();

        let newline_indices: Vec<usize> = contents.match_indices("\n").map(|i| i.0).collect();
        let mut startline_indices: Vec<usize> = vec![0];
        startline_indices.append(&mut newline_indices.iter().map(|n| n + 1).collect());

        for p in &self.settings.search_patterns {
            let matches: Vec<Match> = if self.settings.first_match {
                match p.find(contents) {
                    Some(m) => vec![m],
                    None => vec![],
                }
            } else {
                p.find_iter(contents).collect()
            };
            for m in matches {
                let lines_before_count = startline_indices
                    .iter()
                    .take_while(|i| i <= &&m.start())
                    .count();
                let line_start_index: usize = match startline_indices.get(lines_before_count - 1) {
                    Some(i) => *i,
                    None => 0usize,
                };
                let line_ends_before: Vec<_> = newline_indices
                    .iter()
                    .skip_while(|i| i <= &&m.start())
                    .take(1)
                    .collect();
                let line_end_index: usize = match line_ends_before.get(0) {
                    Some(i) => **i,
                    None => contents.len(),
                };
                let line = &contents[line_start_index..line_end_index];
                let lines_before: Vec<String> = if self.settings.lines_before > 0 {
                    let nlis: &[usize] = &newline_indices[..lines_before_count - 1];
                    let nlis: Vec<usize> = nlis
                        .iter()
                        .rev()
                        .take(self.settings.lines_before + 1)
                        .rev()
                        .cloned()
                        .collect();
                    self.get_lines_from_contents(contents, nlis)
                } else {
                    vec![]
                };
                if !self.lines_match(
                    &lines_before,
                    &self.settings.in_lines_before_patterns,
                    &self.settings.out_lines_before_patterns,
                ) {
                    continue;
                }
                let lines_after: Vec<String> = if (self.settings.lines_after > 0
                    || !self.settings.lines_after_to_patterns.is_empty()
                    || !self.settings.lines_after_until_patterns.is_empty())
                    && newline_indices.len() > lines_before_count
                {
                    let nlis: &[usize] = &newline_indices[lines_before_count - 1..];
                    let nlis: Vec<usize> = if self.settings.lines_after > 0 {
                        nlis.iter()
                            .take(self.settings.lines_after + 1)
                            .cloned()
                            .collect()
                    } else {
                        match p.find(&contents[line_end_index..]) {
                            Some(m) => {
                                let match_start_index = m.start() + line_end_index;
                                let _nlis: Vec<usize> = nlis
                                    .iter()
                                    .take_while(|i| i < &&match_start_index)
                                    .cloned()
                                    .collect();

                                if !self.settings.lines_after_to_patterns.is_empty() {
                                    nlis.iter().take(_nlis.len() + 1).cloned().collect()
                                } else {
                                    _nlis
                                }
                            }
                            None => vec![],
                        }
                    };
                    self.get_lines_from_contents(contents, nlis)
                } else {
                    vec![]
                };
                if !lines_after.is_empty() {
                    if !self.lines_match(
                        &lines_after,
                        &self.settings.in_lines_after_patterns,
                        &self.settings.out_lines_after_patterns,
                    ) {
                        continue;
                    }
                } else if !self.settings.lines_after_to_patterns.is_empty()
                    || !self.settings.lines_after_until_patterns.is_empty()
                {
                    continue;
                }
                let r = SearchResult::new(
                    p.as_str().to_string(),
                    None,
                    lines_before_count,
                    m.start() - line_start_index + 1,
                    m.end() - line_start_index + 1,
                    line.to_string().clone(),
                    lines_before,
                    lines_after,
                );
                results.push(r);
            }
        }

        results
    }
}

/// Get the unique list of directories for which search results were found
pub fn get_result_dirs(results: &[SearchResult]) -> Vec<&String> {
    let mut dirs: Vec<&String> = Vec::new();
    for r in results.iter() {
        if let Some(f) = &r.file {
            dirs.push(&f.path);
        }
    }
    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

/// Get the unique list of files for which search results were found
pub fn get_result_files(results: &[SearchResult]) -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for r in results.iter() {
        if let Some(f) = &r.file {
            let filepath = f.filepath();
            files.push(filepath);
        }
    }
    files.sort_unstable();
    files.dedup();
    files
}

/// Get the [unique] list of lines containing matches for a set of search results
pub fn get_result_lines(results: &[SearchResult], unique: bool) -> Vec<&str> {
    let mut lines: Vec<&str> = Vec::new();
    for r in results.iter() {
        lines.push(&r.line.trim());
    }
    lines.sort_unstable();
    if unique {
        lines.dedup();
    }
    lines
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::config::{Config, CONFIG_FILE_PATH};
    use crate::filetypes::FileType;

    use super::*;

    fn get_default_test_settings() -> SearchSettings {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from(".");
        settings.add_search_pattern(String::from("Searcher"));
        settings
    }

    #[test]
    fn test_filter_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(searcher.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(searcher.filter_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.filter_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.filter_file(&file));

        let path = String::from(".");
        let filename = String::from(".codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(!searcher.filter_file(&file));

        let mut settings = get_default_test_settings();
        settings.search_archives = true;
        let searcher = Searcher::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(searcher.filter_file(&file));
    }

    #[test]
    fn test_is_search_dir() {
        let mut settings = get_default_test_settings();
        settings.add_out_dir_pattern(String::from("temp"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let path = String::from(".");
        assert!(searcher.is_search_dir(&path));

        let path = String::from(".git");
        assert!(!searcher.is_search_dir(&path));

        let path = String::from("./temp/");
        assert!(!searcher.is_search_dir(&path));
    }

    #[test]
    fn test_is_search_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(searcher.is_search_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(searcher.is_search_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.is_search_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.is_search_file(&file));

        let path = String::from("./");
        let filename = String::from(".codefile.ts");
        let file = SearchFile::new(path, filename, FileType::Code);
        assert!(!searcher.is_search_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(!searcher.is_search_file(&file));
    }

    #[test]
    fn test_is_archive_search_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_archive_file_pattern(String::from("temp"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(searcher.is_archive_search_file(&file));

        let path = String::from(".");
        let filename = String::from(".archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(!searcher.is_archive_search_file(&file));

        let path = String::from("./temp");
        let filename = String::from("archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(!searcher.is_archive_search_file(&file));

        let path = String::from(".");
        let filename = String::from("temp_archive.zip");
        let file = SearchFile::new(path, filename, FileType::Archive);
        assert!(!searcher.is_archive_search_file(&file));
    }

    #[test]
    fn test_search_text_lines() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from(".");
        settings.add_search_pattern(String::from("Searcher"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: std::str::Lines = contents.lines();

        let results = searcher.search_text_lines(&mut lines);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 0);
        assert_eq!(results[0].lines_after.len(), 0);
        assert_eq!(results[1].line_num, 35);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 0);
        assert_eq!(results[1].lines_after.len(), 0);
    }

    #[test]
    fn test_search_text_lines_lines_after_to_until() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from(".");
        settings.add_search_pattern(String::from("Searcher"));
        settings.add_lines_after_to_pattern("after".to_string());
        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: std::str::Lines = contents.lines();

        let results = searcher.search_text_lines(&mut lines);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_search_text_contents() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from(".");
        settings.add_search_pattern(String::from("Searcher"));

        settings.lines_before = 2;
        settings.lines_after = 2;

        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = searcher.search_multiline_string(&contents);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 2);
        assert_eq!(results[0].lines_after.len(), 2);
        assert_eq!(results[1].line_num, 35);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 2);
        assert_eq!(results[1].lines_after.len(), 2);
    }

    #[test]
    fn test_search_text_contents_lines_after_to_until() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from(".");
        settings.add_search_pattern(String::from("Searcher"));

        settings.add_lines_after_to_pattern("after".to_string());

        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = searcher.search_multiline_string(&contents);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_search_code_files() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from("/Users/cary/src/xsearch/rust");
        settings.add_in_extension(String::from("go,rs"));
        settings.add_search_pattern(String::from("Searcher"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let results = searcher.search();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
    }

    #[test]
    fn test_search_helloworld_file() {
        let mut settings = SearchSettings::default();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/helloworld.txt");

        settings.startpath = String::from(testfile_path.to_str().unwrap());
        settings.add_search_pattern(String::from("Hello"));
        settings.add_search_pattern(String::from("你好"));
        //settings.text_file_encoding = String::from("windows-1252");
        let searcher = Searcher::new(settings).ok().unwrap();

        let results = searcher.search();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
    }

    #[test]
    fn test_search_binary_files() {
        let mut settings = SearchSettings::default();
        settings.startpath = String::from("/Users/cary/src/xsearch/java");
        settings.add_in_extension(String::from("class"));
        settings.add_search_pattern(String::from("Searcher"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let results = searcher.search();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
        if !results.is_empty() {
            assert_eq!(results[0].pattern, "Searcher");
            assert!(results[0].file.is_some());
            assert_eq!(results[0].line, "");
            assert_eq!(results[0].line_num, 0);
        }
    }
}
