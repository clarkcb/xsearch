use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs;
use std::io;
// use std::io::BufReader;
use std::io::Read;
// use std::path::Path;
use std::str::Lines;

use encoding::codec::singlebyte::SingleByteEncoding;
use encoding::{label, DecoderTrap, Encoding};
use regex::{Match, Regex};
use rsfind::fileresult::FileResult;
use rsfind::filetypes::FileType;
use rsfind::fileutil::FileUtil;
// use zip::read::ZipFile;

use crate::common::log;
use crate::searcherror::SearchError;
use crate::searchresult::SearchResult;
use crate::searchsettings::SearchSettings;

const BINARY_ENCODING: &SingleByteEncoding = encoding::all::ISO_8859_1;

pub struct Searcher {
    pub finder: rsfind::finder::Finder,
    pub settings: SearchSettings,
}

impl Searcher {
    /// Create a new Searcher instance for the given settings, if valid
    pub fn new(settings: SearchSettings) -> Result<Searcher, SearchError> {
        let finder = match rsfind::finder::Finder::new(settings.find_settings()) {
            Ok(finder) => finder,
            Err(error) => return Err(SearchError::new(error.description.as_str())),
        };

        if let Err(error) = Searcher::validate_settings(&settings) {
            return Err(error);
        }

        Ok(Searcher {
            finder,
            settings,
        })
    }

    fn validate_settings(settings: &SearchSettings) -> Result<(), SearchError> {
        if settings.search_patterns().is_empty() {
            return Err(SearchError::new("No search patterns defined"));
        }
        if let None = label::encoding_from_whatwg_label(&settings.text_file_encoding()) {
            return Err(SearchError::new(
                "Invalid or unsupported text file encoding",
            ));
        }
        if settings.lines_after() < 0 {
            return Err(SearchError::new("Invalid linesafter"));
        }
        if settings.lines_before() < 0 {
            return Err(SearchError::new("Invalid linesbefore"));
        }
        Ok(())
    }

    fn get_text_file_encoding(&self) -> &'static dyn Encoding {
        label::encoding_from_whatwg_label(&self.settings.text_file_encoding()).unwrap()
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

    /// Initiate a searcher search for the given settings and get the results
    pub fn search(&self) -> Result<Vec<SearchResult>, SearchError> {
        let mut file_results: Vec<FileResult> = Vec::new();

        match self.finder.find() {
            Ok(mut find_results) => {
                file_results.append(&mut find_results);
            }
            Err(error) => return Err(SearchError::new(error.description.as_str())),
        }

        if self.settings.verbose() && !file_results.is_empty() {
            let mut dirs: Vec<String> = file_results.iter().map(|f| String::from(f.parent())).collect();
            dirs.sort_unstable();
            dirs.dedup();
            log(format!("\nDirectories to be searched ({}):", dirs.len()).as_str());
            for dir in dirs.iter() {
                log(format!("{}", dir).as_str());
            }

            let files: Vec<String> = file_results.iter().map(|f| f.file_path()).collect();
            log(format!("\nFiles to be searched ({}):", files.len()).as_str());
            for file in files.iter() {
                log(format!("{}", file).as_str());
            }
        }

        self.search_files(file_results)
    }

    fn search_files(&self, files: Vec<FileResult>) -> Result<Vec<SearchResult>, SearchError> {
        let mut results: Vec<SearchResult> = Vec::new();
        for fr in files.into_iter() {
            match self.search_file(fr) {
                Ok(mut file_results) => {
                    results.append(&mut file_results);
                },
                Err(error) => {
                    log(format!("{}", error).as_str());
                    return Err(error);
                }
            }
        }
        // TODO: implement custom sorting of [SearchResult]
        results.sort_unstable();
        Ok(results)
    }

    /// Search an individual file and get the results
    pub fn search_file(&self, file: FileResult) -> Result<Vec<SearchResult>, SearchError> {
        match file.file_type {
            FileType::Text | FileType::Code | FileType::Xml => self.search_text_file(file),
            FileType::Binary => self.search_binary_file(file),
            FileType::Archive => {
                if self.settings.search_archives() {
                    self.search_archive_file(file)
                } else {
                    log(format!("Skipping archive file: {}", file.file_path()).as_str());
                    Ok(vec![])
                }
            },
            _ => {
                log(format!("Skipping unknown file type: {:?}", file.file_type).as_str());
                Ok(vec![])
            }
        }
    }

    fn search_archive_file(
        &self,
        file: FileResult,
    ) -> Result<Vec<SearchResult>, SearchError> {
        if self.settings.verbose() {
            log(format!("Searching archive file {}", file.file_path()).as_str());
        }

        match FileUtil::get_extension(&file.file_name()) {
            // TODO: what other extensions are zip format?
            Some(ext) if ["zip", "zipx", "jar", "war", "ear", "whl"].contains(&ext) => {
                // self.search_archive_zip_file(file)
                log(format!("Searching zip archives files is currently disabled").as_str());
                Ok(vec![])
            },
            Some(ext) => {
                log(format!("Searching not currently supported for {} files", ext).as_str());
                Ok(vec![])
            },
            None => {
                log(format!(
                    "Skipping unknown archive file of unknown type: {}",
                    file.file_path()
                )
                .as_str());
                Ok(vec![])
            }
        }
    }

    // TODO: enable after implementing archive file "finding" in rsfind
    // fn search_archive_zip_file(
    //     &self,
    //     file: &FileResult,
    // ) -> Result<Vec<SearchResult>, SearchError> {
    //     if self.settings.verbose() {
    //         log(format!("Searching zip file {}", file.file_path()).as_str());
    //     }
    //     let mut results: Vec<SearchResult> = Vec::new();
    //     match fs::File::open(file.file_path()) {
    //         Ok(f) => {
    //             let reader = BufReader::new(f);
    //             let mut archive = match zip::ZipArchive::new(reader) {
    //                 Ok(archive) => archive,
    //                 Err(error) => return Err(SearchError::new(&error.to_string())),
    //             };
    //             for i in 0..archive.len() {
    //                 let mut zipfile = match archive.by_index(i) {
    //                     Ok(zipfile) => zipfile,
    //                     Err(error) => return Err(SearchError::new(&error.to_string())),
    //                 };
    //                 if zipfile.is_file() {
    //                     match self.search_zip_file(file, &mut zipfile) {
    //                         Ok(mut zip_results) => results.append(&mut zip_results),
    //                         Err(error) => return Err(error),
    //                     }
    //                 }
    //             }
    //         },
    //         Err(error) => {
    //             return Err(SearchError::new(&error.to_string()));
    //         }
    //     }
    //     Ok(results)
    // }
    //
    // fn search_zip_file(
    //     &self,
    //     file: &FileResult,
    //     zipfile: &mut ZipFile,
    // ) -> Result<Vec<SearchResult>, SearchError> {
    //     let path = if let Some(path) = Path::new(zipfile.name()).parent() {
    //         path.to_str().unwrap()
    //     } else {
    //         ""
    //     };
    //     let filename = Path::new(zipfile.name())
    //         .file_name()
    //         .unwrap()
    //         .to_str()
    //         .unwrap();
    //     let mut containers = vec![];
    //     for c in file.containers.iter() {
    //         containers.push(c.clone());
    //     }
    //     containers.push(file.file_path());
    //     let zip_file_result = FileResult::with_containers(
    //         containers,
    //         path.to_string(),
    //         filename.to_string(),
    //         self.filetypes.get_file_type(filename),
    //         0,
    //         0
    //     );
    //     if self.is_search_file(&zip_file_result) {
    //         match zip_file_result.file_type {
    //             FileType::Text | FileType::Code | FileType::Xml => {
    //                 let encoding = self.get_text_file_encoding();
    //                 let zip_contents = match self.get_text_reader_contents(zipfile, encoding) {
    //                     Ok(bytestring) => bytestring,
    //                     Err(error) => return Err(error),
    //                 };
    //
    //                 let results = if self.settings.multi_line_search() {
    //                     self.search_multi_line_string(&zip_contents)
    //                 } else {
    //                     let mut lines = zip_contents.lines();
    //                     self.search_text_lines(&mut lines)
    //                 };
    //                 return Ok(results
    //                     .iter()
    //                     .map(|r| {
    //                         SearchResult::new(
    //                             r.pattern.clone(),
    //                             Some(zip_file_result.clone()),
    //                             r.line_num,
    //                             r.match_start_index,
    //                             r.match_end_index,
    //                             r.line.clone(),
    //                             r.lines_before.clone(),
    //                             r.lines_after.clone(),
    //                         )
    //                     })
    //                     .collect());
    //             },
    //             FileType::Binary => match self.get_byte_string_for_reader(zipfile) {
    //                 Ok(bytestring) => match self.search_binary_byte_string(&bytestring) {
    //                     Ok(results) => {
    //                         return Ok(results
    //                             .iter()
    //                             .map(|r| {
    //                                 SearchResult::new(
    //                                     r.pattern.clone(),
    //                                     Some(zip_file_result.clone()),
    //                                     0,
    //                                     r.match_start_index,
    //                                     r.match_end_index,
    //                                     "".to_string(),
    //                                     Vec::new(),
    //                                     Vec::new(),
    //                                 )
    //                             })
    //                             .collect())
    //                     }
    //                     Err(error) => return Err(error),
    //                 },
    //                 Err(error) => return Err(SearchError::new(&error.to_string())),
    //             },
    //             _ => {}
    //         }
    //     }
    //     Ok(vec![])
    // }

    fn get_encoded_byte_string_for_reader(
        &self,
        reader: &mut dyn Read,
        enc: &dyn Encoding,
    ) -> Result<String, SearchError> {
        let mut buffer: Vec<u8> = Vec::new();
        match reader.read_to_end(&mut buffer) {
            Ok(_) => match enc.decode(&buffer, DecoderTrap::Strict) {
                Ok(bytestring) => Ok(bytestring),
                Err(cow) => return Err(SearchError::new(&cow.to_string())),
            },
            Err(error) => {
                return Err(SearchError::new(&error.to_string()));
            }
        }
    }

    fn get_encoded_byte_string(
        &self,
        file: &FileResult,
        enc: &dyn Encoding,
    ) -> Result<String, SearchError> {
        match fs::File::open(file.file_path()) {
            Ok(mut f) => self.get_encoded_byte_string_for_reader(&mut f, enc),
            Err(error) => Err(SearchError::new(&error.to_string())),
        }
    }

    fn get_byte_string_for_reader(&self, reader: &mut dyn Read) -> Result<String, SearchError> {
        self.get_encoded_byte_string_for_reader(reader, BINARY_ENCODING)
    }

    fn get_byte_string(&self, file: &FileResult) -> Result<String, SearchError> {
        self.get_encoded_byte_string(file, BINARY_ENCODING)
    }

    fn search_binary_file(
        &self,
        file: FileResult,
    ) -> Result<Vec<SearchResult>, SearchError> {
        if self.settings.verbose() {
            log(format!("Searching binary file {}", file.file_path()).as_str());
        }
        let mut results: Vec<SearchResult> = Vec::new();
        match self.get_byte_string(&file) {
            Ok(byte_string) => match self.search_binary_byte_string(&byte_string) {
                Ok(rs) => {
                    for r in rs.into_iter() {
                        results.push(SearchResult::new(
                            r.pattern.clone(),
                            Some(file.clone()),
                            0,
                            r.match_start_index,
                            r.match_end_index,
                            "".to_string(),
                            Vec::new(),
                            Vec::new(),
                        ));
                    }
                },
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
        for p in self.settings.search_patterns() {
            let matches: Vec<Match> = if self.settings.first_match() {
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

    /// Try to get the reader contents for the given encoding (UTF-8 by default)
    fn get_text_reader_contents(
        &self,
        reader: &mut dyn Read,
        encoding: &'static dyn Encoding,
    ) -> Result<String, SearchError> {
        let mut into_string = String::from("");
        let contents = match encoding.name() {
            "utf-8" => match reader.read_to_string(&mut into_string) {
                Ok(_) => into_string,
                Err(error) if error.kind() == io::ErrorKind::InvalidData => {
                    match self.get_byte_string_for_reader(reader) {
                        Ok(bytestring) => bytestring,
                        Err(error) => return Err(error),
                    }
                },
                Err(error) => {
                    //let msg = format!("{} (file: {}", error.description(), searchfile.file_path());
                    return Err(SearchError::new(&error.to_string()));
                }
            },
            _ => match self.get_encoded_byte_string_for_reader(reader, encoding) {
                Ok(bytestring) => bytestring,
                Err(error) => return Err(error),
            },
        };
        Ok(contents)
    }

    /// Try to get the file contents for the given encoding (UTF-8 by default)
    fn get_text_file_contents(
        &self,
        file: &FileResult,
        encoding: &'static dyn Encoding,
    ) -> Result<String, SearchError> {
        match fs::File::open(file.file_path()) {
            Ok(mut f) => self.get_text_reader_contents(&mut f, encoding),
            Err(error) => Err(SearchError::new(&error.to_string())),
        }
    }

    fn search_text_file(&self, file: FileResult) -> Result<Vec<SearchResult>, SearchError> {
        let encoding = self.get_text_file_encoding();
        if self.settings.verbose() {
            log(format!("Searching text file {}", file.file_path()).as_str());
        }
        if self.settings.multi_line_search() {
            return self.search_text_file_contents(file, encoding);
        }
        return self.search_text_file_lines(file, encoding);
    }

    fn search_text_file_lines<'a>(
        &self,
        // file: &'a FileResult,
        file: FileResult,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<SearchResult>, SearchError> {
        let contents = match self.get_text_file_contents(&file, encoding) {
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
                    Some(file.clone()),
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
            while lines_after.len() < self.settings.lines_after() as usize {
                match lines.next() {
                    Some(l) => lines_after.push_back(l),
                    None => break,
                }
            }
            for p in self.settings.search_patterns() {
                for m in p.find_iter(line) {
                    let mut v_lines_before: Vec<String> = Vec::new();
                    if !lines_before.is_empty() {
                        for line_before in lines_before.iter() {
                            v_lines_before.push(line_before.to_string());
                        }
                        if !self.lines_match(
                            &v_lines_before,
                            &self.settings.in_lines_before_patterns(),
                            &self.settings.out_lines_before_patterns(),
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
                            &self.settings.in_lines_after_patterns(),
                            &self.settings.out_lines_after_patterns(),
                        ) {
                            continue;
                        }
                    }

                    let has_lines_after_to_patterns =
                        !self.settings.lines_after_to_patterns().is_empty();
                    let has_lines_after_until_patterns =
                        !self.settings.lines_after_until_patterns().is_empty();
                    if has_lines_after_to_patterns || has_lines_after_until_patterns {
                        let lines_after_patterns = if has_lines_after_to_patterns {
                            self.settings.lines_after_to_patterns()
                        } else {
                            self.settings.lines_after_until_patterns()
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
                                },
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
                    if self.settings.first_match()
                        && matched_patterns.len() == self.settings.search_patterns().len()
                    {
                        return results;
                    }
                }
            }
            if self.settings.lines_before() > 0 {
                if lines_before.len() == self.settings.lines_before() as usize {
                    lines_before.pop_front();
                }
                if lines_before.len() < self.settings.lines_before() as usize {
                    lines_before.push_back(line);
                }
            }
            current_linenum += 1;
        }
        results
    }

    fn search_text_file_contents(
        &self,
        file: FileResult,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<SearchResult>, SearchError> {
        let contents = match self.get_text_file_contents(&file, encoding) {
            Ok(contents) => contents,
            Err(error) => return Err(error),
        };
        Ok(self
            .search_multi_line_string(&contents)
            .iter()
            .map(|r| {
                SearchResult::new(
                    r.pattern.clone(),
                    Some(file.clone()),
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

    /// Search a file's contents as a multi-line string
    pub fn search_multi_line_string<'a>(&self, contents: &str) -> Vec<SearchResult> {
        let mut results: Vec<SearchResult> = Vec::new();

        let newline_indices: Vec<usize> = contents.match_indices("\n").map(|i| i.0).collect();
        let mut startline_indices: Vec<usize> = vec![0];
        startline_indices.append(&mut newline_indices.iter().map(|n| n + 1).collect());

        for p in self.settings.search_patterns() {
            let matches: Vec<Match> = if self.settings.first_match() {
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
                let lines_before: Vec<String> = if self.settings.lines_before() > 0 {
                    let nlis: &[usize] = &newline_indices[..lines_before_count - 1];
                    let nlis: Vec<usize> = nlis
                        .iter()
                        .rev()
                        .take(self.settings.lines_before() as usize + 1)
                        .rev()
                        .cloned()
                        .collect();
                    self.get_lines_from_contents(contents, nlis)
                } else {
                    vec![]
                };
                if !self.lines_match(
                    &lines_before,
                    &self.settings.in_lines_before_patterns(),
                    &self.settings.out_lines_before_patterns(),
                ) {
                    continue;
                }
                let lines_after: Vec<String> = if (self.settings.lines_after() > 0
                    || !self.settings.lines_after_to_patterns().is_empty()
                    || !self.settings.lines_after_until_patterns().is_empty())
                    && newline_indices.len() > lines_before_count
                {
                    let nlis: &[usize] = &newline_indices[lines_before_count - 1..];
                    let nlis: Vec<usize> = if self.settings.lines_after() > 0 {
                        nlis.iter()
                            .take(self.settings.lines_after() as usize + 1)
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

                                if !self.settings.lines_after_to_patterns().is_empty() {
                                    nlis.iter().take(_nlis.len() + 1).cloned().collect()
                                } else {
                                    _nlis
                                }
                            },
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
                        &self.settings.in_lines_after_patterns(),
                        &self.settings.out_lines_after_patterns(),
                    ) {
                        continue;
                    }
                } else if !self.settings.lines_after_to_patterns().is_empty()
                    || !self.settings.lines_after_until_patterns().is_empty()
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
pub fn get_result_dirs(results: &[SearchResult]) -> Vec<String> {
    let mut dirs: Vec<String> = Vec::new();
    for r in results.iter() {
        if let Some(f) = &r.file {
            dirs.push(String::from(f.parent()));
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
            let filepath = f.file_path();
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

    use super::*;

    #[test]
    fn test_search_text_lines() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("."));
        settings.add_search_pattern(String::from("Searcher"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: Lines = contents.lines();

        let results = searcher.search_text_lines(&mut lines);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 30);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 0);
        assert_eq!(results[0].lines_after.len(), 0);
        assert_eq!(results[1].line_num, 36);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 0);
        assert_eq!(results[1].lines_after.len(), 0);
    }

    #[test]
    fn test_search_text_lines_lines_after_to_until() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("."));
        settings.add_search_pattern(String::from("Searcher"));
        settings.add_lines_after_to_pattern("after".to_string());
        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: Lines = contents.lines();

        let results = searcher.search_text_lines(&mut lines);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 30);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_search_text_contents() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("."));
        settings.add_search_pattern(String::from("Searcher"));

        settings.set_lines_before(2);
        settings.set_lines_after(2);

        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = searcher.search_multi_line_string(&contents);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 30);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 2);
        assert_eq!(results[0].lines_after.len(), 2);
        assert_eq!(results[1].line_num, 36);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 2);
        assert_eq!(results[1].lines_after.len(), 2);
    }

    #[test]
    fn test_search_text_contents_lines_after_to_until() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("."));
        settings.add_search_pattern(String::from("Searcher"));

        settings.add_lines_after_to_pattern("after".to_string());

        let searcher = Searcher::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = searcher.search_multi_line_string(&contents);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 30);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_search_code_files() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("/Users/cary/src/xsearch/rust"));
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
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/helloworld_utf8.txt");

        if testfile_path.exists() {
            settings.add_path(String::from(testfile_path.to_str().unwrap()));
            settings.add_search_pattern(String::from("Hello"));
            settings.add_search_pattern(String::from("你好"));
            //settings.text_file_encoding = String::from("windows-1252");
            let searcher = Searcher::new(settings).ok().unwrap();

            let results = searcher.search();
            assert!(results.is_ok());
            let results = results.ok().unwrap();
            println!("results: {}", results.len());
        }
    }

    #[test]
    fn test_search_binary_files() {
        let mut settings = SearchSettings::default();
        settings.add_path(String::from("/Users/cary/src/xsearch/java"));
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

    #[test]
    fn test_search_zip_file() {
        let mut settings = SearchSettings::default();
        let path = Path::new("../../shared/testFiles.zip");
        let startpath = if path.exists() {
            String::from("../../shared/testFiles.zip")
        } else {
            String::from("../../shared")
        };
        settings.add_path(startpath);
        settings.set_search_archives(true);
        settings.add_search_pattern(String::from("Searcher"));
        let searcher = Searcher::new(settings).ok().unwrap();

        let results = searcher.search();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
        if !results.is_empty() {
            assert_eq!(results[0].pattern, "Searcher");
            assert!(results[0].file.is_some());
            //assert_eq!(results[0].line, "");
            assert_eq!(results[0].line_num, 8);
        }
    }
}
