use std::fmt;
use regex::Regex;

use rsfind::findsettings::FindSettings;

#[derive(Clone)]
pub struct SearchSettings {
    _find_settings: FindSettings,
    _colorize: bool,
    _first_match: bool,
    _in_lines_after_patterns: Vec<Regex>,
    _in_lines_before_patterns: Vec<Regex>,
    _lines_after: i32,
    _lines_after_to_patterns: Vec<Regex>,
    _lines_after_until_patterns: Vec<Regex>,
    _lines_before: i32,
    _max_line_length: i32,
    _multi_line_search: bool,
    _out_lines_after_patterns: Vec<Regex>,
    _out_lines_before_patterns: Vec<Regex>,
    _print_lines: bool,
    _print_results: bool,
    _search_archives: bool,
    _search_patterns: Vec<Regex>,
    _text_file_encoding: String,
    _unique_lines: bool,
}

impl SearchSettings {
    pub fn default() -> SearchSettings {
        SearchSettings {
            _find_settings: FindSettings::default(),
            _colorize: true,
            _first_match: false,
            _in_lines_after_patterns: Vec::new(),
            _in_lines_before_patterns: Vec::new(),
            _lines_after: 0i32,
            _lines_after_to_patterns: Vec::new(),
            _lines_after_until_patterns: Vec::new(),
            _lines_before: 0i32,
            _max_line_length: 150i32,
            _multi_line_search: false,
            _out_lines_after_patterns: Vec::new(),
            _out_lines_before_patterns: Vec::new(),
            _print_lines: false,
            _print_results: false,
            _search_archives: false,
            _search_patterns: Vec::new(),
            _text_file_encoding: String::from("utf-8"),
            _unique_lines: false,
        }
    }

    pub fn find_settings(&self) -> FindSettings {
        self._find_settings.clone()
    }


    pub fn archives_only(&self) -> bool {
        self._find_settings.archives_only()
    }

    pub fn set_archives_only(&mut self, b: bool) {
        self._find_settings.set_archives_only(b);
        if b {
            self._search_archives = b;
        }
    }

    pub fn colorize(&self) -> bool {
        self._colorize
    }

    pub fn set_colorize(&mut self, b: bool) {
        self._colorize = b
    }

    pub fn debug(&self) -> bool {
        self._find_settings.debug()
    }

    pub fn set_debug(&mut self, b: bool) {
        self._find_settings.set_debug(b);
    }

    pub fn first_match(&self) -> bool {
        self._first_match
    }

    pub fn set_first_match(&mut self, b: bool) {
        self._first_match = b
    }

    pub fn follow_symlinks(&self) -> bool {
        self._find_settings.follow_symlinks()
    }

    pub fn set_follow_symlinks(&mut self, b: bool) {
        self._find_settings.set_follow_symlinks(b);
    }

    pub fn in_archive_extensions(&self) -> &Vec<String> {
        &self._find_settings.in_archive_extensions()
    }

    pub fn add_in_archive_extension(&mut self, ext: String) {
        self._find_settings.add_in_archive_extension(ext)
    }

    pub fn in_archive_file_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.in_archive_file_patterns()
    }

    pub fn add_in_archive_file_pattern(&mut self, pattern: String) {
        self._find_settings.add_in_archive_file_pattern(pattern)
    }

    pub fn include_hidden(&self) -> bool {
        self._find_settings.include_hidden()
    }

    pub fn set_include_hidden(&mut self, b: bool) {
        self._find_settings.set_include_hidden(b)
    }

    pub fn in_dir_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.in_dir_patterns()
    }

    pub fn add_in_dir_pattern(&mut self, pattern: String) {
        self._find_settings.add_in_dir_pattern(pattern)
    }

    pub fn in_extensions(&self) -> &Vec<String> {
        &self._find_settings.in_extensions()
    }

    pub fn add_in_extension(&mut self, ext: String) {
        self._find_settings.add_in_extension(ext)
    }

    pub fn in_file_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.in_file_patterns()
    }

    pub fn add_in_file_pattern(&mut self, pattern: String) {
        self._find_settings.add_in_file_pattern(pattern)
    }

    pub fn in_file_types(&self) -> &Vec<rsfind::filetypes::FileType> {
        &self._find_settings.in_file_types()
    }

    pub fn add_in_file_type(&mut self, file_type: rsfind::filetypes::FileType) {
        self._find_settings.add_in_file_type(file_type)
    }

    pub fn in_lines_after_patterns(&self) -> &Vec<Regex> {
        &self._in_lines_after_patterns
    }

    pub fn add_in_lines_after_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._in_lines_after_patterns);
    }

    pub fn in_lines_before_patterns(&self) -> &Vec<Regex> {
        &self._in_lines_before_patterns
    }

    pub fn add_in_lines_before_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._in_lines_before_patterns);
    }

    pub fn include_archives(&self) -> bool {
        self._find_settings.include_archives()
    }

    pub fn set_include_archives(&mut self, b: bool) {
        self._find_settings.set_include_archives(b)
    }

    pub fn lines_after(&self) -> i32 {
        self._lines_after
    }

    pub fn set_lines_after(&mut self, i: i32) {
        self._lines_after = i
    }

    pub fn lines_after_to_patterns(&self) -> &Vec<Regex> {
        &self._lines_after_to_patterns
    }

    pub fn add_lines_after_to_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._lines_after_to_patterns);
    }

    pub fn lines_after_until_patterns(&self) -> &Vec<Regex> {
        &self._lines_after_until_patterns
    }

    pub fn add_lines_after_until_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._lines_after_until_patterns);
    }

    pub fn lines_before(&self) -> i32 {
        self._lines_before
    }

    pub fn set_lines_before(&mut self, i: i32) {
        self._lines_before = i
    }

    pub fn max_depth(&self) -> i32 {
        self._find_settings.max_depth()
    }

    pub fn set_max_depth(&mut self, m: i32) {
        self._find_settings.set_max_depth(m)
    }

    pub fn max_last_mod(&self) -> u64 {
        self._find_settings.max_last_mod()
    }

    pub fn set_max_last_mod(&mut self, u: u64) {
        self._find_settings.set_max_last_mod(u)
    }

    pub fn max_line_length(&self) -> i32 {
        self._max_line_length
    }

    pub fn set_max_line_length(&mut self, i: i32) {
        self._max_line_length = i
    }

    pub fn max_size(&self) -> u64 {
        self._find_settings.max_size()
    }

    pub fn set_max_size(&mut self, u: u64) {
        self._find_settings.set_max_size(u)
    }

    pub fn min_depth(&self) -> i32 {
        self._find_settings.min_depth()
    }

    pub fn set_min_depth(&mut self, m: i32) {
        self._find_settings.set_min_depth(m)
    }

    pub fn min_last_mod(&self) -> u64 {
        self._find_settings.min_last_mod()
    }

    pub fn set_min_last_mod(&mut self, u: u64) {
        self._find_settings.set_min_last_mod(u)
    }

    pub fn min_size(&self) -> u64 {
        self._find_settings.min_size()
    }

    pub fn set_min_size(&mut self, u: u64) {
        self._find_settings.set_min_size(u)
    }

    pub fn multi_line_search(&self) -> bool {
        self._multi_line_search
    }

    pub fn set_multi_line_search(&mut self, b: bool) {
        self._multi_line_search = b
    }

    pub fn out_archive_extensions(&self) -> &Vec<String> {
        &self._find_settings.out_archive_extensions()
    }

    pub fn add_out_archive_extension(&mut self, ext: String) {
        self._find_settings.add_out_archive_extension(ext)
    }

    pub fn out_archive_file_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.out_archive_file_patterns()
    }

    pub fn add_out_archive_file_pattern(&mut self, pattern: String) {
        self._find_settings.add_out_archive_file_pattern(pattern)
    }

    pub fn out_dir_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.out_dir_patterns()
    }

    pub fn add_out_dir_pattern(&mut self, pattern: String) {
        self._find_settings.add_out_dir_pattern(pattern)
    }

    pub fn out_extensions(&self) -> &Vec<String> {
        &self._find_settings.out_extensions()
    }

    pub fn add_out_extension(&mut self, ext: String) {
        self._find_settings.add_out_extension(ext)
    }

    pub fn out_file_patterns(&self) -> &Vec<Regex> {
        &self._find_settings.out_file_patterns()
    }

    pub fn add_out_file_pattern(&mut self, pattern: String) {
        self._find_settings.add_out_file_pattern(pattern)
    }

    pub fn out_file_types(&self) -> &Vec<rsfind::filetypes::FileType> {
        &self._find_settings.out_file_types()
    }

    pub fn add_out_file_type(&mut self, file_type: rsfind::filetypes::FileType) {
        self._find_settings.add_out_file_type(file_type)
    }

    pub fn out_lines_after_patterns(&self) -> &Vec<Regex> {
        &self._out_lines_after_patterns
    }

    pub fn add_out_lines_after_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._out_lines_after_patterns);
    }

    pub fn out_lines_before_patterns(&self) -> &Vec<Regex> {
        &self._out_lines_before_patterns
    }

    pub fn add_out_lines_before_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._out_lines_before_patterns);
    }

    pub fn paths(&self) -> &Vec<String> {
        &self._find_settings.paths()
    }

    pub fn add_path(&mut self, path: String) {
        self._find_settings.add_path(path)
    }

    pub fn print_dirs(&self) -> bool {
        self._find_settings.print_dirs()
    }

    pub fn set_print_dirs(&mut self, b: bool) {
        self._find_settings.set_print_dirs(b)
    }

    pub fn print_files(&self) -> bool {
        self._find_settings.print_files()
    }

    pub fn set_print_files(&mut self, b: bool) {
        self._find_settings.set_print_files(b)
    }

    pub fn print_lines(&self) -> bool {
        self._print_lines
    }

    pub fn set_print_lines(&mut self, b: bool) {
        self._print_lines = b
    }

    pub fn print_results(&self) -> bool {
        self._print_results
    }

    pub fn set_print_results(&mut self, b: bool) {
        self._print_results = b
    }

    pub fn print_usage(&self) -> bool {
        self._find_settings.print_usage()
    }

    pub fn set_print_usage(&mut self, b: bool) {
        self._find_settings.set_print_usage(b)
    }

    pub fn print_version(&self) -> bool {
        self._find_settings.print_version()
    }

    pub fn set_print_version(&mut self, b: bool) {
        self._find_settings.set_print_version(b)
    }

    pub fn recursive(&self) -> bool {
        self._find_settings.recursive()
    }

    pub fn set_recursive(&mut self, b: bool) {
        self._find_settings.set_recursive(b)
    }

    pub fn search_archives(&self) -> bool {
        self._search_archives
    }

    pub fn set_search_archives(&mut self, b: bool) {
        self._search_archives = b
    }

    pub fn search_patterns(&self) -> &Vec<Regex> {
        &self._search_patterns
    }

    pub fn add_search_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._search_patterns);
    }

    pub fn sort_by(&self) -> rsfind::sortby::SortBy {
        self._find_settings.sort_by()
    }

    pub fn set_sort_by(&mut self, s: rsfind::sortby::SortBy) {
        self._find_settings.set_sort_by(s)
    }

    pub fn sort_case_insensitive(&self) -> bool {
        self._find_settings.sort_case_insensitive()
    }

    pub fn set_sort_case_insensitive(&mut self, b: bool) {
        self._find_settings.set_sort_case_insensitive(b)
    }

    pub fn sort_descending(&self) -> bool {
        self._find_settings.sort_descending()
    }

    pub fn set_sort_descending(&mut self, b: bool) {
        self._find_settings.set_sort_descending(b)
    }

    pub fn text_file_encoding(&self) -> &String {
        &self._text_file_encoding
    }

    pub fn set_text_file_encoding(&mut self, s: String) {
        self._text_file_encoding = s
    }

    pub fn unique_lines(&self) -> bool {
        self._unique_lines
    }

    pub fn set_unique_lines(&mut self, b: bool) {
        self._unique_lines = b
    }

    pub fn verbose(&self) -> bool {
        self._find_settings.verbose()
    }

    pub fn set_verbose(&mut self, b: bool) {
        self._find_settings.set_verbose(b)
    }

    fn get_settings_string(&self) -> String {
        let mut s = String::from("SearchSettings(");
        s.push_str(format!("archives_only={}", &self.archives_only()).as_str());
        s.push_str(format!(", colorize={}", &self.colorize()).as_str());
        s.push_str(format!(", debug={}", &self.debug()).as_str());
        s.push_str(format!(", first_match={}", &self.first_match()).as_str());
        s.push_str(format!(", follow_symlinks={}", &self.follow_symlinks()).as_str());
        s.push_str(format!(", in_archive_extensions={:?}", &self.in_archive_extensions()).as_str());
        s.push_str(format!(", in_archive_file_patterns={}", get_regex_vec_string(&self.in_archive_file_patterns())).as_str());
        s.push_str(format!(", in_dir_patterns={}", get_regex_vec_string(&self.in_dir_patterns())).as_str());
        s.push_str(format!(", in_extensions={:?}", &self.in_extensions()).as_str());
        s.push_str(format!(", in_file_patterns={}", get_regex_vec_string(&self.in_file_patterns())).as_str());
        s.push_str(format!(", in_file_types={:?}", &self.in_file_types()).as_str());
        s.push_str(format!(", in_lines_after_patterns={}", get_regex_vec_string(&self.in_lines_after_patterns())).as_str());
        s.push_str(format!(", in_lines_before_patterns={}", get_regex_vec_string(&self.in_lines_before_patterns())).as_str());
        s.push_str(format!(", include_archives={:?}", &self.include_archives()).as_str());
        s.push_str(format!(", include_hidden={:?}", &self.include_hidden()).as_str());
        s.push_str(format!(", lines_after={:?}", &self.lines_after()).as_str());
        s.push_str(format!(", lines_after_to_patterns={}", get_regex_vec_string(&self.lines_after_to_patterns())).as_str());
        s.push_str(format!(", lines_after_until_patterns={}", get_regex_vec_string(&self.lines_after_until_patterns())).as_str());
        s.push_str(format!(", lines_before={:?}", &self.lines_before()).as_str());
        s.push_str(format!(", max_depth={:?}", &self.max_depth()).as_str());
        s.push_str(format!(", max_last_mod={:?}", &self.max_last_mod()).as_str());
        s.push_str(format!(", max_line_length={:?}", &self.max_line_length()).as_str());
        s.push_str(format!(", max_size={:?}", &self.max_size()).as_str());
        s.push_str(format!(", min_depth={:?}", &self.min_depth()).as_str());
        s.push_str(format!(", min_last_mod={:?}", &self.min_last_mod()).as_str());
        s.push_str(format!(", min_size={:?}", &self.min_size()).as_str());
        s.push_str(format!(", multi_line_search={:?}", &self.multi_line_search()).as_str());
        s.push_str(format!(", out_archive_extensions={:?}", &self.out_archive_extensions()).as_str());
        s.push_str(format!(", out_archive_file_patterns={}", get_regex_vec_string(&self.out_archive_file_patterns())).as_str());
        s.push_str(format!(", out_dir_patterns={}", get_regex_vec_string(&self.out_dir_patterns())).as_str());
        s.push_str(format!(", out_extensions={:?}", &self.out_extensions()).as_str());
        s.push_str(format!(", out_file_patterns={}", get_regex_vec_string(&self.out_file_patterns())).as_str());
        s.push_str(format!(", out_file_types={:?}", &self.out_file_types()).as_str());
        s.push_str(format!(", out_lines_after_patterns={}", get_regex_vec_string(&self.out_lines_after_patterns())).as_str());
        s.push_str(format!(", out_lines_before_patterns={}", get_regex_vec_string(&self.out_lines_before_patterns())).as_str());
        s.push_str(format!(", paths={:?}", &self.paths()).as_str());
        s.push_str(format!(", print_dirs={:?}", &self.print_dirs()).as_str());
        s.push_str(format!(", print_files={:?}", &self.print_files()).as_str());
        s.push_str(format!(", print_lines={:?}", &self.print_lines()).as_str());
        s.push_str(format!(", print_results={:?}", &self.print_results()).as_str());
        s.push_str(format!(", print_usage={:?}", &self.print_usage()).as_str());
        s.push_str(format!(", print_version={:?}", &self.print_version()).as_str());
        s.push_str(format!(", recursive={}", &self.recursive()).as_str());
        s.push_str(format!(", search_archives={:?}", &self.search_archives()).as_str());
        s.push_str(format!(", search_patterns={}", get_regex_vec_string(&self.search_patterns())).as_str());
        s.push_str(format!(", sort_by={:?}", &self.sort_by()).as_str());
        s.push_str(format!(", sort_case_insensitive={}", &self.sort_case_insensitive()).as_str());
        s.push_str(format!(", sort_descending={}", &self.sort_descending()).as_str());
        s.push_str(format!(", text_file_encoding={}", &self.text_file_encoding()).as_str());
        s.push_str(format!(", unique_lines={}", &self.unique_lines()).as_str());
        s.push_str(format!(", verbose={}", &self.verbose()).as_str());
        s.push_str(")");
        s
    }
}

impl fmt::Debug for SearchSettings {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_settings_string())
    }
}

pub fn add_pattern(pattern: String, patterns: &mut Vec<Regex>) {
    patterns.push(Regex::new(pattern.as_str()).unwrap());
}

fn get_regex_vec_string(vec: &Vec<Regex>) -> String {
    let patterns: Vec<String> = vec.iter().map(|r| r.to_string()).collect();
    let mut s = String::from("[");
    let mut i = 0;
    for pattern in patterns.iter() {
        if i > 0 {
            s.push_str(", ");
        }
        s.push_str(format!("{:?}", pattern).as_str());
        i += 1;
    }
    s.push_str("]");
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let settings = SearchSettings::default();
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.colorize(), true);
        assert_eq!(settings.debug(), false);
        assert_eq!(settings.first_match(), false);
        assert_eq!(settings.follow_symlinks(), false);
        assert!(settings.in_archive_extensions().is_empty());
        assert!(settings.in_archive_file_patterns().is_empty());
        assert_eq!(settings.include_hidden(), false);
        assert!(settings.in_dir_patterns().is_empty());
        assert!(settings.in_extensions().is_empty());
        assert!(settings.in_file_patterns().is_empty());
        assert!(settings.in_file_types().is_empty());
        assert_eq!(settings.lines_after(), 0);
        assert!(settings.lines_after_to_patterns().is_empty());
        assert!(settings.lines_after_until_patterns().is_empty());
        assert_eq!(settings.lines_before(), 0);
        assert_eq!(settings.max_depth(), -1);
        assert_eq!(settings.max_last_mod(), 0);
        assert_eq!(settings.max_line_length(), 150);
        assert_eq!(settings.max_size(), 0);
        assert_eq!(settings.min_depth(), -1);
        assert_eq!(settings.min_last_mod(), 0);
        assert_eq!(settings.min_size(), 0);
        assert_eq!(settings.multi_line_search(), false);
        assert!(settings.out_archive_extensions().is_empty());
        assert!(settings.out_archive_file_patterns().is_empty());
        assert!(settings.out_dir_patterns().is_empty());
        assert!(settings.out_extensions().is_empty());
        assert!(settings.out_file_patterns().is_empty());
        assert!(settings.out_file_types().is_empty());
        assert!(settings.paths().is_empty());
        assert_eq!(settings.print_dirs(), false);
        assert_eq!(settings.print_files(), false);
        assert_eq!(settings.print_lines(), false);
        assert_eq!(settings.print_results(), false);
        assert_eq!(settings.print_usage(), false);
        assert_eq!(settings.print_version(), false);
        assert_eq!(settings.recursive(), true);
        assert_eq!(settings.search_archives(), false);
        assert!(settings.search_patterns().is_empty());
        assert_eq!(settings.sort_by(), rsfind::sortby::SortBy::FilePath);
        assert_eq!(settings.sort_case_insensitive(), false);
        assert_eq!(settings.sort_descending(), false);
        assert_eq!(settings.text_file_encoding(), &String::from("utf-8"));
        assert_eq!(settings.unique_lines(), false);
        assert_eq!(settings.verbose(), false);
    }

    #[test]
    fn test_set_archives_only() {
        let mut settings = SearchSettings::default();
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.search_archives(), false);
        settings.set_archives_only(true);
        assert_eq!(settings.archives_only(), true);
        assert_eq!(settings.search_archives(), true);
        settings.set_archives_only(false);
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.search_archives(), true);
    }

    #[test]
    fn test_set_debug() {
        let mut settings = SearchSettings::default();
        assert_eq!(settings.debug(), false);
        assert_eq!(settings.verbose(), false);
        settings.set_debug(true);
        assert_eq!(settings.debug(), true);
        assert_eq!(settings.verbose(), true);
        settings.set_debug(false);
        assert_eq!(settings.debug(), false);
        assert_eq!(settings.verbose(), true);
    }

    #[test]
    fn test_add_extensions() {
        let mut settings = SearchSettings::default();

        settings.add_in_extension("c".to_string());
        assert_eq!(settings.in_extensions().len(), 1);
        assert_eq!(settings.in_extensions()[0], "c".to_string());

        settings.add_in_extension("cpp,hs,js".to_string());
        assert_eq!(settings.in_extensions().len(), 4);
        assert_eq!(settings.in_extensions()[1], "cpp".to_string());
        assert_eq!(settings.in_extensions()[2], "hs".to_string());
        assert_eq!(settings.in_extensions()[3], "js".to_string());

        settings.add_in_extension("rs,".to_string());
        assert_eq!(settings.in_extensions().len(), 5);
        assert_eq!(settings.in_extensions()[4], "rs".to_string());

        settings.add_in_extension(",ts".to_string());
        assert_eq!(settings.in_extensions().len(), 6);
        assert_eq!(settings.in_extensions()[5], "ts".to_string());
    }

    #[test]
    fn test_add_pattern() {
        let mut settings = SearchSettings::default();

        settings.add_in_dir_pattern("src".to_string());
        assert_eq!(settings.in_dir_patterns().len(), 1);
        assert_eq!(settings.in_dir_patterns()[0].to_string(), "src".to_string());

        settings.add_out_dir_pattern("temp".to_string());
        assert_eq!(settings.out_dir_patterns().len(), 1);
        assert_eq!(settings.out_dir_patterns()[0].to_string(), "temp".to_string());

        settings.add_in_file_pattern("search".to_string());
        assert_eq!(settings.in_file_patterns().len(), 1);
        assert_eq!(
            settings.in_file_patterns()[0].to_string(),
            "search".to_string()
        );

        settings.add_out_file_pattern("tempfile".to_string());
        assert_eq!(settings.out_file_patterns().len(), 1);
        assert_eq!(
            settings.out_file_patterns()[0].to_string(),
            "tempfile".to_string()
        );
    }
}
