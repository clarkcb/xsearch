use std::cmp::Ordering;
use std::path::Path;

use crate::filetypes::FileType;

#[derive(Clone, Debug, Eq)]
pub struct SearchFile {
    pub containers: Vec<String>,
    pub path: String,
    pub name: String,
    pub filetype: FileType,
}

impl SearchFile {
    pub fn new(path: String, name: String, filetype: FileType) -> SearchFile {
        SearchFile::with_containers(Vec::new(), path, name, filetype)
    }

    pub fn with_containers(
        containers: Vec<String>,
        path: String,
        name: String,
        filetype: FileType,
    ) -> SearchFile {
        SearchFile {
            containers: containers,
            path: path,
            name: name,
            filetype: filetype,
        }
    }

    pub fn filepath(&self) -> String {
        format!("{}", Path::new(&self.path).join(&self.name).display())
    }
}

impl Ord for SearchFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.filepath().cmp(&other.filepath())
    }
}

impl PartialOrd for SearchFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for SearchFile {
    fn eq(&self, other: &Self) -> bool {
        self.filepath() == other.filepath()
    }
}

#[cfg(test)]
mod tests {
    use crate::filetypes::FileType;

    use super::*;

    #[test]
    fn test_search_file_abs_path() {
        let sf = SearchFile::new(
            "/Users/cary/src/xsearch/rust/rssearch/src".to_string(),
            "searcher.rs".to_string(),
            FileType::Code,
        );
        assert_eq!(
            sf.filepath(),
            "/Users/cary/src/xsearch/rust/rssearch/src/searcher.rs"
        );
    }

    #[test]
    fn test_search_file_rel_path() {
        let sf = SearchFile::new(".".to_string(), "searcher.rs".to_string(), FileType::Code);
        assert_eq!(sf.filepath(), "./searcher.rs");
    }
}
