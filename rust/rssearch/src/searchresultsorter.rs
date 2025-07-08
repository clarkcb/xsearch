use std::cmp::Ordering;
use crate::searchresult::SearchResult;
use crate::searchsettings::SearchSettings;
use rsfind::sortby::SortBy;

pub struct SearchResultSorter {
    pub settings: SearchSettings,
}

fn cmp_by_search_fields(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    if sr1.line_num == sr2.line_num {
        if sr1.match_start_index == sr2.match_start_index {
            return sr1.match_end_index.cmp(&sr2.match_end_index);
        }
        return sr1.match_start_index.cmp(&sr2.match_start_index);
    }
    sr1.line_num.cmp(&sr2.line_num)
}

fn cmp_by_path(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_path(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_path_ci(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_path_ci(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_name(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_name(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_name_ci(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_name_ci(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_size(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_size(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_size_ci(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_size_ci(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_type(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_type(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_type_ci(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_type_ci(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_last_mod(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_last_mod(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

fn cmp_by_last_mod_ci(sr1: &SearchResult, sr2: &SearchResult) -> Ordering {
    match (&sr1.file, &sr2.file) {
        (Some(f1), Some(f2)) => match rsfind::fileresultsorter::cmp_by_last_mod_ci(f1, f2) {
            Ordering::Equal => cmp_by_search_fields(sr1, sr2),
            other => other,
        },
        _ => cmp_by_search_fields(sr1, sr2),
    }
}

impl SearchResultSorter {
    pub fn new(settings: SearchSettings) -> SearchResultSorter {
        Self {
            settings,
        }
    }

    pub fn get_search_result_comparator(&self) -> impl Fn(&SearchResult, &SearchResult) -> Ordering + use<> {
        match (self.settings.sort_by(), self.settings.sort_case_insensitive(), self.settings.sort_descending()) {
            (SortBy::FileName, false, false) => cmp_by_name,
            (SortBy::FileName, false, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_name(fr2, fr1),
            (SortBy::FileName, true, false) => cmp_by_name_ci,
            (SortBy::FileName, true, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_name_ci(fr2, fr1),
            (SortBy::FilePath, false, false) => cmp_by_path,
            (SortBy::FilePath, false, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_path(fr2, fr1),
            (SortBy::FilePath, true, false) => cmp_by_path_ci,
            (SortBy::FilePath, true, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_path_ci(fr2, fr1),
            (SortBy::FileSize, false, false) => cmp_by_size,
            (SortBy::FileSize, false, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_size(fr2, fr1),
            (SortBy::FileSize, true, false) => cmp_by_size_ci,
            (SortBy::FileSize, true, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_size_ci(fr2, fr1),
            (SortBy::FileType, false, false) => cmp_by_type,
            (SortBy::FileType, false, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_type(fr2, fr1),
            (SortBy::FileType, true, false) => cmp_by_type_ci,
            (SortBy::FileType, true, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_type_ci(fr2, fr1),
            (SortBy::LastMod, false, false) => cmp_by_last_mod,
            (SortBy::LastMod, false, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_last_mod(fr2, fr1),
            (SortBy::LastMod, true, false) => cmp_by_last_mod_ci,
            (SortBy::LastMod, true, true) => |fr1: &SearchResult, fr2: &SearchResult| cmp_by_last_mod_ci(fr2, fr1),
        }
    }

    pub fn sort(&self, search_results: &mut Vec<SearchResult>) {
        let search_result_comparator = self.get_search_result_comparator();
        search_results.sort_by(search_result_comparator);
    }
}
