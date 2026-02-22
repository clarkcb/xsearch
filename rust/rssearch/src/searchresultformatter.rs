use crate::searchresult::SearchResult;
use crate::searchsettings::SearchSettings;

use rsfind::fileresultformatter::{colorize, FileResultFormatter};

pub struct SearchResultFormatter {
    pub settings: SearchSettings,
    pub file_formatter: FileResultFormatter,
    pub format_line: Box<dyn Fn(&str, &SearchSettings) -> String>,
    pub format_match: Box<dyn Fn(&str, &SearchSettings) -> String>,
}

const SEPARATOR_LEN: usize = 80;

fn format_line_with_color(line: &str, settings: &SearchSettings) -> String {
    let mut formatted_line = String::from(line);
    for p in settings.search_patterns() {
        let m = p.find(&line);
        if m.is_some() {
            formatted_line = colorize(&formatted_line, m.unwrap().start(),
                                      m.unwrap().end(), &settings.line_color());
            break;
        }
    }
    formatted_line
}

fn format_match_with_color(m: &str, settings: &SearchSettings) -> String {
    colorize(&m, 0, m.len(), &settings.line_color())
}

fn blank_or_whitespace(s: &str) -> bool {
    for (_index, character) in s.char_indices() {
        if !character.is_whitespace() {
            return false;
        }
    }
    true
}

impl SearchResultFormatter {
    pub fn new(settings: SearchSettings) -> SearchResultFormatter {
        let file_formatter = FileResultFormatter::new(settings.find_settings());
        let _format_line: fn(&str, &SearchSettings) -> String =
            if settings.colorize() {
                |line: &str, settings: &SearchSettings| format_line_with_color(&line, &settings)
            } else {
                |line: &str, _settings: &SearchSettings| String::from(line)
            };
        let _format_match: fn(&str, &SearchSettings) -> String =
            if settings.colorize() {
                |m: &str, settings: &SearchSettings| format_match_with_color(&m, &settings)
            } else {
                |m: &str, _settings: &SearchSettings| String::from(m)
            };
        Self {
            settings,
            file_formatter,
            format_line: Box::new(_format_line),
            format_match: Box::new(_format_match),
        }
    }

    fn format_result_match(&self, result: &SearchResult) -> String {
        if blank_or_whitespace(&result.line) || self.settings.max_line_length() == 0 {
            String::from("")
        } else {
            let match_start_idx = result.match_start_index - 1usize;
            let match_end_idx = result.match_end_index - 1usize;
            let match_length = match_end_idx - match_start_idx;
            if match_length <= self.settings.max_line_length() as usize {
                let match_string = String::from(&result.line[match_start_idx..match_end_idx]);
                if self.settings.colorize() {
                    colorize(&match_string, 0, match_string.len(), &self.settings.line_color())
                } else {
                    match_string
                }
            } else {
                let prefix = if match_start_idx > 2 { "..." } else { "" };
                let suffix = "...";
                let color_start_idx = prefix.len();
                let color_end_idx = (self.settings.max_line_length() as usize) - suffix.len();
                let match_end_idx = match_start_idx + color_end_idx;
                let match_start_idx = match_start_idx + color_start_idx;
                let match_string = String::from(format!("{}{}{}",
                                                        String::from(prefix),
                                                        String::from(&result.line[match_start_idx..match_end_idx]),
                                                        String::from(suffix)));

                if self.settings.colorize() {
                    colorize(&match_string, color_start_idx, color_end_idx, &self.settings.line_color())
                } else {
                    match_string
                }
            }
        }
    }

    fn format_result_line(&self, result: &SearchResult) -> String {
        if blank_or_whitespace(&result.line) || self.settings.max_line_length() == 0 {
            String::from("")
        } else if self.settings.max_line_length() > 0
            && result.match_end_index - result.match_start_index > self.settings.max_line_length() as usize {
            self.format_result_match(result)
        } else {
            let mut line_start_idx = 0usize;
            let mut line_end_idx = result.line.len() - 1usize;

            // get the index after leading whitespace
            for (_index, character) in result.line.char_indices() {
                if character.is_whitespace() {
                    line_start_idx += 1;
                } else {
                    break;
                }
            }

            // get the index before trailing whitespace
            for (_index, character) in result.line.char_indices().rev() {
                if character.is_whitespace() {
                    line_end_idx -= 1;
                } else {
                    break;
                }
            }

            let match_length = result.match_end_index - result.match_start_index;
            let mut match_start_idx = result.match_start_index - 1 - line_start_idx;
            let mut match_end_idx = match_start_idx + match_length;

            let mut prefix = "";
            let mut suffix = "";

            let trimmed_length = line_end_idx - line_start_idx;
            let max_line_length = self.settings.max_line_length() as usize;

            if self.settings.max_line_length() > 0 && trimmed_length > max_line_length {
                line_start_idx = result.match_start_index - 1usize;
                line_end_idx = line_start_idx + match_length;
                match_start_idx = 0;
                match_end_idx = match_length;

                let mut current_len = line_end_idx - line_start_idx;
                while current_len < max_line_length {
                    if line_start_idx > 0 {
                        line_start_idx -= 1;
                        match_start_idx += 1;
                        match_end_idx += 1;
                        current_len += 1;
                    }
                    if current_len < max_line_length && line_end_idx < trimmed_length {
                        line_end_idx += 1;
                        current_len += 1;
                    }
                }

                if line_start_idx > 2 {
                    prefix = "...";
                    line_start_idx += 3;
                }
                if line_end_idx < (trimmed_length - 3) {
                    suffix = "...";
                    line_end_idx -= 3;
                }
            } else {
                line_end_idx += 1;
            }

            let line = String::from(format!("{}{}{}",
                                            String::from(prefix),
                                            String::from(&result.line[line_start_idx..line_end_idx]),
                                            String::from(suffix)));

            if self.settings.colorize() {
                colorize(&line, match_start_idx, match_end_idx, &self.settings.line_color())
            } else {
                line
            }
        }
    }

    fn single_line_format(&self, result: &SearchResult) -> String {
        let file_prefix = match &result.file {
            Some(fr) => self.file_formatter.format_file_result(fr),
            None => "<text>".to_string(),
        };
        if result.line_num > 0 {
            return String::from(
                format!("{}: {}: [{}:{}]: {}",
                        file_prefix,
                        result.line_num,
                        result.match_start_index,
                        result.match_end_index,
                        self.format_result_line(result)
            ));
        }
        String::from(
            format!("{} matches at [{}:{}]",
                    file_prefix,
                    result.match_start_index,
                    result.match_end_index
        ))
    }

    fn line_num_padding(&self, result: &SearchResult) -> usize {
        format!("{}", result.line_num + result.lines_after.len()).len()
    }

    fn multi_line_format(&self, result: &SearchResult) -> String {
        let mut buffer = String::new();
        buffer.push_str("=".repeat(SEPARATOR_LEN).as_str());
        match &result.file {
            Some(fr) => {
                buffer.push_str(
                    format!(
                        "\n{}: {}: [{}:{}]\n",
                        self.file_formatter.format_file_result(fr),
                        result.line_num,
                        result.match_start_index,
                        result.match_end_index
                    )
                        .as_str(),
                );
            },
            None => {
                buffer.push_str(
                    format!(
                        "\n{}: [{}:{}]\n",
                        result.line_num, result.match_start_index, result.match_end_index
                    )
                        .as_str(),
                );
            }
        }
        buffer.push_str("-".repeat(SEPARATOR_LEN).as_str());
        buffer.push_str("\n");
        let linenum_padding = self.line_num_padding(result);
        let mut current_linenum = result.line_num;
        if !result.lines_before.is_empty() {
            current_linenum -= result.lines_before.len();
            for line_before in result.lines_before.iter() {
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
        let mut line = result.line.clone();
        if self.settings.colorize() {
            line = colorize(&line, result.match_start_index - 1,
                            result.match_end_index - 1, &self.settings.line_color());
        }
        let l = format!(
            "> {:linenum_padding$} | {}\n",
            current_linenum,
            line,
            linenum_padding = linenum_padding
        );
        buffer.push_str(l.as_str());
        if !result.lines_after.is_empty() {
            current_linenum += 1;
            for line_after in result.lines_after.iter() {
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
        buffer
    }

    pub fn format(&self, result: &SearchResult) -> String {
        if !result.lines_before.is_empty() || !result.lines_after.is_empty() {
            self.multi_line_format(result)
        } else {
            self.single_line_format(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::searchresultformatter::SearchResultFormatter;
    use std::path::Path;
    use rsfind::consolecolor::{CONSOLE_GREEN, CONSOLE_RESET};
    use rsfind::fileresult::FileResult;
    use rsfind::filetypes::FileType;
    use crate::searchsettings::SearchSettings;
    use super::*;

    #[test]
    fn test_single_line_search_result_longer_than_max_line_length() {
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

        let mut settings = SearchSettings::default();
        settings.set_colorize(false);
        settings.set_max_line_length(100);
        let formatter = SearchResultFormatter::new(settings);

        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_single_line_search_result_longer_than_max_line_length_colorize() {
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
        let expected_line =
            String::from(format!("...89012345678901234567890123456789012345678901{}maxlen{}89012345678901234567890123456789012345678901...",
                                 CONSOLE_GREEN,
                                 CONSOLE_RESET));
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            expected_line
        );

        let mut settings = SearchSettings::default();
        settings.set_colorize(true);
        settings.set_max_line_length(100);
        let formatter = SearchResultFormatter::new(settings);

        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_search_result_match_longer_than_max_line_length_colorize() {
        let pattern = String::from("\\d+maxlen\\d+");
        let file_path = Path::new("./maxlen.txt");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Text, 0, 0);
        let line_num = 1;
        let match_start_index = 1;
        let match_end_index = 110;
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
        let expected_line =
            String::from(format!("{}0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456{}...",
                                 CONSOLE_GREEN,
                                 CONSOLE_RESET));
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            expected_line
        );

        let mut settings = SearchSettings::default();
        settings.set_colorize(true);
        settings.set_max_line_length(100);
        let formatter = SearchResultFormatter::new(settings);

        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_search_result2_match_longer_than_max_line_length_colorize() {
        let pattern = String::from("\\d+maxlen\\d+");
        let file_path = Path::new("./maxlen.txt");
        let file = FileResult::new(file_path.to_path_buf(), FileType::Text, 0, 0);
        let line_num = 1;
        let match_start_index = 11;
        let match_end_index = 120;
        let line = String::from("ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ");
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
        let expected_line =
            String::from(format!("...{}3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456{}...",
                                 CONSOLE_GREEN,
                                 CONSOLE_RESET));
        let expected_output = format!(
            "{}: {}: [{}:{}]: {}",
            &file.file_path(),
            line_num,
            match_start_index,
            match_end_index,
            expected_line
        );

        let mut settings = SearchSettings::default();
        settings.set_colorize(true);
        settings.set_max_line_length(100);
        let formatter = SearchResultFormatter::new(settings);

        let output = formatter.format(&result);
        assert_eq!(output, expected_output);
    }
}
