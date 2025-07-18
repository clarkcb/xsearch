use crate::searchresult::SearchResult;
use crate::searchsettings::SearchSettings;

use rsfind::fileresultformatter::{colorize, FileResultFormatter};

pub struct SearchResultFormatter {
    pub settings: SearchSettings,
    pub file_formatter: FileResultFormatter,
    pub format_line: Box<dyn Fn(&str, &SearchSettings) -> String>,
}

const SEPARATOR_LEN: usize = 80;

fn format_line_with_color(line: &str, settings: &SearchSettings) -> String {
    let mut formatted_line = String::from(line);
    for p in settings.search_patterns() {
        let m = p.find(&line);
        if m.is_some() {
            formatted_line = colorize(&formatted_line, m.unwrap().start(),
                                      m.unwrap().end());
            break;
        }
    }
    formatted_line
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
        Self {
            settings,
            file_formatter,
            format_line: Box::new(_format_line),
        }
    }

    fn format_matching_line(&self, result: &SearchResult) -> String {
        let whitespace_chars = " \t\n\r";
        let mut leading_ws_count = 0;
        let mut formatted = result.line.clone();
        while whitespace_chars.contains(formatted.chars().next().unwrap()) {
            formatted = String::from(&formatted[1..]);
            leading_ws_count += 1;
        }
        while whitespace_chars.contains(formatted.chars().rev().next().unwrap()) {
            formatted = String::from(&formatted[..(formatted.len() - 1)]);
        }

        let mut formatted_length = formatted.len();
        let max_line_end_index = formatted_length - 1;
        let match_length = result.match_end_index - result.match_start_index;
        let mut match_start_index = result.match_start_index - 1 - leading_ws_count;
        let mut match_end_index = match_start_index + match_length;

        let max_line_length = self.settings.max_line_length() as usize;
        if formatted_length > max_line_length {
            let mut line_start_index = match_start_index + 0;
            let mut line_end_index = line_start_index + match_length;
            match_start_index = 0;
            match_end_index = match_length + 0;

            while line_end_index > formatted_length - 1 {
                line_start_index -= 1;
                line_end_index -= 1;
                match_start_index += 1;
                match_end_index += 1;
            }

            formatted_length = line_end_index - line_start_index;
            while formatted_length < max_line_length {
                if line_start_index > 0 {
                    line_start_index -= 1;
                    match_start_index += 1;
                    match_end_index += 1;
                    formatted_length = line_end_index - line_start_index;
                }
                if formatted_length < max_line_length && line_end_index < max_line_end_index {
                    line_end_index += 1;
                }
                formatted_length = line_end_index - line_start_index;
            }

            let mut before = String::from("");
            let mut after = String::from("");
            if line_start_index > 2 {
                before = String::from("...");
                line_start_index += 3;
            }
            if line_end_index < max_line_end_index - 3 {
                after = String::from("...");
                line_end_index -= 3;
            }

            formatted = String::from(format!("{}{}{}",
                                             String::from(before),
                                             String::from(&formatted[line_start_index..line_end_index]),
                                             String::from(after)));
        }

        if self.settings.colorize() {
            formatted = colorize(&formatted, match_start_index, match_end_index);
        }
        formatted
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
                        self.format_matching_line(result)
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
                            result.match_end_index - 1);
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
