#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>

#include "SearchException.h"
#include "Searcher.h"
#include "cppfind.h"

namespace cppsearch {
    Searcher::Searcher(const SearchSettings& settings)  try : m_finder(cppfind::Finder(settings)),  m_search_settings(settings)
    {
        validate_settings(settings);
    } catch (const cppfind::FindException& e) {
        throw SearchException(e.what());
    } catch (const std::bad_alloc& e) {
        throw SearchException(e.what());
    }

    Searcher::Searcher(const std::unique_ptr<SearchSettings>& settings_ptr)  try : m_finder(cppfind::Finder(*settings_ptr)),  m_search_settings(*settings_ptr)
    {
        validate_settings(m_search_settings);
    } catch (const cppfind::FindException& e) {
        throw SearchException(e.what());
    } catch (const std::bad_alloc& e) {
        throw SearchException(e.what());
    }

    void Searcher::validate_settings(const SearchSettings& settings) {
        if (settings.search_patterns().empty()) {
            throw SearchException("No search patterns defined");
        }
    }

    std::vector<SearchFileResult> Searcher::search() const {
        std::vector<SearchFileResult> results{};

        try {
            const std::vector<cppfind::FileResult> file_results = m_finder.find();
            results = search_files(file_results);
        } catch (const cppfind::FindException& e) {
            throw SearchException(e.what());
        }

        return results;
    }

    bool matches_any_pattern(const std::string_view s,
                             const std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash>& patterns) {
        const std::string ss{s};
        return std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const cppfind::RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss,
                                 const std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash>& patterns) {
        return std::ranges::any_of(ss.begin(), ss.end(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    std::vector<SearchFileResult> Searcher::search_files(const std::vector<cppfind::FileResult>& files) const {
        std::vector<SearchFileResult> results{};
        for (auto& fr : files) {
            std::vector<SearchFileResult> fr_results = search_file(fr);
            results.insert(results.end(), fr_results.begin(), fr_results.end());
        }
        return results;
    }

    std::vector<SearchFileResult> Searcher::search_file(const cppfind::FileResult& fr) const {
        std::vector<SearchFileResult> results{};
        if (fr.file_type() == cppfind::FileType::CODE
            || fr.file_type() == cppfind::FileType::XML
            || fr.file_type() == cppfind::FileType::TEXT) {
            results = search_text_file(fr);
        } else if (fr.file_type() == cppfind::FileType::BINARY) {
            results = search_binary_file(fr);
        } else if (fr.file_type() == cppfind::FileType::ARCHIVE) {
            //cout << "m_file is an ARCHIVE file: " << fr->file_name() << endl;
            if (m_search_settings.search_archives()) {
                // TODO: search archive
            }
        } else {
            std::cout << "m_file is an UNKNOWN file: " << fr.file_path() << std::endl;
        }
        return results;
    }

    std::vector<SearchFileResult> Searcher::search_text_file(const cppfind::FileResult& fr) const {
        if (m_search_settings.debug()) {
            std::cout << "Searching text file " << fr.file_path() << std::endl;
        }
        std::ifstream fin(fr.string());
        std::vector<SearchTextResult> text_results = search_ifstream(fin);
        fin.close();

        std::vector<SearchFileResult> file_results;
        auto text_to_file = [&fr](SearchTextResult& r) {
            return SearchFileResult(fr, std::move(r));
        };
        std::ranges::transform(text_results.begin(), text_results.end(), file_results.begin(),
            text_to_file);
        return file_results;
    }

    std::vector<SearchTextResult> Searcher::search_ifstream(std::ifstream& fin) const {
        if (m_search_settings.multi_line_search()) {
            return search_ifstream_contents(fin);
        }
        return search_ifstream_lines(fin);
    }

    bool lines_match(const std::vector<std::string>& lines,
                     const std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash>& in_patterns,
                     const std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash>& out_patterns) {
        return lines.empty() ||
               ((in_patterns.empty() || any_matches_any_pattern(lines, in_patterns)) &&
                (out_patterns.empty() || !any_matches_any_pattern(lines, out_patterns)));
    }

    std::vector<SearchTextResult> Searcher::search_ifstream_lines(std::ifstream& fin) const {
        std::vector<SearchTextResult> results{};

        for (const auto& p : m_search_settings.search_patterns()) {

            fin.seekg(0);

            bool found_pattern = false;

            int line_num = 0;
            std::string line;

            std::deque<std::string> lines_before;
            std::deque<std::string> lines_after;
            unsigned int lines_before_count = m_search_settings.lines_before();
            unsigned int lines_after_count = m_search_settings.lines_after();

            while (true) {
                if (m_search_settings.first_match() && found_pattern) {
                    break;
                }

                ++line_num;
                if (!lines_after.empty()) {
                    line = lines_after.front();
                    lines_after.pop_front();
                } else if (getline(fin, line)) {
                    // nothing to do, action in if clause
                } else {
                    break;
                }

                if (lines_after_count > 0) {
                    std::string next_line;
                    while (lines_after.size() < lines_after_count && getline(fin, next_line)) {
                        lines_after.push_back(next_line);
                    }
                }

                std::regex r = p.regex();
                auto matches_begin = std::sregex_iterator(line.begin(), line.end(), r);
                auto matches_end = std::sregex_iterator();

                for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {
                    if (m_search_settings.first_match() && found_pattern) {
                        break;
                    }

                    const std::smatch& match = *it;

                    unsigned long match_start_idx = match.position(0);
                    unsigned long match_end_idx = match_start_idx + match.length(0);
                    auto v_lines_before = std::vector<std::string>(lines_before.begin(), lines_before.end());
                    if (!lines_match(v_lines_before, m_search_settings.in_lines_before_patterns(),
                                     m_search_settings.out_lines_before_patterns())) {
                        continue;
                    }
                    auto v_lines_after = std::vector<std::string>(lines_after.begin(), lines_after.end());
                    if (!lines_match(v_lines_after, m_search_settings.in_lines_after_patterns(),
                                     m_search_settings.out_lines_after_patterns())) {
                        continue;
                    }

                    results.emplace_back(p, line_num, match_start_idx + 1,
                                         match_end_idx + 1, line,
                                         v_lines_before, v_lines_after);

                    if (m_search_settings.first_match()) {
                        found_pattern = true;
                        break;
                    }
                }

                if (lines_before_count > 0) {
                    if (lines_before.size() == lines_before_count) {
                        lines_before.pop_front();
                    }
                    if (lines_before.size() < lines_before_count) {
                        lines_before.push_back(line);
                    }
                }
            }
        }
        return results;
    }

    std::string get_contents(std::ifstream& fin) {
        std::string s;
        if (fin) {
            std::ostringstream ss;
            ss << fin.rdbuf(); // reading data
            s = ss.str();
        }
        return s;
    }

    std::vector<SearchTextResult> Searcher::search_ifstream_contents(std::ifstream& fin) const {
        // auto contents = cppfind::FileUtil::get_contents(fin);
        const auto contents = get_contents(fin);
        return search_multiline_string(contents);
    }

    std::vector<unsigned long> get_newline_indices(const std::string& s) {
        std::vector<unsigned long> newline_indices{};
        for (unsigned long i=0; i < s.length(); ++i) {
            if (s.at(i) == '\n') {
                newline_indices.push_back(i);
            }
        }
        return newline_indices;
    }

    unsigned long get_line_num_for_pos(const std::vector<unsigned long>& newline_indices, const unsigned long pos) {
        long i = 0;
        while (newline_indices[i] < pos) i++;
        return ++i;
    }

    std::pair<unsigned long, unsigned long> get_line_start_end_for_pos(const std::vector<unsigned long>& newline_indices,
                                                                       const unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        unsigned long line_start_idx = newline_indices[i-1] + 1;
        unsigned long line_end_idx = newline_indices[i];
        return std::make_pair(line_start_idx, line_end_idx);
    }

    std::string get_line_for_pos(std::string& s, std::vector<unsigned long>& newline_indices, unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        const unsigned long line_start_idx = newline_indices[i-1] + 1;
        const unsigned long line_len = newline_indices[i] - newline_indices[i-1] - 1;
        return s.substr(line_start_idx, line_len);
    }

    std::vector<std::string> get_lines_before_pos(const std::string& s,
                                                  const std::vector<unsigned long>& newline_indices,
                                                  unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        --i;
        while (i > 0 && line_count > 0) {
            std::pair<unsigned long, unsigned long> start_end =
                    get_line_start_end_for_pos(newline_indices, newline_indices[i - 1]);
            unsigned long line_start_idx = start_end.first;
            unsigned long line_end_idx = start_end.second;
            lines.insert(lines.begin(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            --i;
            --line_count;
        }
        return lines;
    }

    std::vector<std::string> get_lines_after_pos(const std::string& s,
                                                 const std::vector<unsigned long>& newline_indices,
                                                 unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        size_t i = 0;
        while (newline_indices[i] <= pos) i++;
        while (i < newline_indices.size() && line_count > 0) {
            auto [fst, snd] =
                    get_line_start_end_for_pos(newline_indices, newline_indices[i]);
            const unsigned long line_start_idx = fst;
            const unsigned long line_end_idx = snd;
            lines.insert(lines.end(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            ++i;
            --line_count;
        }
        return lines;
    }

    std::vector<SearchTextResult> Searcher::search_multiline_string(const std::string& s) const {
        std::vector<SearchTextResult> results{};

        // get newline, startline and endline indices
        std::vector<unsigned long> newline_indices = get_newline_indices(s);
        std::vector<unsigned long> line_start_indices = {0};
        auto plus_one = [](unsigned long num) {return num + 1;};
        std::ranges::transform(newline_indices.begin(), newline_indices.end(), std::back_inserter(line_start_indices), plus_one);

        for (const auto& p : m_search_settings.search_patterns()) {
            // ---------------------------------------------------------------------
            std::regex r = p.regex();
            auto matches_begin = std::sregex_iterator(s.begin(), s.end(), r);
            auto matches_end = std::sregex_iterator();

            for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {

                const std::smatch& match = *it;

                unsigned long match_start_idx = match.position(0);
                unsigned long match_end_idx = match_start_idx + match.length(0);

                unsigned long line_num = get_line_num_for_pos(newline_indices, match_start_idx);
                std::pair<unsigned long, unsigned long> startend =
                        get_line_start_end_for_pos(newline_indices, match_start_idx);
                unsigned long line_start_idx = startend.first;
                unsigned long line_end_idx = startend.second;

                std::string line = s.substr(line_start_idx, line_end_idx - line_start_idx);

                std::vector<std::string> lines_before;
                if (m_search_settings.lines_before() > 0) {
                    lines_before = get_lines_before_pos(s, newline_indices, m_search_settings.lines_before(),
                                                        line_start_idx);
                    if (!lines_match(lines_before, m_search_settings.in_lines_before_patterns(),
                                     m_search_settings.out_lines_before_patterns())) {
                        continue;
                    }
                }
                std::vector<std::string> lines_after;
                if (m_search_settings.lines_after() > 0) {
                    lines_after = get_lines_after_pos(s, newline_indices, m_search_settings.lines_after(),
                                                       line_start_idx);
                    if (!lines_match(lines_after, m_search_settings.in_lines_after_patterns(),
                                     m_search_settings.out_lines_after_patterns())) {
                        continue;
                    }
                }

                results.emplace_back(p, line_num,
                                     match_start_idx - line_start_idx + 1,
                                     match_end_idx - line_start_idx + 1,
                                     line, lines_before, lines_after);

                if (m_search_settings.first_match()) {
                    break;
                }
            }
            // ---------------------------------------------------------------------
        }

        return results;
    }

    std::vector<SearchFileResult> Searcher::search_binary_file(const cppfind::FileResult& fr) const {
        std::vector<SearchFileResult> results{};
        std::unordered_set<std::string> found_patterns{};

        std::ifstream fin(fr.string());
        // std::string s = cppfind::FileUtil::get_contents(fin);
        auto s = get_contents(fin);
        fin.close();

        std::smatch pmatch;
        for (const auto& p : m_search_settings.search_patterns()) {
            if (m_search_settings.first_match() && found_patterns.contains(p.pattern())) {
                continue;
            }
            unsigned long trimmed = 0;
            std::string sbuf = std::string(s);
            bool skip_pattern = false;
            while (!skip_pattern && !sbuf.empty() && regex_search(sbuf, pmatch, p.regex())) {
                for (unsigned i=0; i < pmatch.size(); ++i) {
                    unsigned long match_start_idx = pmatch.position(i) + trimmed;
                    unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                    std::string line;
                    results.emplace_back(fr, SearchTextResult(p, 0, match_start_idx, match_end_idx, line));
                    if (m_search_settings.first_match()) {
                        found_patterns.insert(p.pattern());
                        skip_pattern = true;
                        break;
                    }
                    sbuf = s.substr(match_end_idx);
                    trimmed = match_end_idx;
                }
            }
        }

        return results;
    }
}
