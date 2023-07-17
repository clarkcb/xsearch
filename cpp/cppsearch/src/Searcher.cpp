#include <algorithm>
#include <fstream>
#include <iostream>
#include "SearchException.h"
#include "Searcher.h"
#include "cppfind.h"

namespace cppsearch {
    Searcher::Searcher(SearchSettings* settings) {
        try {
            m_finder = new cppfind::Finder(settings);
        } catch (const cppfind::FindException& e) {
            throw SearchException(e.what());
        }
        validate_settings(settings);
        m_settings = settings;
    }

    void Searcher::validate_settings(SearchSettings* ss) {
        if (ss->search_patterns()->empty()) {
            throw SearchException("No search patterns defined");
        }
    }

    std::vector<SearchResult*> Searcher::search() {
        std::vector<SearchResult*> results{};

        try {
            std::vector<cppfind::FileResult*> file_results = m_finder->find();
            results = search_files(file_results);
        } catch (const cppfind::FindException& e) {
            throw SearchException(e.what());
        }

        return results;
    }

    bool matches_any_pattern(const std::string& s, const std::vector<cppfind::RegexPattern*>& patterns) {
        std::smatch pmatch;
        for (auto& p : patterns) {
            if (regex_search(s, pmatch, p->r())) {
                return true;
            }
        }
        return false;
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss, const std::vector<cppfind::RegexPattern*>& patterns) {
        return std::any_of(ss.begin(), ss.end(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    std::vector<SearchResult*> Searcher::search_files(const std::vector<cppfind::FileResult*>& files) {
        std::vector<SearchResult*> results{};
        for (auto& fr : files) {
            std::vector<SearchResult*> fr_results = search_file(fr);
            results.insert(results.end(), fr_results.begin(), fr_results.end());
        }
        return results;
    }

    std::vector<SearchResult*> Searcher::search_file(cppfind::FileResult* fr) {
        std::vector<SearchResult*> results = {};
        if (fr->file_type() == cppfind::FileType::CODE || fr->file_type() == cppfind::FileType::XML || fr->file_type() == cppfind::FileType::TEXT) {
            results = search_text_file(fr);
        } else if (fr->file_type() == cppfind::FileType::BINARY) {
            results = search_binary_file(fr);
        } else if (fr->file_type() == cppfind::FileType::ARCHIVE) {
            //cout << "m_file is an ARCHIVE file: " << fr->file_name() << endl;
        } else {
            std::cout << "m_file is an UNKNOWN file: " << fr->file_name() << std::endl;
        }
        return results;
    }

    std::vector<SearchResult*> Searcher::search_text_file(cppfind::FileResult* fr) {
        if (m_settings->debug()) {
            std::cout << "Searching text file " << fr->string() << std::endl;
        }
        std::ifstream fin(fr->string());
        std::vector<SearchResult*> results = search_ifstream(fin);
        fin.close();

        for (const auto& r : results) {
            r->set_file(fr);
        }
        return results;
    }

    std::vector<SearchResult*> Searcher::search_ifstream(std::ifstream& fin) {
        if (m_settings->multi_line_search()) {
            return search_ifstream_contents(fin);
        } else {
            return search_ifstream_lines(fin);
        }
    }

    bool lines_match(std::vector<std::string>& lines, std::vector<cppfind::RegexPattern*>* in_patterns,
                     std::vector<cppfind::RegexPattern*>* out_patterns) {
        return lines.empty() ||
               ((in_patterns->empty() || any_matches_any_pattern(lines, *in_patterns)) &&
                (out_patterns->empty() || !any_matches_any_pattern(lines, *out_patterns)));
    }

    std::vector<SearchResult*> Searcher::search_ifstream_lines(std::ifstream& fin) {
        std::vector<SearchResult*> results = {};

        for (const auto& p : *(m_settings->search_patterns())) {

            fin.seekg(0);

            bool found_pattern = false;

            int line_num = 0;
            std::string line;

            std::deque<std::string> lines_before;
            std::deque<std::string> lines_after;
            unsigned int lines_before_count = m_settings->lines_before();
            unsigned int lines_after_count = m_settings->lines_after();

            while (true) {
                if (m_settings->first_match() && found_pattern) {
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

                std::regex r = p->r();
                auto matches_begin = std::sregex_iterator(line.begin(), line.end(), r);
                auto matches_end = std::sregex_iterator();

                for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {
                    if (m_settings->first_match() && found_pattern) {
                        break;
                    }

                    std::smatch match = *it;

                    unsigned long match_start_idx = match.position(0);
                    unsigned long match_end_idx = match_start_idx + match.length(0);
                    auto* v_lines_before = new std::vector<std::string>(lines_before.begin(), lines_before.end());
                    if (!lines_match(*v_lines_before, m_settings->in_lines_before_patterns(),
                                     m_settings->out_lines_before_patterns())) {
                        continue;
                    }
                    auto* v_lines_after = new std::vector<std::string>(lines_after.begin(), lines_after.end());
                    if (!lines_match(*v_lines_after, m_settings->in_lines_after_patterns(),
                                     m_settings->out_lines_after_patterns())) {
                        continue;
                    }

                    results.push_back(new SearchResult(p, nullptr, line_num,
                                                       match_start_idx + 1,
                                                       match_end_idx + 1,
                                                       line, v_lines_before, v_lines_after));

                    if (m_settings->first_match()) {
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

    std::vector<SearchResult*> Searcher::search_ifstream_contents(std::ifstream& fin) {
        std::string contents = cppfind::FileUtil::get_contents(fin);
        return search_multiline_string(contents);
    }

    std::vector<unsigned long> get_newline_indices(std::string& s) {
        std::vector<unsigned long> newline_indices = {};
        for (unsigned long i=0; i < s.length(); i++) {
            if (s.at(i) == '\n') {
                newline_indices.push_back(i);
            }
        }
        return newline_indices;
    }

    unsigned long get_line_num_for_pos(std::vector<unsigned long>& newline_indices, unsigned long pos) {
        long i = 0;
        while (newline_indices[i] < pos) i++;
        return ++i;
    }

    std::pair<unsigned long, unsigned long> get_line_start_end_for_pos(std::string& s,
                                                                       std::vector<unsigned long>& newline_indices,
                                                                       unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        unsigned long line_start_idx = newline_indices[i-1] + 1;
        unsigned long line_end_idx = newline_indices[i];
        return std::make_pair(line_start_idx, line_end_idx);
    }

    std::string get_line_for_pos(std::string& s, std::vector<unsigned long> newline_indices, unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        unsigned long line_start_idx = newline_indices[i-1] + 1;
        unsigned long line_len = newline_indices[i] - newline_indices[i-1] - 1;
        return s.substr(line_start_idx, line_len);
    }

    std::vector<std::string> get_lines_before_pos(std::string& s, std::vector<unsigned long> newline_indices,
                                                  unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        --i;
        while (i > 0 && line_count > 0) {
            std::pair<unsigned long, unsigned long> start_end =
                    get_line_start_end_for_pos(s, newline_indices, newline_indices[i - 1]);
            unsigned long line_start_idx = start_end.first;
            unsigned long line_end_idx = start_end.second;
            lines.insert(lines.begin(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            --i;
            --line_count;
        }
        return lines;
    }

    std::vector<std::string> get_lines_after_pos(std::string& s, std::vector<unsigned long> newline_indices,
                                                 unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        while (i < newline_indices.size() && line_count > 0) {
            std::pair<unsigned long, unsigned long> start_end =
                    get_line_start_end_for_pos(s, newline_indices, newline_indices[i]);
            unsigned long line_start_idx = start_end.first;
            unsigned long line_end_idx = start_end.second;
            lines.insert(lines.end(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            ++i;
            --line_count;
        }
        return lines;
    }

    std::vector<SearchResult*> Searcher::search_multiline_string(std::string& s) {
        std::vector<SearchResult*> results = {};

        // get newline, startline and endline indices
        std::vector<unsigned long> newline_indices = get_newline_indices(s);
        std::vector<unsigned long> line_start_indices = {0};
        auto plus_one = [](unsigned long num) {return num + 1;};
        std::transform(newline_indices.begin(), newline_indices.end(), std::back_inserter(line_start_indices), plus_one);

        for (const auto& p : *(m_settings->search_patterns())) {
            // ---------------------------------------------------------------------
            std::regex r = p->r();
            auto matches_begin = std::sregex_iterator(s.begin(), s.end(), r);
            auto matches_end = std::sregex_iterator();

            for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {

                std::smatch match = *it;

                unsigned long match_start_idx = match.position(0);
                unsigned long match_end_idx = match_start_idx + match.length(0);

                unsigned long line_num = get_line_num_for_pos(newline_indices, match_start_idx);
                std::pair<unsigned long, unsigned long> startend =
                        get_line_start_end_for_pos(s, newline_indices, match_start_idx);
                unsigned long line_start_idx = startend.first;
                unsigned long line_end_idx = startend.second;

                std::string line = s.substr(line_start_idx, line_end_idx - line_start_idx);

                std::vector<std::string> lines_before;
                if (m_settings->lines_before() > 0) {
                    lines_before = get_lines_before_pos(s, newline_indices, m_settings->lines_before(),
                                                        line_start_idx);
                    if (!lines_match(lines_before, m_settings->in_lines_before_patterns(),
                                     m_settings->out_lines_before_patterns())) {
                        continue;
                    }
                }
                std::vector<std::string> lines_after;
                if (m_settings->lines_after() > 0) {
                    lines_after = get_lines_after_pos(s, newline_indices, m_settings->lines_after(),
                                                       line_start_idx);
                    if (!lines_match(lines_after, m_settings->in_lines_after_patterns(),
                                     m_settings->out_lines_after_patterns())) {
                        continue;
                    }
                }

                results.push_back(new SearchResult(p, nullptr, line_num,
                                                   match_start_idx - line_start_idx + 1,
                                                   match_end_idx - line_start_idx + 1,
                                                   line, &lines_before, &lines_after));

                if (m_settings->first_match()) {
                    break;
                }
            }
            // ---------------------------------------------------------------------
        }

        return results;
    }

    std::vector<SearchResult*> Searcher::search_binary_file(cppfind::FileResult* fr) {
        std::vector<SearchResult*> results = {};
        std::set <std::string> found_patterns = {};

        std::ifstream fin(fr->string());
        std::string s = cppfind::FileUtil::get_contents(fin);
        fin.close();

        std::smatch pmatch;
        for (const auto& p : *(m_settings->search_patterns())) {
            if (m_settings->first_match() && found_patterns.find(p->pattern()) != found_patterns.end()) {
                continue;
            }
            unsigned long trimmed = 0;
            std::string sbuf = std::string(s);
            bool skip_pattern = false;
            while (!skip_pattern && !sbuf.empty() && regex_search(sbuf, pmatch, p->r())) {
                for (unsigned i=0; i < pmatch.size(); ++i) {
                    unsigned long match_start_idx = pmatch.position(i) + trimmed;
                    unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                    results.push_back(new SearchResult(p, fr, 0, match_start_idx, match_end_idx, ""));
                    if (m_settings->first_match()) {
                        found_patterns.insert(p->pattern());
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
