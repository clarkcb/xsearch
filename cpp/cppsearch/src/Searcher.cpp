#include <algorithm>
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include "common.h"
#include "FileTypes.h"
#include "FileUtil.h"
#include "SearchException.h"
#include "Searcher.h"

namespace cppsearch {
    Searcher::Searcher(SearchSettings* settings) {
        validate_settings(settings);
        m_settings = settings;
        m_file_types = new FileTypes();
    }

    void Searcher::validate_settings(SearchSettings* ss) {
        if (ss->paths()->empty()) {
            throw SearchException("Startpath not defined");
        }
        for (auto& p : *ss->paths()) {
            if (p.empty()) {
                throw SearchException("Startpath not defined");
            }
            std::string expanded = FileUtil::expand_path(p);
            if (!FileUtil::file_exists(p) && !FileUtil::file_exists(expanded)) {
                throw SearchException("Startpath not found");
            }
        }
        if (ss->search_patterns()->empty()) {
            throw SearchException("No search patterns defined");
        }
    }

    SearchFile* Searcher::get_search_file(std::string& file_path) {
        FileType file_type = m_file_types->get_file_type(file_path);
        boost::filesystem::path path(file_path);
        std::string parent_path = path.parent_path().string();
        std::string file_name = path.filename().string();
        return new SearchFile(parent_path, file_name, file_type);
    }

    std::vector<SearchResult*> Searcher::search() {

        std::vector<SearchResult*> results{};

        for (auto& p : *m_settings->paths()) {
            std::string expanded = FileUtil::expand_path(p);

            if (FileUtil::is_directory(p) || FileUtil::is_directory(expanded)
                || FileUtil::is_regular_file(p) || FileUtil::is_regular_file(expanded)) {

                std::vector<SearchResult*> p_results{};

                if (FileUtil::is_directory(p)) {
                    p_results = search_path(p);

                } else if (FileUtil::is_directory(expanded)) {
                    p_results = search_path(expanded);

                } else if (FileUtil::is_regular_file(p)) {
                    auto* sf = get_search_file(p);
                    p_results = search_file(sf);

                } else if (FileUtil::is_regular_file(expanded)) {
                    auto* sf = get_search_file(expanded);
                    p_results = search_file(sf);
                }

                results.insert(results.end(), p_results.begin(), p_results.end());

            } else {
                throw SearchException("path is an unsupported file type");
            }
        }

        return results;
    }

    bool matches_any_pattern(const std::string& s, const std::vector<SearchPattern*>& patterns) {
        std::smatch pmatch;
        for (auto& p : patterns) {
            if (regex_search(s, pmatch, p->r())) {
                return true;
            }
        }
        return false;
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss, const std::vector<SearchPattern*>& patterns) {
        return std::any_of(ss.begin(), ss.end(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    bool Searcher::is_search_dir(const std::string& file_path) {
        std::vector<std::string> elems = FileUtil::split_path(file_path);
        if (m_settings->exclude_hidden()) {
            for (auto& elem : elems) {
                if (FileUtil::is_hidden(elem)) {
                    return false;
                }
            }
        }
        std::vector<SearchPattern*>* in_dir_patterns = m_settings->in_dir_patterns();
        std::vector<SearchPattern*>* out_dir_patterns = m_settings->out_dir_patterns();
        return ((in_dir_patterns->empty() || any_matches_any_pattern(elems, *in_dir_patterns))
                && (out_dir_patterns->empty() || !any_matches_any_pattern(elems, *out_dir_patterns)));
    }

    std::vector<SearchFile*> Searcher::get_search_files(const std::string& file_path) {
        boost::filesystem::path p(file_path);
        std::vector<std::string> search_dirs = {};
        std::vector<SearchFile*> search_files = {};

        std::vector<boost::filesystem::directory_entry> v;
        copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(), back_inserter(v));

        for (std::vector<boost::filesystem::directory_entry>::const_iterator it = v.begin(); it != v.end(); ++it) {
            boost::filesystem::path sub_path = (*it).path();
            if (boost::filesystem::is_directory(sub_path) && m_settings->recursive() && is_search_dir(sub_path.string())) {
                search_dirs.push_back(sub_path.string());
            } else if (boost::filesystem::is_regular_file(sub_path) && filter_file(sub_path.string())) {
                std::string parent_path = sub_path.parent_path().string();
                std::string file_name = sub_path.filename().string();
                FileType file_type = m_file_types->get_file_type(file_name);
                search_files.push_back(new SearchFile(parent_path, file_name, file_type));
            }
        }

        for (const auto& search_dir : search_dirs) {
            std::vector<SearchFile*> subsearch_files = get_search_files(search_dir);
            search_files.insert(search_files.end(), subsearch_files.begin(), subsearch_files.end());
        }

        return search_files;
    }

    bool Searcher::is_search_file(const std::string& file_name) {
        std::string ext = FileUtil::get_extension(file_name);
        std::vector <std::string>* in_exts = m_settings->in_extensions();
        std::vector <std::string>* out_exts = m_settings->out_extensions();
        if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<SearchPattern*>* in_file_patterns = m_settings->in_file_patterns();
        std::vector<SearchPattern*>* out_file_patterns = m_settings->out_file_patterns();
        return ((in_file_patterns->empty() || matches_any_pattern(file_name, *in_file_patterns))
                && (out_file_patterns->empty() || !matches_any_pattern(file_name, *out_file_patterns)));
    }

    bool Searcher::is_archive_search_file(const std::string& file_name) {
        std::string ext = FileUtil::get_extension(file_name);
        std::vector <std::string>* in_exts = m_settings->in_archive_extensions();
        std::vector <std::string>* out_exts = m_settings->out_archive_extensions();
        if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<SearchPattern*>* in_file_patterns = m_settings->in_archive_file_patterns();
        std::vector<SearchPattern*>* out_file_patterns = m_settings->out_archive_file_patterns();
        return ((in_file_patterns->empty() || matches_any_pattern(file_name, *in_file_patterns))
                && (out_file_patterns->empty() || !matches_any_pattern(file_name, *out_file_patterns)));
    }

    bool Searcher::filter_file(const std::string& file_path) {
        boost::filesystem::path p(file_path);
        std::string file_name = p.filename().string();
        if (FileUtil::is_hidden(file_name) && m_settings->exclude_hidden()) {
            return false;
        }
        if (m_file_types->get_file_type(file_name) == FileType::ARCHIVE) {
            return m_settings->search_archives() && is_archive_search_file(file_name);
        }
        return !m_settings->archives_only() && is_search_file(file_name);
    }

    std::vector<SearchResult*> Searcher::search_path(const std::string& file_path) {
        std::vector<SearchResult*> results = {};
        std::vector<SearchFile*> search_files = get_search_files(file_path);

        // sort using a lambda expression
        std::sort(search_files.begin(), search_files.end(), [](SearchFile* sf1, SearchFile* sf2) {
            if (sf1->path() == sf2->path()) {
                return sf1->file_name() < sf2->file_name();
            }
            return sf1->path() < sf2->path();
        });

        if (m_settings->verbose()) {
            std::set<std::string> search_dir_set = {};
            for (const auto& search_file : search_files) {
                search_dir_set.insert(search_file->path());
            }
            std::vector<std::string> search_dirs(search_dir_set.begin(), search_dir_set.end());

            std::string msg = "\nDirectories to be searched (";
            msg.append(std::to_string(search_dirs.size())).append("):");
            log(msg);
            for (const auto& search_dir : search_dirs) {
                log(search_dir);
            }

            msg = "\nFiles to be searched (";
            msg.append(std::to_string(search_files.size())).append("):");
            log(msg);
            for (const auto& search_file : search_files) {
                log(search_file->string());
            }
        }

        for (const auto& sf : search_files) {
            std::vector<SearchResult*> file_results = search_file(sf);
            if (!file_results.empty()) {
                results.insert(results.end(), file_results.begin(), file_results.end());
            }
        }
        return results;
    }

    std::vector<SearchResult*> Searcher::search_file(SearchFile* sf) {
        std::vector<SearchResult*> results = {};
        if (sf->file_type() == FileType::CODE || sf->file_type() == FileType::XML || sf->file_type() == FileType::TEXT) {
            results = search_text_file(sf);
        } else if (sf->file_type() == FileType::BINARY) {
            results = search_binary_file(sf);
        } else if (sf->file_type() == FileType::ARCHIVE) {
            //cout << "m_search_file is an ARCHIVE file: " << sf->file_name() << endl;
        } else {
            std::cout << "m_search_file is an UNKNOWN file: " << sf->file_name() << std::endl;
        }
        return results;
    }

    std::vector<SearchResult*> Searcher::search_text_file(SearchFile* sf) {
        if (m_settings->debug()) {
            std::cout << "Searching text file " << sf->string() << std::endl;
        }
        std::ifstream fin(sf->string());
        std::vector<SearchResult*> results = search_ifstream(fin);
        fin.close();

        for (const auto& r : results) {
            r->set_search_file(sf);
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

    bool lines_match(std::vector<std::string>& lines, std::vector<SearchPattern*>* in_patterns,
                     std::vector<SearchPattern*>* out_patterns) {
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
        std::string contents = FileUtil::get_contents(fin);
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

    std::vector<SearchResult*> Searcher::search_binary_file(SearchFile* sf) {
        std::vector<SearchResult*> results = {};
        std::set <std::string> found_patterns = {};

        std::ifstream fin(sf->string());
        std::string s = FileUtil::get_contents(fin);
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
                    results.push_back(new SearchResult(p, sf, 0, match_start_idx, match_end_idx, ""));
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
