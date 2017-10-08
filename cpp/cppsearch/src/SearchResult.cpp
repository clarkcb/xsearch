#include <boost/format.hpp>
#include <StringUtil.h>

using namespace std;
#include "SearchResult.h"

SearchResult::SearchResult(const SearchPattern* p, SearchFile* sf,
                           const long lnum, const long ms_idx, const long me_idx,
                           const string ln) {
    vector<string> lsb = {};
    vector<string> lsa = {};
    init(p, sf, lnum, ms_idx, me_idx, ln, &lsb, &lsa);
}

SearchResult::SearchResult(const SearchPattern* p, SearchFile* sf, const long lnum,
                           const long ms_idx, const long me_idx, const string ln,
                           vector<string>* lsb, vector<string>* lsa) {
    init(p, sf, lnum, ms_idx, me_idx, ln, lsb, lsa);
}

void SearchResult::init(const SearchPattern* p, SearchFile* sf, const long lnum,
                        const long ms_idx, const long me_idx, const string ln,
                        vector<string>* lsb, vector<string>* lsa) {
    pattern = p;
    searchfile = sf;
    linenum = lnum;
    match_start_idx = ms_idx;
    match_end_idx = me_idx;
    line = ln;
    lines_before = *lsb;
    lines_after = *lsa;
}

string SearchResult::single_line_result_to_string() {
    auto result_string = string(searchfile->searchfile_to_string());
    if (linenum == 0) {
        result_string.append(" matches at [")
                .append(to_string(match_start_idx))
                .append(":")
                .append(to_string(match_end_idx))
                .append("]");
    } else {
        result_string.append(": ")
                .append(to_string(linenum))
                .append(": [")
                .append(to_string(match_start_idx))
                .append(":")
                .append(to_string(match_end_idx))
                .append("]: ")
                .append(StringUtil::trim_leading_whitespace(line));
    }
    return result_string;
}

long SearchResult::linenum_padding() {
    long max_linenum = linenum + lines_after.size();
    return boost::str(boost::format("%ld") % max_linenum).length();
}

string SearchResult::multi_line_result_to_string() {
    int line_sep_length = 80;
    auto result_string = string("================================================================================\n");
    if (searchfile == nullptr) {
        result_string.append("<text>");
    } else {
        result_string.append(searchfile->searchfile_to_string());
    }
    result_string.append(": ").append(to_string(linenum)).append(": [").append(to_string(match_start_idx))
            .append(":").append(to_string(match_end_idx)).append("]\n");
    result_string.append("--------------------------------------------------------------------------------\n");
    long current_linenum = linenum;
    string lineFormat = " %1$";
    lineFormat.append(to_string(linenum_padding())).append("ld | %2$s\n");

    if (!lines_before.empty()) {
        current_linenum -= lines_before.size();
        for (const auto& line_before : lines_before) {
            result_string.append(boost::str(boost::format(lineFormat) % current_linenum % line_before));
            current_linenum++;
        }
    }
    result_string.append(">").append(boost::str(boost::format(lineFormat) % current_linenum % line));
    current_linenum++;

    if (!lines_after.empty()) {
        for (const auto& line_after : lines_after) {
            result_string.append(boost::str(boost::format(lineFormat) % current_linenum % line_after));
            current_linenum++;
        }
    }

    return result_string;
}

string SearchResult::result_to_string() {
    if (!lines_before.empty() || !lines_after.empty()) {
        return multi_line_result_to_string();
    } else {
        return single_line_result_to_string();
    }
}
