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
    auto* result_string = new string(searchfile->searchfile_to_string());
    result_string->append(": ");
    result_string->append(to_string(linenum));
    result_string->append(": [");
    result_string->append(to_string(match_start_idx));
    result_string->append(":");
    result_string->append(to_string(match_end_idx));
    result_string->append("]: ");
    result_string->append(StringUtil::trim_leading_whitespace(line));
    return *result_string;
}

long SearchResult::linenum_padding() {
    long max_linenum = linenum + lines_after.size();
    return boost::str(boost::format("%ld") % max_linenum).length();
}

string SearchResult::multi_line_result_to_string() {
    int line_sep_length = 80;
    auto* result_string = new string("================================================================================\n");
    if (searchfile == nullptr) {
        result_string->append("<text>");
    } else {
        result_string->append(searchfile->searchfile_to_string());
    }
    result_string->append(": ");
    result_string->append(to_string(linenum));
    result_string->append(": [");
    result_string->append(to_string(match_start_idx));
    result_string->append(":");
    result_string->append(to_string(match_end_idx));
    result_string->append("]\n");
    result_string->append("--------------------------------------------------------------------------------\n");
    int current_linenum = linenum;
    string lineFormat = " %1$";
    lineFormat.append(to_string(linenum_padding()));
    lineFormat.append("ld | %2$s\n");

    result_string->append(StringUtil::trim_leading_whitespace(line));
    return *result_string;
}

string SearchResult::result_to_string() {
    if (!lines_before.empty() || !lines_after.empty()) {
        return multi_line_result_to_string();
    } else {
        return single_line_result_to_string();
    }
}
