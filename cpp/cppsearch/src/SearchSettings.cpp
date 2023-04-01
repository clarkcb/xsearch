#include "FileUtil.h"
#include "StringUtil.h"
#include "SearchSettings.h"

namespace cppsearch {
    SearchSettings::SearchSettings() {
        m_in_archiveextensions = {};
        m_in_archivefilepatterns = {};
        m_in_dirpatterns = {};
        m_in_extensions = {};
        m_in_filepatterns = {};
        m_in_filetypes = {};
        m_out_archiveextensions = {};
        m_out_archivefilepatterns = {};
        m_out_dirpatterns = {};
        m_out_extensions = {};
        m_out_filepatterns = {};
        m_out_filetypes = {};
        m_paths = {};
        m_searchpatterns = {};
    }

    void SearchSettings::add_pattern(const std::string& p, std::vector<SearchPattern*>* ps) {
        ps->push_back(new SearchPattern(p));
    }

    void SearchSettings::add_extensions(const std::string& exts, std::vector<std::string>* extensions) {
        std::vector<std::string> xs = StringUtil::split_string(exts, ",");
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions->push_back(x);
            }
        }
    }

    void SearchSettings::add_in_archiveextension(const std::string& ext) {
        add_extensions(ext, &m_in_archiveextensions);
    }

    void SearchSettings::add_in_archivefilepattern(const std::string& p) {
        add_pattern(p, &m_in_archivefilepatterns);
    }

    void SearchSettings::add_in_dirpattern(const std::string& p) {
        add_pattern(p, &m_in_dirpatterns);
    }

    void SearchSettings::add_in_extension(const std::string& ext) {
        add_extensions(ext, &m_in_extensions);
    }

    void SearchSettings::add_in_filepattern(const std::string& p) {
        add_pattern(p, &m_in_filepatterns);
    }

    void SearchSettings::add_in_filetype(const FileType filetype) {
        m_in_filetypes.push_back(filetype);
    }

    void SearchSettings::add_in_linesafterpattern(const std::string& p) {
        add_pattern(p, &m_in_linesafterpatterns);
    }

    void SearchSettings::add_in_linesbeforepattern(const std::string& p) {
        add_pattern(p, &m_in_linesbeforepatterns);
    }

    void SearchSettings::add_linesaftertopattern(const std::string& p) {
        add_pattern(p, &m_linesaftertopatterns);
    }

    void SearchSettings::add_linesafteruntilpattern(const std::string& p) {
        add_pattern(p, &m_linesafteruntilpatterns);
    }

    void SearchSettings::add_out_archiveextension(const std::string& ext) {
        add_extensions(ext, &m_out_archiveextensions);
    }

    void SearchSettings::add_out_archivefilepattern(const std::string& p) {
        add_pattern(p, &m_out_archivefilepatterns);
    }

    void SearchSettings::add_out_dirpattern(const std::string& p) {
        add_pattern(p, &m_out_dirpatterns);
    }

    void SearchSettings::add_out_extension(const std::string& ext) {
        add_extensions(ext, &m_out_extensions);
    }

    void SearchSettings::add_out_filepattern(const std::string& p) {
        add_pattern(p, &m_out_filepatterns);
    }

    void SearchSettings::add_out_filetype(const FileType filetype) {
        m_out_filetypes.push_back(filetype);
    }

    void SearchSettings::add_out_linesafterpattern(const std::string& p) {
        add_pattern(p, &m_out_linesafterpatterns);
    }

    void SearchSettings::add_out_linesbeforepattern(const std::string& p) {
        add_pattern(p, &m_out_linesbeforepatterns);
    }

    void SearchSettings::add_path(const std::string& p) {
        m_paths.push_back(p);
    }

    void SearchSettings::add_searchpattern(const std::string& p) {
        add_pattern(p, &m_searchpatterns);
    }

    bool SearchSettings::archivesonly() const {
        return m_archivesonly;
    }

    bool SearchSettings::colorize() const {
        return m_colorize;
    }

    bool SearchSettings::debug() const {
        return m_debug;
    }

    bool SearchSettings::excludehidden() const {
        return m_excludehidden;
    }

    bool SearchSettings::firstmatch() const {
        return m_firstmatch;
    }

    bool SearchSettings::multilinesearch() const {
        return m_multilinesearch;
    }

    unsigned int SearchSettings::linesafter() const {
        return m_linesafter;
    }

    unsigned int SearchSettings::linesbefore() const {
        return m_linesbefore;
    }

    bool SearchSettings::listdirs() const {
        return m_listdirs;
    }

    bool SearchSettings::listfiles() const {
        return m_listfiles;
    }

    bool SearchSettings::listlines() const {
        return m_listlines;
    }

    size_t SearchSettings::maxlinelength() const {
        return m_maxlinelength;
    }

    bool SearchSettings::printresults() const {
        return m_printresults;
    }

    bool SearchSettings::printusage() const {
        return m_printusage;
    }

    bool SearchSettings::printversion() const {
        return m_printversion;
    }

    bool SearchSettings::recursive() const {
        return m_recursive;
    }

    bool SearchSettings::searcharchives() const {
        return m_searcharchives;
    }

    std::vector<std::string>* SearchSettings::in_archiveextensions() {
        return &m_in_archiveextensions;
    }

    std::vector<SearchPattern*>* SearchSettings::in_archivefilepatterns() {
        return &m_in_archivefilepatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_dirpatterns() {
        return &m_in_dirpatterns;
    }

    std::vector<std::string>* SearchSettings::in_extensions() {
        return &m_in_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::in_filepatterns() {
        return &m_in_filepatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_linesafterpatterns() {
        return &m_in_linesafterpatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_linesbeforepatterns() {
        return &m_in_linesbeforepatterns;
    }

    std::vector<std::string>* SearchSettings::out_archiveextensions() {
        return &m_out_archiveextensions;
    }

    std::vector<SearchPattern*>* SearchSettings::out_archivefilepatterns() {
        return &m_out_archivefilepatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_dirpatterns() {
        return &m_out_dirpatterns;
    }

    std::vector<std::string>* SearchSettings::out_extensions() {
        return &m_out_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::out_filepatterns() {
        return &m_out_filepatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_linesafterpatterns() {
        return &m_out_linesafterpatterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_linesbeforepatterns() {
        return &m_out_linesbeforepatterns;
    }

    std::vector<std::string>* SearchSettings::paths() {
        return &m_paths;
    }

    std::vector<SearchPattern*>* SearchSettings::searchpatterns() {
        return &m_searchpatterns;
    }

    bool SearchSettings::uniquelines() const {
        return m_uniquelines;
    }

    bool SearchSettings::verbose() const {
        return m_verbose;
    }

    void SearchSettings::archivesonly(const bool b) {
        m_archivesonly = b;
        if (b) m_searcharchives = b;
    }

    void SearchSettings::colorize(const bool b) {
        m_colorize = b;
    }

    void SearchSettings::debug(const bool b) {
        m_debug = b;
        if (b) m_verbose = b;
    }

    void SearchSettings::excludehidden(const bool b) {
        m_excludehidden = b;
    }

    void SearchSettings::firstmatch(const bool b) {
        m_firstmatch = b;
    }

    void SearchSettings::linesafter(const unsigned int linecount) {
        m_linesafter = linecount;
    }

    void SearchSettings::linesbefore(const unsigned int linecount) {
        m_linesbefore = linecount;
    }

    void SearchSettings::listdirs(const bool b) {
        m_listdirs = b;
    }

    void SearchSettings::listfiles(const bool b) {
        m_listfiles = b;
    }

    void SearchSettings::listlines(const bool b) {
        m_listlines = b;
    }

    void SearchSettings::maxlinelength(const size_t max) {
        m_maxlinelength = max;
    }

    void SearchSettings::multilinesearch(const bool b) {
        m_multilinesearch = b;
    }

    void SearchSettings::printresults(const bool b) {
        m_printresults = b;
    }

    void SearchSettings::printusage(const bool b) {
        m_printusage = b;
    }

    void SearchSettings::printversion(const bool b) {
        m_printversion = b;
    }

    void SearchSettings::recursive(const bool b) {
        m_recursive = b;
    }

    void SearchSettings::searcharchives(const bool b) {
        m_searcharchives = b;
    }

    void SearchSettings::uniquelines(const bool b) {
        m_uniquelines = b;
    }

    void SearchSettings::verbose(const bool b) {
        m_verbose = b;
    }

    std::string SearchSettings::bool_to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string SearchSettings::string_vector_to_string(std::vector<std::string>* ss) {
        std::string ss_string = "[";
        int count = 0;
        for (auto const& s : *ss) {
            if (count > 0) {
                ss_string.append(", ");
            }
            ss_string.append("\"").append(s).append("\"");
            count++;
        }
        ss_string.append("]");
        return ss_string;
    }

    std::string SearchSettings::searchpatterns_to_string(std::vector<SearchPattern*>* ps) {
        std::string ps_string = "[";
        int count = 0;
        for (auto const& p : *ps) {
            if (count > 0) {
                ps_string.append(", ");
            }
            ps_string.append("\"").append(p->pattern()).append("\"");
            count++;
        }
        ps_string.append("]");
        return ps_string;
    }

    std::string SearchSettings::string() {
        auto settings_str =
                std::string("SearchSettings(")
                + "archivesonly: " + bool_to_string(m_archivesonly)
                + ", colorize: " + bool_to_string(m_colorize)
                + ", debug: " + bool_to_string(m_debug)
                + ", excludehidden: " + bool_to_string(m_excludehidden)
                + ", firstmatch: " + bool_to_string(m_firstmatch)
                + ", in_archiveextensions: " + string_vector_to_string(&m_in_archiveextensions)
                + ", in_archivefilepatterns: " + searchpatterns_to_string(&m_in_archivefilepatterns)
                + ", in_dirpatterns: " + searchpatterns_to_string(&m_in_dirpatterns)
                + ", in_extensions: " + string_vector_to_string(&m_in_extensions)
                + ", in_filepatterns: " + searchpatterns_to_string(&m_in_filepatterns)
                + ", in_linesafterpatterns: " + searchpatterns_to_string(&m_in_linesafterpatterns)
                + ", in_linesbeforepatterns: " + searchpatterns_to_string(&m_in_linesbeforepatterns)
                + ", linesafter: " + std::to_string(m_linesafter)
                + ", linesaftertopatterns: " + searchpatterns_to_string(&m_linesaftertopatterns)
                + ", linesafteruntilpatterns: " + searchpatterns_to_string(&m_linesafteruntilpatterns)
                + ", linesbefore: " + std::to_string(m_linesbefore)
                + ", listdirs: " + bool_to_string(m_listdirs)
                + ", listfiles: " + bool_to_string(m_listfiles)
                + ", listlines: " + bool_to_string(m_listlines)
                + ", maxlinelength: " + std::to_string(m_maxlinelength)
                + ", multilinesearch: " + bool_to_string(m_multilinesearch)
                + ", out_archiveextensions: " + string_vector_to_string(&m_out_archiveextensions)
                + ", out_archivefilepatterns: " + searchpatterns_to_string(&m_out_archivefilepatterns)
                + ", out_dirpatterns: " + searchpatterns_to_string(&m_out_dirpatterns)
                + ", out_extensions: " + string_vector_to_string(&m_out_extensions)
                + ", out_filepatterns: " + searchpatterns_to_string(&m_out_filepatterns)
                + ", out_linesafterpatterns: " + searchpatterns_to_string(&m_out_linesafterpatterns)
                + ", out_linesbeforepatterns: " + searchpatterns_to_string(&m_out_linesbeforepatterns)
                + ", paths: " + string_vector_to_string(&m_paths)
                + ", printresults: " + bool_to_string(m_printresults)
                + ", printusage: " + bool_to_string(m_printusage)
                + ", printversion: " + bool_to_string(m_printversion)
                + ", recursive: " + bool_to_string(m_recursive)
                + ", searcharchives: " + bool_to_string(m_searcharchives)
                + ", searchpatterns: " + searchpatterns_to_string(&m_searchpatterns)
                + ", uniquelines: " + bool_to_string(m_uniquelines)
                + ", verbose: " + bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }

    std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
        std::string settings_string = settings.string();
        return strm << settings_string;
    }
}
