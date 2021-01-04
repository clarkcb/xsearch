#ifndef CPPSEARCH_SEARCHSETTINGS_H
#define CPPSEARCH_SEARCHSETTINGS_H

#include <cstdlib>
#include "FileTypes.h"
#include "SearchPattern.h"

namespace cppsearch {
    class SearchSettings {
    private:
        bool m_archivesonly = false;
        bool m_colorize = true;
        bool m_debug = false;
        bool m_excludehidden = true;
        bool m_firstmatch = false;

        std::vector<std::string> m_in_archiveextensions;
        std::vector<SearchPattern*> m_in_archivefilepatterns;
        std::vector<SearchPattern*> m_in_dirpatterns;
        std::vector<std::string> m_in_extensions;
        std::vector<SearchPattern*> m_in_filepatterns;
        std::vector<FileType> m_in_filetypes;

        std::vector<SearchPattern*> m_in_linesafterpatterns;
        std::vector<SearchPattern*> m_in_linesbeforepatterns;

        std::vector<SearchPattern*> m_linesaftertopatterns;
        std::vector<SearchPattern*> m_linesafteruntilpatterns;

        unsigned int m_linesafter = 0;
        unsigned int m_linesbefore = 0;
        bool m_listdirs = false;
        bool m_listfiles = false;
        bool m_listlines = false;
        size_t m_maxlinelength = 150;
        bool m_multilinesearch = false;

        std::vector<std::string> m_out_archiveextensions;
        std::vector<SearchPattern*> m_out_archivefilepatterns;
        std::vector<SearchPattern*> m_out_dirpatterns;
        std::vector<std::string> m_out_extensions;
        std::vector<SearchPattern*> m_out_filepatterns;
        std::vector<FileType> m_out_filetypes;

        std::vector<SearchPattern*> m_out_linesafterpatterns;
        std::vector<SearchPattern*> m_out_linesbeforepatterns;

        bool m_printresults = false;
        bool m_printusage = false;
        bool m_printversion = false;
        bool m_recursive = true;
        bool m_searcharchives = false;

        std::vector<SearchPattern*> m_searchpatterns;
        std::string* m_startpath;

        bool m_uniquelines = false;
        bool m_verbose = false;

        static std::string bool_to_string(bool b);
        static std::string string_vector_to_string(std::vector<std::string>* s);
        static std::string searchpatterns_to_string(std::vector<SearchPattern*>* ps);

        static void add_pattern(const std::string& p, std::vector<SearchPattern*>* ps);
        static void add_extensions(const std::string& exts, std::vector<std::string>* extensions);

    public:
        SearchSettings();
        void add_in_archiveextension(const std::string& ext);
        void add_in_archivefilepattern(const std::string& pattern);
        void add_in_dirpattern(const std::string& pattern);
        void add_in_extension(const std::string& ext);
        void add_in_filepattern(const std::string& pattern);
        void add_in_filetype(FileType filetype);
        void add_in_linesafterpattern(const std::string& pattern);
        void add_in_linesbeforepattern(const std::string& pattern);
        void add_linesaftertopattern(const std::string& pattern);
        void add_linesafteruntilpattern(const std::string& pattern);
        void add_out_archiveextension(const std::string& ext);
        void add_out_archivefilepattern(const std::string& pattern);
        void add_out_dirpattern(const std::string& pattern);
        void add_out_extension(const std::string& ext);
        void add_out_filepattern(const std::string& pattern);
        void add_out_filetype(FileType filetype);
        void add_out_linesafterpattern(const std::string& pattern);
        void add_out_linesbeforepattern(const std::string& pattern);
        void add_searchpattern(const std::string& searchpattern);

        bool archivesonly() const;
        bool colorize() const;
        bool debug() const;
        bool excludehidden() const;
        bool firstmatch() const;
        bool multilinesearch() const;
        unsigned int linesafter() const;
        unsigned int linesbefore() const;
        bool listdirs() const;
        bool listfiles() const;
        bool listlines() const;
        size_t maxlinelength() const;
        bool printresults() const;
        bool printusage() const;
        bool printversion() const;
        bool recursive() const;
        bool searcharchives() const;
        std::string* startpath();
        bool uniquelines() const;
        bool verbose() const;

        std::vector<std::string>* in_archiveextensions();
        std::vector<SearchPattern*>* in_archivefilepatterns();
        std::vector<SearchPattern*>* in_dirpatterns();
        std::vector<std::string>* in_extensions();
        std::vector<SearchPattern*>* in_filepatterns();

        std::vector<std::string>* out_archiveextensions();
        std::vector<SearchPattern*>* out_archivefilepatterns();
        std::vector<SearchPattern*>* out_dirpatterns();
        std::vector<std::string>* out_extensions();
        std::vector<SearchPattern*>* out_filepatterns();
        std::vector<SearchPattern*>* searchpatterns();

        // bool is_in_archiveextension(const std::string* ext);
        // bool is_in_extension(const std::string* ext);
        // bool is_in_filetype(const FileType* m_filetype);
        // bool is_out_archiveextension(const std::string* ext);
        // bool is_out_extension(const std::string* ext);

        void archivesonly(bool b);
        void colorize(bool b);
        void debug(bool b);
        void excludehidden(bool b);
        void firstmatch(bool b);
        void multilinesearch(bool b);
        void linesafter(unsigned int linecount);
        void linesbefore(unsigned int linecount);
        void listdirs(bool b);
        void listfiles(bool b);
        void listlines(bool b);
        void maxlinelength(size_t max);
        void printresults(bool b);
        void printusage(bool b);
        void printversion(bool b);
        void recursive(bool b);
        void searcharchives(bool b);
        void startpath(std::string& startpath);
        void uniquelines(bool b);
        void verbose(bool b);
        std::string string();
    };
}

#endif //CPPSEARCH_SEARCHSETTINGS_H
