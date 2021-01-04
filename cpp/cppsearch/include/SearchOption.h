#ifndef CPPSEARCH_SEARCHOPTION_H
#define CPPSEARCH_SEARCHOPTION_H

namespace cppsearch {
    class SearchOption {
    private:
        const std::string* m_shortarg;
        std::string m_longarg;
        std::string m_description;
        std::string m_sortarg;

    public:
        SearchOption(const std::string* shortarg, const std::string& longarg, const std::string& description);
        const std::string* shortarg() const;
        std::string longarg() const;
        std::string description() const;
        std::string sortarg() const;
    };
}

#endif //CPPSEARCH_SEARCHOPTION_H
