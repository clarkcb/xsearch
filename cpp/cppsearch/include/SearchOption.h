#ifndef CPPSEARCH_SEARCHOPTION_H
#define CPPSEARCH_SEARCHOPTION_H

namespace cppsearch {
    class SearchOption {
    private:
        const std::string* m_short_arg;
        std::string m_long_arg;
        std::string m_description;
        std::string m_sort_arg;

    public:
        SearchOption(const std::string* short_arg, const std::string& long_arg, const std::string& description);
        [[nodiscard]] const std::string* short_arg() const;
        [[nodiscard]] std::string long_arg() const;
        [[nodiscard]] std::string description() const;
        [[nodiscard]] std::string sort_arg() const;
    };
}

#endif //CPPSEARCH_SEARCHOPTION_H
