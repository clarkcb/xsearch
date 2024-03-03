#include "SearchOption.h"

namespace cppsearch {
    SearchOption::SearchOption(const std::string_view short_arg, const std::string_view long_arg,
                               const std::string_view description) {
        m_short_arg = std::string{short_arg};
        m_long_arg = std::string{long_arg};
        m_description = std::string{description};
        if (!m_short_arg.empty()) {
            m_sort_arg = m_short_arg;
            std::transform(m_sort_arg.begin(), m_sort_arg.end(), m_sort_arg.begin(),
                           [](const unsigned char c) { return std::tolower(c); });
            m_sort_arg.append("@");
            m_sort_arg.append(m_long_arg);
        } else {
            m_sort_arg = m_long_arg;
        }
    }

    std::string SearchOption::short_arg() const {
        return m_short_arg;
    }

    std::string SearchOption::long_arg() const {
        return m_long_arg;
    }

    std::string SearchOption::description() const {
        return m_description;
    }

    std::string SearchOption::sort_arg() const {
        return m_sort_arg;
    }
}
