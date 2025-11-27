#include <ranges>

#include "SearchOption.h"

namespace cppsearch {
    SearchOption::SearchOption(const std::string_view short_arg, const std::string_view long_arg,
                               const std::string_view description, const int arg_type) :
    m_short_arg(short_arg), m_long_arg(long_arg), m_description(description), m_arg_type{arg_type} {
        if (!m_short_arg.empty()) {
            m_sort_arg = m_short_arg;
            std::ranges::transform(m_sort_arg.begin(), m_sort_arg.end(), m_sort_arg.begin(),
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

    int SearchOption::arg_type() const {
        return m_arg_type;
    }

    std::string SearchOption::sort_arg() const {
        return m_sort_arg;
    }
}
