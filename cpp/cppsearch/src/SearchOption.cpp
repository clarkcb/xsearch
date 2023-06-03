#include <boost/algorithm/string.hpp>
#include "SearchOption.h"

namespace cppsearch {
    SearchOption::SearchOption(const std::string* sa, const std::string& la, const std::string& desc) {
        m_short_arg = sa;
        m_long_arg = la;
        m_description = desc;
        if (m_short_arg != nullptr && !m_short_arg->empty()) {
            std::string so = boost::to_lower_copy(*m_short_arg);
            so.append("@");
            so.append(m_long_arg);
            m_sort_arg = so;
        } else {
            m_sort_arg = m_long_arg;
        }
    }

    const std::string* SearchOption::short_arg() const {
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
