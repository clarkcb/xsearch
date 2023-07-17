#include "SearchException.h"

namespace cppsearch {
    SearchException::SearchException(const std::string& message) {
        m_message = message;
    }

    std::string SearchException::msg() const noexcept {
        return m_message;
    }

    const char *SearchException::what() const noexcept {
        return m_message.c_str();
    }
}
