#include "SearchException.h"

namespace cppsearch {
    SearchException::SearchException(const std::string& message) {
        m_message = message;
    }

    const char *SearchException::what() const throw() {
        return m_message.c_str();
    }
}
