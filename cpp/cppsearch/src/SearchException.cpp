#include "SearchException.h"

SearchException::SearchException(const std::string msg) {
    message = msg;
}

const char *SearchException::what() const throw() {
    return message.c_str();
}
