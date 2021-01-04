#ifndef CPPSEARCH_SEARCHEXCEPTION_H
#define CPPSEARCH_SEARCHEXCEPTION_H

#include <string>

namespace cppsearch {
    class SearchException : public std::exception {
    public:
        explicit SearchException(const std::string& message);
        const char *what() const throw();

    private:
        std::string m_message;
    };
}

#endif //CPPSEARCH_SEARCHEXCEPTION_H
