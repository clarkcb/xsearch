#ifndef CPPSEARCH_SEARCHEXCEPTION_H
#define CPPSEARCH_SEARCHEXCEPTION_H

#include <string>

class SearchException : public std::exception {
public:
    explicit SearchException(std::string message);
    const char *what() const throw();

private:
    std::string message;
};

#endif //CPPSEARCH_SEARCHEXCEPTION_H
