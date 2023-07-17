#ifndef CPPSEARCH_SEARCHEXCEPTION_H
#define CPPSEARCH_SEARCHEXCEPTION_H

#include <string>

namespace cppsearch {
    class SearchException : public std::exception {
    public:
        explicit SearchException(const std::string& message);
        [[nodiscard]] std::string msg() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;

    private:
        std::string m_message;
    };
}

#endif //CPPSEARCH_SEARCHEXCEPTION_H
