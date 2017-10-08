#include <iostream>
#include "common.h"

void log(const std::string& msg) {
    std::cout << msg << std::endl;
}

void log_error(const std::string& msg) {
    std::cout << "ERROR: " << msg << std::endl;
}
