#ifndef ZUNG_LOGGER_H
#define ZUNG_LOGGER_H

#include <string>
#include <iostream>

enum ErrorPriority {
    WARN,
    FATAL
};

static inline void error(ErrorPriority priority, const std::string& msg) {
    std::cerr << (priority == FATAL ? "Fatal Error: ": "Warning: ") << msg;
}

static inline void error(const std::string& msg) {
    error(FATAL, msg);
}


#endif //ZUNG_LOGGER_H
