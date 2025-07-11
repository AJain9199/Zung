#include "cmd.h"
#include <algorithm>

std::string getCmdOption(char **begin, char **end, const std::string &option) {
    char **itr = std::find(begin, end, option);
    if (itr != end && ++itr != end) {
        return *itr;
    }
    return "";
}

std::string getCmdOption(char **begin, char **end, const std::string &option, const std::string &default_value) {
    return getCmdOption(begin, end, option) ? getCmdOption(begin, end, option) : default_value;
}

bool cmdOptionExists(char **begin, char **end, const std::string &option) {
    return std::find(begin, end, option) != end;
}
