#ifndef CMD_H
#define CMD_H

#include <string>

#define GET_CMD(option) getCmdOption(argv, argv + argc, option)
#define GET_CMD_DEFAULT(option, default_value) getCmdOption(argv, argv + argc, option, default_value)

std::string getCmdOption(char **begin, char **end, const std::string &option);
std::string getCmdOption(char **begin, char **end, const std::string &option, const std::string &default_value);

bool cmdOptionExists(char **begin, char **end, const std::string &option);

#endif //CMD_H
