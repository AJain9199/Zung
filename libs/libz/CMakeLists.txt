CMAKE_MINIMUM_REQUIRED(VERSION 3.22)

PROJECT(libz VERSION 0.1 LANGUAGES ASM)

add_library(z SHARED src/pow.S src/flr.S src/syscall.S)

set_target_properties(z PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    PUBLIC_HEADER include/z.h)

install(TARGETS z
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})