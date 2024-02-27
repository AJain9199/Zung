cmake_minimum_required(VERSION 3.22)
project(Zung)

set(CMAKE_CXX_STANDARD 23)

#find_package(LLVM REQUIRED CONFIG)
#
include_directories(include)
#separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
#add_definitions(${LLVM_DEFINITIONS_LIST})
add_executable(Zung main.cpp src/lexer.cpp src/AST.cpp src/parser.cpp
        src/PrintVisitor.cpp)

#llvm_map_components_to_libnames(llvm_libs engine)
#target_link_libraries(Zung ${llvm_libs})