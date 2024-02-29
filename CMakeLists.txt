cmake_minimum_required(VERSION 3.22)
project(Zung)

set(CMAKE_CXX_STANDARD 23)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(include ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(Zung main.cpp src/lexer.cpp src/AST.cpp src/parse.cpp
        src/print_visitor.cpp
        src/symbol_table.cpp
        src/code_generation_engine.cpp)

execute_process(COMMAND ~/llvm-project/build/bin/llvm-config --libs core --system-libs RESULT_VARIABLE l OUTPUT_VARIABLE llvm_libs)

message(STATUS "LLVM_LIBS: ${llvm_libs} ${l}")

# Link against LLVM libraries
target_link_libraries(Zung LLVMCore LLVMRemarks LLVMBitstreamReader LLVMBinaryFormat LLVMTargetParser LLVMSupport LLVMDemangle rt dl m)