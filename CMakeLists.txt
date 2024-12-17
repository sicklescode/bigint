cmake_minimum_required(VERSION 3.16)
project(numbers)

set(CMAKE_CXX_STANDARD 17)

add_library(numbers INTERFACE)
target_include_directories(numbers INTERFACE include/)

add_executable(numbers-test numbers.cpp)
target_link_libraries(numbers-test numbers)
