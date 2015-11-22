cmake_minimum_required(VERSION 3.2)
project(pwb_13)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES cpp/src/main.cpp)
add_executable(pwb_13 ${SOURCE_FILES})