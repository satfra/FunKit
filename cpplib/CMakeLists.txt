cmake_minimum_required(VERSION 3.20)
project(
  FunKit
  VERSION 0.1.0
  LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

find_package(OpenMP REQUIRED)

add_executable(funkit main.cpp)
target_include_directories(funkit PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include/)

# enable march=native
target_compile_options(funkit PRIVATE "-march=native;-ftree-vectorize")
target_compile_definitions(funkit PRIVATE "OPENMP")
target_link_libraries(funkit PRIVATE OpenMP::OpenMP_CXX)
