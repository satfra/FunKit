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

set(SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/source/core.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/exceptions.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/io.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/parse.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/derivatives.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/simplify.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/transformations.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/source/truncation.cpp)

add_library(funkitlib STATIC ${SOURCES})
target_link_libraries(funkitlib PUBLIC OpenMP::OpenMP_CXX)
target_include_directories(funkitlib
                           PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include/)

add_executable(funkit main.cpp)
target_link_libraries(funkit PRIVATE funkitlib)

# enable march=native if NOT on MacOS, enable tree vectorization and disable
if(APPLE)
  target_compile_options(
    funkit PRIVATE "-ftree-vectorize;-fno-math-errno;-fno-trapping-math")
else()
  target_compile_options(
    funkit
    PRIVATE "-march=native;-ftree-vectorize;-fno-math-errno;-fno-trapping-math")
endif()

# add tests (OFF skips the Catch2 FetchContent, which needs network on the
# first configure — used by FSetBackendCpp["RunTests" -> False])
option(FUNKIT_BUILD_TESTING "Build the cpplib test suite" ON)
if(FUNKIT_BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()
