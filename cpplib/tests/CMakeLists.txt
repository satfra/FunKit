# get the Catch2 testing framework

include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v3.15.1 # or a later release
)

FetchContent_MakeAvailable(Catch2)
list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)
include(CTest)
include(Catch)

# enable testing
enable_testing()

add_compile_definitions(
  "BOILERPLATE_DIR=std::string(\"${CMAKE_CURRENT_SOURCE_DIR}/boilerplate/\")")

add_executable(derivatives ${CMAKE_CURRENT_SOURCE_DIR}/derivatives.cpp)
target_link_libraries(derivatives PRIVATE funkitlib Catch2::Catch2WithMain)
catch_discover_tests(derivatives)
