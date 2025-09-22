# ##############################################################################
# CMakeLists.txt for the DiFfRG library
# ##############################################################################

cmake_minimum_required(VERSION 3.20)

project(FunKit)

set(CMAKE_INSTALL_MESSAGE LAZY)

include(${CMAKE_CURRENT_SOURCE_DIR}/utils/common.cmake)

if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.31.0")
  cmake_policy(SET CMP0177 NEW)
endif()

# ##############################################################################
# Install the Mathematica package
# ##############################################################################

# download CPM.cmake
file(
  DOWNLOAD
  https://github.com/cpm-cmake/CPM.cmake/releases/download/v0.40.8/CPM.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/cmake/CPM.cmake
  EXPECTED_HASH
    SHA256=78ba32abdf798bc616bab7c73aac32a17bbd7b06ad9e26a6add69de8f3ae4791)
include(${CMAKE_CURRENT_BINARY_DIR}/cmake/CPM.cmake)
if("${CPM_SOURCE_CACHE}" STREQUAL "OFF" OR NOT DEFINED CPM_SOURCE_CACHE)
  set(CPM_SOURCE_CACHE $ENV{HOME}/.cache/CPM)
endif()

# Get codeparser
cpmaddpackage(
  NAME
  LibraryLinkUtilities
  GITHUB_REPOSITORY
  WolframResearch/LibraryLinkUtilities
  GIT_TAG
  v3.2.0
  DOWNLOAD_ONLY
  True)
list(APPEND CMAKE_MODULE_PATH ${LibraryLinkUtilities_SOURCE_DIR}/cmake)

find_package(WolframLanguage 12.0 COMPONENTS wolframscript)

if(${WolframLanguage_FOUND})
  message(STATUS "Wolfram Language found: ${WolframLanguage_VERSION}")
  message(
    STATUS "WolframScript executable: ${WolframLanguage_wolframscript_EXE}")

  # get the application directory
  execute_process(
    COMMAND ${WolframLanguage_wolframscript_EXE} -script
            ${CMAKE_CURRENT_SOURCE_DIR}/utils/get_wolfram_app_dir.m
    OUTPUT_VARIABLE WOLFRAM_APP_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  message(STATUS "Wolfram Language application directory: ${WOLFRAM_APP_DIR}")

  # install the Mathematica package
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/FunKit.m
          DESTINATION ${WOLFRAM_APP_DIR}/FunKit)
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/modules
              ${CMAKE_CURRENT_SOURCE_DIR}/examples
              ${CMAKE_CURRENT_SOURCE_DIR}/utils
              ${CMAKE_CURRENT_SOURCE_DIR}/tests
    MESSAGE_NEVER
    DESTINATION ${WOLFRAM_APP_DIR}/FunKit
    FILES_MATCHING
    PATTERN "*.m"
    PATTERN "*.wl"
    PATTERN "*.mx"
    PATTERN "*.nb")
else()
  message(ERROR "Wolfram Language not found.")
endif()

# ##############################################################################
# Testing
# ##############################################################################

add_custom_target(
  test
  COMMAND
    ${WolframLanguage_wolframscript_EXE} -script
    ${CMAKE_CURRENT_SOURCE_DIR}/tests/TestRunner.m | tee
    ${CMAKE_CURRENT_BINARY_DIR}/test.log
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tests/TestRunner.m
  COMMENT "Running tests"
  VERBATIM)
