# ##############################################################################
# CMakeLists.txt for the DiFfRG library
# ##############################################################################

cmake_minimum_required(VERSION 3.20)

project(FunKit LANGUAGES C)

set(CMAKE_INSTALL_MESSAGE LAZY)

include(${CMAKE_CURRENT_SOURCE_DIR}/utils/common.cmake)

if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.31.0")
  cmake_policy(SET CMP0177 NEW)
endif()

# ##############################################################################
# Install the Mathematica package
# ##############################################################################

if(WIN32)
  message(FATAL_ERROR "Installation on Windows is not yet supported - on it!.")
endif()

set(WolframLanguage_wolframscript_EXE "")
execute_process(
  COMMAND bash -c "which wolframscript"
  OUTPUT_VARIABLE WolframLanguage_wolframscript_EXE
  ERROR_QUIET)
if(WolframLanguage_wolframscript_EXE STREQUAL "")
  execute_process(
    COMMAND bash -c "which wolfram"
    OUTPUT_VARIABLE WolframLanguage_wolframscript_EXE
    ERROR_QUIET)
endif()
if(WolframLanguage_wolframscript_EXE STREQUAL "")
  execute_process(
    COMMAND bash -c "which math"
    OUTPUT_VARIABLE WolframLanguage_wolframscript_EXE
    ERROR_QUIET)
endif()

# See if the variable WolframLanguage_wolframscript_EXE is defined
if(DEFINED ENV{WOLFRAM_SCRIPT})
  set(WolframLanguage_wolframscript_EXE $ENV{WOLFRAM_SCRIPT})
elseif(NOT (WolframLanguage_wolframscript_EXE STREQUAL ""))
  # nothing to do, we found it with which
else()
  set(WolframLanguage_wolframscript_EXE "")
endif()

if(NOT DEFINED WolframLanguage_wolframscript_EXE
   OR WolframLanguage_wolframscript_EXE STREQUAL "")
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
  else()
    message(ERROR "Wolfram Language not found.")
  endif()
endif()

# strip newlines from the executable path. Happens sometimes, breaks everything
string(REPLACE "\n" "" WolframLanguage_wolframscript_EXE
               "${WolframLanguage_wolframscript_EXE}")
message(
  STATUS
    "Found a matching wolfram executable at: ${WolframLanguage_wolframscript_EXE}"
)

# get the application directory
execute_process(
  COMMAND ${WolframLanguage_wolframscript_EXE} -script
          ${CMAKE_CURRENT_SOURCE_DIR}/utils/get_wolfram_app_dir.m
  OUTPUT_VARIABLE WOLFRAM_APP_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "Wolfram Language application directory: ${WOLFRAM_APP_DIR}")

# Install dependencies if we are NOT on windows
if(WIN32)
  execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/dependencies/install.bat
                          ${WOLFRAM_APP_DIR})
else()
  execute_process(
    COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/dependencies/install.sh
            ${WOLFRAM_APP_DIR})
endif()

# install the Mathematica package
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/FunKit.m
        DESTINATION ${WOLFRAM_APP_DIR}/FunKit)
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/modules
            ${CMAKE_CURRENT_SOURCE_DIR}/examples
            ${CMAKE_CURRENT_SOURCE_DIR}/utils ${CMAKE_CURRENT_SOURCE_DIR}/tests
  MESSAGE_NEVER
  DESTINATION ${WOLFRAM_APP_DIR}/FunKit
  FILES_MATCHING
  PATTERN "*.m"
  PATTERN "*.wl"
  PATTERN "*.mx"
  PATTERN "*.nb")

# ##############################################################################
# Testing
# ##############################################################################

add_custom_target(
  test
  COMMAND
    ${WolframLanguage_wolframscript_EXE} -script
    ${CMAKE_CURRENT_SOURCE_DIR}/tests/Test.m | tee
    ${CMAKE_CURRENT_BINARY_DIR}/test.log
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tests/Test.m
  COMMENT "Running tests"
  VERBATIM)
