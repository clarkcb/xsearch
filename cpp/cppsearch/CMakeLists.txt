cmake_minimum_required(VERSION 4.00)
project(cppsearch VERSION 0.1.0 LANGUAGES CXX)

#message("PROJECT_NAME: ${PROJECT_NAME}")
#message("PROJECT_SOURCE_DIR: ${PROJECT_SOURCE_DIR}")

if (CMAKE_CXX_FLAGS STREQUAL "")
        set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic")
endif()

# Adding AddressSanitizer
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -g")

if (NOT DEFINED CMAKE_CXX_STANDARD)
        set(CMAKE_CXX_STANDARD 20)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20")
endif()

if (DEFINED ENV{XSEARCH_PATH})
        set(XSEARCH_PATH $ENV{XSEARCH_PATH})
        message("XSEARCH_PATH (from env): ${XSEARCH_PATH}")
else()
        set(XSEARCH_PATH $ENV{HOME}/src/xsearch)
        message("XSEARCH_PATH (from home): ${XSEARCH_PATH}")
endif()

include_directories(${XSEARCH_PATH}/cpp/cppsearch/include)
include_directories(${Boost_INCLUDE_DIRS})


################################################################################
# cppfind Library section
################################################################################

add_subdirectory(cppfind)
#if (DEFINED ENV{XFIND_PATH})
#        set(XFIND_PATH $ENV{XFIND_PATH})
#        message("XFIND_PATH (from env): ${XFIND_PATH}")
#else()
#        set(XFIND_PATH $ENV{HOME}/src/xfind)
#        message("XFIND_PATH (from home): ${XFIND_PATH}")
#endif()
#set(CPPFIND_PATH ${XFIND_PATH}/cpp/cppfind)
#
#add_directory(${CPPFIND_PATH})
#
#add_library(cppfind STATIC)
#
#target_sources(cppfind
#        PUBLIC
#        ${CPPFIND_PATH}/public/include/cppfind.h
#        PRIVATE
#        ${CPPFIND_PATH}/include/common.h ${CPPFIND_PATH}/src/common.cpp
#        ${CPPFIND_PATH}/include/config.h ${CPPFIND_PATH}/src/config.cpp
#        ${CPPFIND_PATH}/include/FileTypes.h ${CPPFIND_PATH}/src/FileTypes.cpp
#        ${CPPFIND_PATH}/include/FileUtil.h ${CPPFIND_PATH}/src/FileUtil.cpp
#        ${CPPFIND_PATH}/include/StringUtil.h ${CPPFIND_PATH}/src/StringUtil.cpp
#        ${CPPFIND_PATH}/include/FindException.h ${CPPFIND_PATH}/src/FindException.cpp
#        ${CPPFIND_PATH}/include/FindSettings.h ${CPPFIND_PATH}/src/FindSettings.cpp
#        ${CPPFIND_PATH}/include/FileResult.h ${CPPFIND_PATH}/src/FileResult.cpp
#        ${CPPFIND_PATH}/include/FindOption.h ${CPPFIND_PATH}/src/FindOption.cpp
#        ${CPPFIND_PATH}/include/FindOptions.h ${CPPFIND_PATH}/src/FindOptions.cpp
#        ${CPPFIND_PATH}/include/RegexPattern.h ${CPPFIND_PATH}/src/RegexPattern.cpp
#        ${CPPFIND_PATH}/include/Finder.h ${CPPFIND_PATH}/src/Finder.cpp)
#
#target_link_libraries(cppfind ${Boost_LIBRARIES})


################################################################################
# Library section
################################################################################

add_library(${PROJECT_NAME} STATIC)

# https://crascit.com/2016/01/31/enhanced-source-file-handling-with-target_sources/
target_sources(${PROJECT_NAME}
        PRIVATE
        include/SearchConfig.h src/SearchConfig.cpp
        include/SearchException.h src/SearchException.cpp
        include/SearchSettings.h src/SearchSettings.cpp
        include/SearchOption.h src/SearchOption.cpp
        include/SearchOptions.h src/SearchOptions.cpp
        include/SearchTextResult.h src/SearchTextResult.cpp
        include/SearchFileResult.h src/SearchFileResult.cpp
        include/SearchResultFormatter.h src/SearchResultFormatter.cpp
        include/SearchResultSorter.h src/SearchResultSorter.cpp
        include/Searcher.h src/Searcher.cpp)

target_link_libraries(${PROJECT_NAME} cppfind ${Boost_LIBRARIES})


################################################################################
# Executable section
################################################################################

add_executable(${PROJECT_NAME}app cppsearch.cpp)

target_link_libraries(${PROJECT_NAME}app ${PROJECT_NAME})


################################################################################
# Test dependencies section
################################################################################

Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v3.11.0
)

FetchContent_MakeAvailable(Catch2)


################################################################################
# Tests section
################################################################################

set(TEST_FILES
        tests/SearcherTest.cpp
        tests/SearchOptionsTest.cpp
        tests/SearchResultTest.cpp
        tests/SearchSettingsTest.cpp)

add_executable(${PROJECT_NAME}-tests ${TEST_FILES})

 target_link_libraries(${PROJECT_NAME}-tests ${PROJECT_NAME} Catch2::Catch2WithMain)

unset(XSEARCH_PATH CACHE)
