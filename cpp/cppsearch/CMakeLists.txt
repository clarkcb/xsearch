cmake_minimum_required(VERSION 3.27.8)
project(cppsearch VERSION 0.1.0 LANGUAGES CXX)

#message("PROJECT_NAME: ${PROJECT_NAME}")
#message("PROJECT_SOURCE_DIR: ${PROJECT_SOURCE_DIR}")

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20")

set(Boost_USE_MULTITHREADED ON)
find_package(Boost REQUIRED COMPONENTS system filesystem)

if (DEFINED ENV{XSEARCH_PATH})
        set(XSEARCH_PATH $ENV{XSEARCH_PATH})
        message("XSEARCH_PATH (from env): ${XSEARCH_PATH}")
else()
        set(XSEARCH_PATH $ENV{HOME}/src/xsearch)
        message("XSEARCH_PATH (from home): ${XSEARCH_PATH}")
endif()

include_directories(${XSEARCH_PATH}/cpp/cppsearch/include)
include_directories(${Boost_INCLUDE_DIRS})

find_package(cppfind CONFIG REQUIRED)


################################################################################
# Library section
################################################################################

add_library(${PROJECT_NAME} STATIC)

# https://crascit.com/2016/01/31/enhanced-source-file-handling-with-target_sources/
target_sources(${PROJECT_NAME}
        PRIVATE
        include/config.h
        include/SearchException.h src/SearchException.cpp
        include/SearchSettings.h src/SearchSettings.cpp
        include/SearchOption.h src/SearchOption.cpp
        include/SearchOptions.h src/SearchOptions.cpp
        include/SearchResult.h src/SearchResult.cpp
        include/SearchResultFormatter.h src/SearchResultFormatter.cpp
        include/Searcher.h src/Searcher.cpp)

target_link_libraries(${PROJECT_NAME} cppfind ${Boost_LIBRARIES})


################################################################################
# Executable section
################################################################################

add_executable(${PROJECT_NAME}app cppsearch.cpp)

target_link_libraries(${PROJECT_NAME}app ${PROJECT_NAME})


################################################################################
# Testing section
################################################################################

Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v2.13.9
)

FetchContent_MakeAvailable(Catch2)

set(TEST_FILES
        tests/SearcherTest.cpp
        tests/SearchOptionsTest.cpp
        tests/SearchResultTest.cpp
        tests/SearchSettingsTest.cpp
        tests/tests-main.cpp)

add_executable(cppsearch-tests ${TEST_FILES})

target_link_libraries(cppsearch-tests ${PROJECT_NAME} Catch2::Catch2)

unset(XSEARCH_PATH CACHE)
