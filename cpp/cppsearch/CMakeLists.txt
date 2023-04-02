cmake_minimum_required(VERSION 3.15)
project(cppsearch)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

set(Boost_USE_MULTITHREADED ON)
find_package(Boost REQUIRED COMPONENTS system filesystem)

if (DEFINED ENV{XSEARCH_PATH})
        set(XSEARCH_PATH $ENV{XFIND_PATH})
        message("XSEARCH_PATH: ${XSEARCH_PATH}")
else()
        set(XSEARCH_PATH $ENV{HOME}/src/xsearch)
        message("XSEARCH_PATH: ${XSEARCH_PATH}")
endif()

include_directories(/Users/cary/src/xsearch/cpp/cppsearch/include)
include_directories(${Boost_INCLUDE_DIRS})

set(SOURCE_FILES include/common.h src/common.cpp
        include/config.h
        include/FileTypes.h src/FileTypes.cpp
        include/FileUtil.h src/FileUtil.cpp
        include/StringUtil.h src/StringUtil.cpp
        include/SearchException.h src/SearchException.cpp
        include/SearchSettings.h src/SearchSettings.cpp
        include/SearchFile.h src/SearchFile.cpp
        include/SearchOption.h src/SearchOption.cpp
        include/SearchOptions.h src/SearchOptions.cpp
        include/SearchPattern.h src/SearchPattern.cpp
        include/SearchResult.h src/SearchResult.cpp
        include/SearchResultFormatter.h src/SearchResultFormatter.cpp
        include/Searcher.h src/Searcher.cpp)

add_library(xsearch ${SOURCE_FILES})

target_link_libraries(xsearch ${Boost_LIBRARIES})

add_executable(cppsearch cppsearch.cpp)

target_link_libraries(cppsearch xsearch)


## Testing section

Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v2.13.4
)

FetchContent_MakeAvailable(Catch2)

set(TEST_FILES tests/FileTypesTest.cpp
        tests/FileUtilTest.cpp
        tests/SearcherTest.cpp
        tests/SearchFileTest.cpp
        tests/SearchOptionsTest.cpp
        tests/SearchResultTest.cpp
        tests/SearchSettingsTest.cpp
        tests/tests-main.cpp)

add_executable(cppsearch-tests ${TEST_FILES})

target_link_libraries(cppsearch-tests xsearch Catch2::Catch2)

unset(XSEARCH_PATH CACHE)
