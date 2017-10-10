cmake_minimum_required(VERSION 3.7)
project(cppsearch)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")


set(Boost_USE_MULTITHREADED ON)
find_package(Boost REQUIRED COMPONENTS system filesystem)

include_directories(/Users/cary/src/xsearch/cpp/cppsearch/include)
include_directories(${Boost_INCLUDE_DIRS})

set(SOURCE_FILES include/common.h src/common.cpp include/config.h
        include/FileTypes.h src/FileTypes.cpp include/FileUtil.h src/FileUtil.cpp
        include/StringUtil.h src/StringUtil.cpp include/SearchException.h src/SearchException.cpp
        include/SearchSettings.h src/SearchSettings.cpp include/SearchFile.h src/SearchFile.cpp
        include/SearchOption.h src/SearchOption.cpp include/SearchOptions.h src/SearchOptions.cpp
        include/SearchPattern.h src/SearchPattern.cpp include/SearchResult.h src/SearchResult.cpp
        include/Searcher.h src/Searcher.cpp cppsearch.cpp)

add_executable(cppsearch ${SOURCE_FILES})

target_link_libraries(cppsearch ${Boost_LIBRARIES})