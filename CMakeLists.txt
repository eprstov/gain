cmake_minimum_required(VERSION 3.6)

cmake_policy( SET CMP0069 NEW )

file( GLOB_RECURSE HEADERS LIST_DIRECTORIES false include/gain/* )
file( GLOB_RECURSE SOURCES LIST_DIRECTORIES false src/* )

add_library( gain ${HEADERS} ${SOURCES} )
set_target_properties( gain PROPERTIES LINKER_LANGUAGE CXX )
target_include_directories( gain PUBLIC include )
target_sources( gain PUBLIC FILE_SET HEADERS BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include FILES ${HEADERS} )

if( NOT GAIN_SKIP_INSTALL )
	install( TARGETS gain FILE_SET HEADERS DESTINATION include )
endif()

source_group( TREE ${CMAKE_CURRENT_SOURCE_DIR}/include/gain PREFIX \\ FILES ${HEADERS} )
source_group( TREE ${CMAKE_CURRENT_SOURCE_DIR}/src PREFIX \\ FILES ${SOURCES} )

