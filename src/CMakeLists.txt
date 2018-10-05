cmake_minimum_required(VERSION 3.8)
project(prms_project LANGUAGES Fortran)

# SET(VERSION x.x.x)
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_SOURCE_DIR: " ${CMAKE_SOURCE_DIR})

# Add our local modules to the module path
if(NOT CMAKE_MODULE_PATH)
  set(CMAKE_MODULE_PATH)
endif()
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../config/cmake")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

include("${CMAKE_MODULE_PATH}/BuildType.cmake")

# GNUInstallDirs is used to install libraries into correct locations
# on all platforms.
include(GNUInstallDirs)

# set the Fortran compile flags
include(${CMAKE_MODULE_PATH}/FortranEnvironment.cmake)

# Add the prms library source directory
add_subdirectory(${CMAKE_SOURCE_DIR}/prmslib)

# Add the directory which contains the source for the prms executable
add_subdirectory(${CMAKE_SOURCE_DIR}/prms)

# Add a distclean target to the Makefile
# ADD_CUSTOM_TARGET(distclean
#     COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
