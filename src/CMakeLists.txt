# set(CMAKE_Fortran_FLAGS_INIT "")
# set(CMAKE_Fortran_FLAGS_DEBUG_INIT "-O0 -g")
# set(CMAKE_Fortran_FLAGS_MINSIZEREL_INIT "-Os")
# set(CMAKE_Fortran_FLAGS_RELEASE_INIT "-O2")
# set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO_INIT "-O2 -g")
#
# set(CMAKE_C_FLAGS_INIT "")
# set(CMAKE_C_FLAGS_DEBUG_INIT "-O0 -g")
# set(CMAKE_C_FLAGS_MINSIZEREL_INIT "-Os")
# set(CMAKE_C_FLAGS_RELEASE_INIT "-O2")
# set(CMAKE_C_FLAGS_RELWITHDEBINFO_INIT "-O2 -g")
#
# set(CMAKE_CXX_FLAGS_INIT "")
# set(CMAKE_CXX_FLAGS_DEBUG_INIT "-O0 -g")
# set(CMAKE_CXX_FLAGS_MINSIZEREL_INIT "-Os")
# set(CMAKE_CXX_FLAGS_RELEASE_INIT "-O2")
# set(CMAKE_CXX_FLAGS_RELWITHDEBINFO_INIT "-O2 -g")

cmake_minimum_required(VERSION 3.6)
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
