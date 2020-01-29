project(prmslib VERSION 0.1 LANGUAGES Fortran)
cmake_minimum_required(VERSION 3.8)

# Create the library
# ${PROJECT_NAME} is the name given for the most recent project() call
add_library(${PROJECT_NAME} "")

# Link in the library
set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE Fortran)

find_package(coretran REQUIRED CONFIG)
target_link_libraries(${PROJECT_NAME} PUBLIC coretran)

find_package(NetCDF REQUIRED COMPONENTS "F90")

if (NOT BUILD_SHARED_LIBS)
  # Linking fails for static build with absolute paths on OSX
  target_link_libraries(${PROJECT_NAME} PUBLIC netcdff netcdf)
else ()
  #target_link_libraries(${PROJECT_NAME} PUBLIC ${NETCDF_F90_LIBRARIES})
  target_link_libraries(${PROJECT_NAME} PUBLIC netcdff netcdf)
endif ()

# # Find the BLAS stuff
# list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# set(BLA_DEFINITIONS)

# if ("$ENV{MKLROOT}" STREQUAL "")
#     message(FATAL_ERROR "MKLROOT is not set. Please source the Intel MKL mklvars.sh file.")
# endif()

# # user defined options for MKL
# option(MKL_USE_parallel "Use MKL parallel" True)
# option(MKL_USE_sdl "Single Dynamic Library or static/dynamic" False)
# set(MKL_USE_interface "lp64" CACHE STRING "for Intel(R)64 compatible arch: ilp64/lp64 or for ia32 arch: cdecl/stdcall")

# if (BLA_VENDOR MATCHES "_seq")
#     set(MKL_USE_parallel OFF)
# else()
#     set(MKL_USE_parallel ON)
# endif()

# find_package(MKL REQUIRED)

# if (MKL_FOUND)
#     set(BLA_INCLUDE_DIR ${MKL_INCLUDE_DIR})
#     set(BLAS_LIBRARIES ${MKL_LIBRARIES})
#     set(BLA_DEFINITIONS USE_MKL)
# endif()



# # Tell CMake to use rpath with the libs we build
# if (APPLE)
#     set(CMAKE_MACOSX_RPATH OFF)
#     set(CMAKE_SKIP_RPATH ON)
#     # set(CMAKE_SKIP_BUILD_RPATH FALSE)
#     # set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
#     # # set(CMAKE_INSTALL_RPATH "@executable_path/../lib/")
#     # set(CMAKE_INSTALL_RPATH "@loader_path/.")

#     # set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
#     # list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
#     # if("${isSystemDir}" STREQUAL "-1")
#     #     set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
#     # endif()
# elseif(UNIX)  # means if LINUX
#     # to fix the installed rpath so it looks in ../lib
#     # https://www.semipol.de/2012/02/16/relative-rpath-settings-with-cmake.html
#     set(CMAKE_INSTALL_RPATH "$ORIGIN/../lib")
# endif()

# message(STATUS "BLAS Libraries: ${BLAS_LIBRARIES}")
# target_link_libraries(${PROJECT_NAME} PUBLIC ${BLAS_LIBRARIES})
# target_include_directories(${PROJECT_NAME} PRIVATE ${BLA_INCLUDE_DIR})



# if (NOT BUILD_SHARED_LIBS)
#   # Linking fails for static build with absolute paths on OSX
#   target_link_libraries(${PROJECT_NAME} PUBLIC netcdff netcdf)
# else ()
#   target_link_libraries(${PROJECT_NAME} PUBLIC ${NETCDF_F90_LIBRARIES})
# endif ()
# target_link_libraries(${PROJECT_NAME} PUBLIC ${NETCDF_LIBRARIES})
# target_include_directories(${PROJECT_NAME} PRIVATE ${NETCDF_INCLUDE_DIRS})
target_include_directories(${PROJECT_NAME} PUBLIC ${NETCDF_INCLUDE_DIRS})

# # Link in the library
# set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE Fortran)

# Add the include folder that contains the library modules
target_include_directories(${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
                                                  $<INSTALL_INTERFACE:include>
                                                  $<INSTALL_INTERFACE:lib>
                                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

include(classes/CMakeLists.txt)
include(io/CMakeLists.txt)
include(misc/CMakeLists.txt)
include(physics/CMakeLists.txt)

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS ${PROJECT_NAME} EXPORT prmslib-config
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    INCLUDES DESTINATION include)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION include
    FILES_MATCHING PATTERN "*.mod"
    PATTERN "CMakeFiles" EXCLUDE)

install(EXPORT prmslib-config DESTINATION lib/cmake)
export(TARGETS ${PROJECT_NAME} FILE prmslib-config.cmake)
#    RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})  # This is for Windows
# This makes the project importable from the build directory
#export(TARGETS lib FILE MyLibraryConfig.cmake)