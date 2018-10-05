
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
  target_link_libraries(${PROJECT_NAME} PUBLIC ${NETCDF_F90_LIBRARIES})
endif ()

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
