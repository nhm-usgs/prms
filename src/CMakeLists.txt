

cmake_minimum_required (VERSION 3.6)
project (prms)
enable_language (Fortran)

# Setup the Fortran cmake environment
include(../FortranEnvironment.cmake)


# Initialize the library with no files added yet
set(libName prmslib)

link_directories(../../coretran/lib)

# Create the library
add_library(${libName} ${libType} "")

# Link in the library
target_link_libraries(${libName} coretran)

# Add the include folder that contains the library modules
target_include_directories(${libName} PUBLIC ../../coretran/include)

include(classes/CMakeLists.txt)
include(io/CMakeLists.txt)
include(misc/CMakeLists.txt)
include(physics/CMakeLists.txt)

