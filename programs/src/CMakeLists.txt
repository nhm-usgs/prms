

cmake_minimum_required (VERSION 3.6)
project (prms)
enable_language (Fortran)

# Setup the Fortran cmake environment
include(../../FortranEnvironment.cmake)

## Create the PRMS program
# Add the linking directory
link_directories(../../lib ../../../coretran/lib)

# Initialize the program
set(programName prms)
add_executable(${programName} ../../src/Main_prms.f90)
# Link in the library
target_link_libraries(${programName} prmslib coretran)
# Add the include folder that contains the library modules
target_include_directories(${programName} PUBLIC ../../include ../../../coretran/include)





