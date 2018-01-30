cmake_minimum_required (VERSION 3.6)
project (prms)
enable_language (Fortran)

# Setup the Fortran cmake environment
include(../../coretran/myCMakeFiles/FortranEnvironment.cmake)

# Initialize the library with no files added yet
set(libName prmslib)

link_directories(../../coretran/lib)

# Create the library
add_library(${libName} ${libType} "")

# Link in the library
target_link_libraries(${libName} coretran)

# Add the include folder that contains the library modules
target_include_directories(${libName} PUBLIC ../../coretran/include)

# Add files to the library as sources
# Each CMakeLists.txt simply uses target_sources to add the source files
# Core
target_sources(${libName}
	PUBLIC
        "${CMAKE_CURRENT_LIST_DIR}/base_datatypes.f90"
        "${CMAKE_CURRENT_LIST_DIR}/basin.f90"
        "${CMAKE_CURRENT_LIST_DIR}/basin_summary.f90"
        "${CMAKE_CURRENT_LIST_DIR}/climate_hru.f90"
        "${CMAKE_CURRENT_LIST_DIR}/climateflow.f90"
        "${CMAKE_CURRENT_LIST_DIR}/constants.f90"
        "${CMAKE_CURRENT_LIST_DIR}/control_mod.f90"
        "${CMAKE_CURRENT_LIST_DIR}/conversions.f90"
        "${CMAKE_CURRENT_LIST_DIR}/ddsolrad.f90"
        "${CMAKE_CURRENT_LIST_DIR}/dimensions_mod.f90"
        "${CMAKE_CURRENT_LIST_DIR}/file_io.f90"
        "${CMAKE_CURRENT_LIST_DIR}/m_prms6.f90"
        "${CMAKE_CURRENT_LIST_DIR}/nhru_summary.f90"
        "${CMAKE_CURRENT_LIST_DIR}/obs.f90"
        "${CMAKE_CURRENT_LIST_DIR}/parameter_mod.f90"
        "${CMAKE_CURRENT_LIST_DIR}/potet_jh.f90"
        "${CMAKE_CURRENT_LIST_DIR}/prms_module.f90"
        "${CMAKE_CURRENT_LIST_DIR}/prms_time.f90"
        "${CMAKE_CURRENT_LIST_DIR}/read_control_file.f90"
        "${CMAKE_CURRENT_LIST_DIR}/read_data_file.f90"
        "${CMAKE_CURRENT_LIST_DIR}/read_parameter_file.f90"
        "${CMAKE_CURRENT_LIST_DIR}/soltab.f90"
        "${CMAKE_CURRENT_LIST_DIR}/time.f90"
        "${CMAKE_CURRENT_LIST_DIR}/transp_tindex.f90"
        "${CMAKE_CURRENT_LIST_DIR}/utils_prms.f90"
        "${CMAKE_CURRENT_LIST_DIR}/variables_mod.f90"
)




## Create the PRMS program
# Add the linking directory
link_directories(../lib ../../coretran/lib)

# Initialize the program
set(programName prms)
add_executable(${programName} "${CMAKE_CURRENT_LIST_DIR}/Main_prms.f90")
# Link in the library
target_link_libraries(${programName} prmslib)
target_link_libraries(${programName} coretran)
# Add the include folder that contains the library modules
target_include_directories(${programName} PUBLIC ../include ../../coretran/include)




