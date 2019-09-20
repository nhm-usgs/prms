
# - Find NetCDF
# Find the native NetCDF includes and library
#
#  NETCDF_INCLUDE_DIR  - user modifiable choice of where netcdf headers are
#  NETCDF_LIBRARY      - user modifiable choice of where netcdf libraries are
#
# Your package can require certain interfaces to be FOUND by setting these
#
#  NETCDF_CXX         - require the C++ interface and link the C++ library
#  NETCDF_F77         - require the F77 interface and link the fortran library
#  NETCDF_F90         - require the F90 interface and link the fortran library
#
# Or equivalently by calling FindNetCDF with a COMPONENTS argument containing one or
# more of "CXX;F77;F90".
#
# When interfaces are requested the user has access to interface specific hints:
#
#  NETCDF_${LANG}_INCLUDE_DIR - where to search for interface header files
#  NETCDF_${LANG}_LIBRARY     - where to search for interface libraries
#
# This module returns these variables for the rest of the project to use.
#
#  NETCDF_FOUND          - True if NetCDF found including required interfaces (see below)
#  NETCDF_LIBRARIES      - All netcdf related libraries.
#  NETCDF_INCLUDE_DIRS   - All directories to include.
#  NETCDF_HAS_INTERFACES - Whether requested interfaces were found or not.
#  NETCDF_${LANG}_INCLUDE_DIRS/NETCDF_${LANG}_LIBRARIES - C/C++/F70/F90 only interface
#
# Normal usage would be:
#  set (NETCDF_F90 "YES")
#  find_package (NetCDF REQUIRED)
#  target_link_libraries (uses_everthing ${NETCDF_LIBRARIES})
#  target_link_libraries (only_uses_f90 ${NETCDF_F90_LIBRARIES})



message(STATUS "Entered FindNetCDF.cmake")
message(STATUS "NETCDF_DIR: $ENV{NETCDF_DIR}")

set(NETCDF_ROOT $ENV{NETCDF_DIR})
mark_as_advanced(NETCDF_ROOT)

message(STATUS "NETCDF_ROOT: ${NETCDF_ROOT}")

set(NETCDF_HAS_INTERFACES "YES")

if (NETCDF_ROOT)
  find_program(netcdf_config nc-config
               HINTS ${NETCDF_ROOT}
               PATH_SUFFIXES bin
               DOC "NetCDF configuration script")

  message(STATUS "netcdf_config: ${netcdf_config}")

  set(NETCDF_LIBRARIES "")
  set(NETCDF_INCLUDE_DIRS "")

  if (netcdf_config)
  # if (netcdf_config AND (NOT (${CMAKE_SYSTEM_NAME} matches "Windows")))
    # Don't use the --fflags for the include directory; this confuses cmake
    # because of the '-I'  part.
    message(STATUS "Found netCDF configuration script: ${netcdf_config}")
    execute_process(COMMAND "${netcdf_config}" "--includedir"
                    RESULT_VARIABLE _ret_code
                    OUTPUT_VARIABLE _stdout
                    ERROR_VARIABLE _stderr)
    string(REGEX REPLACE "[\n\r]" "" _fflags_answer ${_stdout})
    # message(STATUS "${netcdf_config} --includedir has returned: '${_fflags_answer}'")

    execute_process(COMMAND "${netcdf_config}" "--flibs"
                    RESULT_VARIABLE _ret_code
                    OUTPUT_VARIABLE _stdout
                    ERROR_VARIABLE _stderr)
    string(REGEX REPLACE "[\n\r]" "" _flibs_answer ${_stdout})
    # message(STATUS "${netcdf_config} --flibs has returned: '${_flibs_answer}'")

    # list(APPEND NetCDF_libs ${NETCDF_${lang}_LIBRARY})
    list(APPEND NETCDF_LIBRARIES ${_flibs_answer})
    list(APPEND NETCDF_INCLUDE_DIRS ${_fflags_answer})
    # set(NETCDF_LIBRARIES "${_flibs_answer}")
    # set(NETCDF_INCLUDE_DIRS ${_fflags_answer})

    # Handle netcdf-c and and netcdf-fortran built separately
    find_library(netcdf_fortran netcdff)
    if (netcdf_fortran AND (NOT DEFINED ${_flibs_answer}))
      execute_process(COMMAND "${netcdf_config}" "--libs"
        RESULT_VARIABLE _ret_code
        OUTPUT_VARIABLE _stdout
        ERROR_VARIABLE _stderr)
      string(REGEX REPLACE "[\n\r]" "" _libs_answer ${_stdout})
      set(NETCDF_LIBRARIES "${_libs_answer} -lnetcdff")
    endif()
  endif()

  mark_as_advanced(NETCDF_LIBRARIES)
  mark_as_advanced(NETCDF_INCLUDE_DIRS)
endif()

# Standard CMake modules see CMAKE_ROOT/Modules
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDE_DIRS NETCDF_HAS_INTERFACES)

# Search starting from user editable cache var
# if (NETCDF_INCLUDE_DIR AND NETCDF_LIBRARY)
#   # Already in cache, be silent
#   set (NETCDF_FIND_QUIETLY TRUE)
# endif ()

# set(USE_DEFAULT_PATHS "NO_DEFAULT_PATH")
# if(NETCDF_USE_DEFAULT_PATHS)
#   set(USE_DEFAULT_PATHS "")
# endif()

# find_path(NETCDF_INCLUDE_DIR netcdf.h PATHS "$ENV{NETCDF_DIR}/include")
# mark_as_advanced(NETCDF_INCLUDE_DIR)

# set(NETCDF_C_INCLUDE_DIRS ${NETCDF_INCLUDE_DIR})

# find_library(NETCDF_LIBRARY NAMES netcdf PATHS "$ENV{NETCDF_DIR}/lib"
#              HINTS "${NETCDF_INCLUDE_DIR}/../lib")
# mark_as_advanced(NETCDF_LIBRARY)

# set(NETCDF_C_LIBRARIES ${NETCDF_LIBRARY})

# # Start finding requested language components
# set(NetCDF_libs "")
# set(NetCDF_includes "${NETCDF_INCLUDE_DIR}")

# get_filename_component(NetCDF_lib_dirs "${NETCDF_LIBRARY}" DIRECTORY)
# set(NETCDF_HAS_INTERFACES "YES") # Will be set to NO if we're missing any interfaces

# macro(NetCDF_check_interface lang header libs)
#   if (NETCDF_${lang})
#     # Search starting from user modifiable cache var
#     find_path(NETCDF_${lang}_INCLUDE_DIR NAMES ${header}
#               HINTS "${NETCDF_INCLUDE_DIR}"
#               HINTS "${NETCDF_${lang}_ROOT}/include"
#               ${USE_DEFAULT_PATHS})

#     find_library(NETCDF_${lang}_LIBRARY NAMES ${libs}
#                  HINTS "${NetCDF_lib_dirs}"
#                  HINTS "${NETCDF_${lang}_ROOT}/lib"
#                  ${USE_DEFAULT_PATHS})

#     mark_as_advanced(NETCDF_${lang}_INCLUDE_DIR NETCDF_${lang}_LIBRARY)

#     # Export to internal varS that rest of project can use directly
#     set(NETCDF_${lang}_LIBRARIES ${NETCDF_${lang}_LIBRARY})
#     set(NETCDF_${lang}_INCLUDE_DIRS ${NETCDF_${lang}_INCLUDE_DIR})

#     if (NETCDF_${lang}_INCLUDE_DIR AND NETCDF_${lang}_LIBRARY)
#       list(APPEND NetCDF_libs ${NETCDF_${lang}_LIBRARY})
#       list(APPEND NetCDF_includes ${NETCDF_${lang}_INCLUDE_DIR})
#     else ()
#       set(NETCDF_HAS_INTERFACES "NO")
#       message(STATUS "Failed to find NetCDF interface for ${lang}")
#     endif ()
#   endif ()
# endmacro ()

# list(FIND NetCDF_FIND_COMPONENTS "CXX" _nextcomp)
# if (_nextcomp GREATER -1)
#   set(NETCDF_CXX 1)
# endif ()

# list(FIND NetCDF_FIND_COMPONENTS "F77" _nextcomp)
# if (_nextcomp GREATER -1)
#   set(NETCDF_F77 1)
# endif ()

# list(FIND NetCDF_FIND_COMPONENTS "F90" _nextcomp)
# if (_nextcomp GREATER -1)
#   set(NETCDF_F90 1)
# endif ()

# NetCDF_check_interface(CXX netcdfcpp.h netcdf_c++)
# NetCDF_check_interface(F77 netcdf.inc netcdff)
# NetCDF_check_interface(F90 netcdf.mod netcdff)

# # Export accumulated results to internal varS that rest of project can depend on
# list(APPEND NetCDF_libs "${NETCDF_C_LIBRARIES}")
# set(NETCDF_LIBRARIES ${NetCDF_libs})
# set(NETCDF_INCLUDE_DIRS ${NetCDF_includes})

# # Handle the QUIETLY and REQUIRED arguments and set NETCDF_FOUND to TRUE if
# # all listed variables are TRUE.
# include(FindPackageHandleStandardArgs)
# find_package_handle_standard_args(NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDE_DIRS NETCDF_HAS_INTERFACES)








# if (NetCDF_FOUND)
#   add_library(NetCDF UNKNOWN IMPORTED)
#   set_target_properties(NetCDF PROPERTIES IMPORTED_LOCATION ${NETCDF_LIBRARIES}
#                                           IMPORTED_LINK_INTERFACE_LANGUAGES "Fortran"
#                                           IMPORTED_LINK_INTERFACE_LIBRARIES netdff netcdf)
  # if(NOT TARGET NetCDF)
  #   add_library(NetCDF UNKNOWN IMPORTED)
  #   set_target_properties(NetCDF PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${NETCDF_INCLUDE_DIRS}")
  #
  # endif()
# endif()


# https://cmake.org/pipermail/cmake/2010-September/039639.html
# If you want to do
#
#   target_link_libraries(mylib /path/to/libother.a)
#
# but do not want "/path/to/libother.a" to appear in the transitive dep
# list of mylib's target file, then do this:
#
#   add_library(other STATIC IMPORTED)
#   set_property(TARGET other PROPERTY IMPORTED_LOCATION /path/to/libother.a)
#   target_link_libraries(mylib other)
