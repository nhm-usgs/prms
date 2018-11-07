## CMAKE Script for setting up the fortran compiling and linking flags for different operating systems and compilers
# All source code follows the free-form fortran format
enable_language(Fortran)

# This half-assed block is only necessary because CMAKE ignores overriding the
# *_INIT variables regardless of where they are placed in the scripts.
if (DEFINED CMAKE_Fortran_FLAGS_RELEASE_INIT AND
    "${CMAKE_Fortran_FLAGS_RELEASE_INIT}" STREQUAL "${CMAKE_Fortran_FLAGS_RELEASE}")
  # Overwrite the init values choosen by CMake
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3" CACHE STRING "" FORCE)
endif()

if (DEFINED CMAKE_Fortran_FLAGS_DEBUG_INIT AND
"${CMAKE_Fortran_FLAGS_DEBUG_INIT}" STREQUAL "${CMAKE_Fortran_FLAGS_DEBUG}")
  # Overwrite the init values choosen by CMake
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0" CACHE STRING "" FORCE)
endif()

# Check if linux
if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()

# Make sure the build type is uppercase
string(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

if(BT STREQUAL "RELEASE")
  set(CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose the type of build, options are DEBUG, or RELEASE." FORCE)
elseif(BT STREQUAL "DEBUG")
  set(CMAKE_BUILD_TYPE DEBUG CACHE STRING "Choose the type of build, options are DEBUG, or RELEASE." FORCE)
elseif(NOT BT)
  set(CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose the type of build, options are DEBUG, or RELEASE." FORCE)
  message(STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE.")
else()
  message(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, or RELEASE.")
endif()

# set (NETCDF_F90 "YES")
# find_package (NetCDF REQUIRED)
# include_directories(${NETCDF_INCLUDES})
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${NetCDF_includes}")
# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${NetCDF_libs}")
# message(STATUS "** NETCDF_INCLUDES: ${NetCDF_includes}")
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I/Users/pnorton/local/netcdf-4.6.1/include")
# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L/Users/pnorton/local/netcdf-4.6.1/lib -lnetcdff -L/Users/pnorton/local/netcdf-4.6.1/lib -L/Users/pnorton/local/udunits-2.2.20/lib -L/Users/pnorton/local/openmpi-1.10.2/lib -L/Users/pnorton/local/hdf-4.2.11/lib -L/Users/pnorton/local/jpeg-9b/lib -L/Users/pnorton/local/hdf5-1.8.17/lib -L/Users/pnorton/local/szip-2.1/lib -L/Users/pnorton/local/zlib-1.2.8/lib -L/Users/pnorton/local/libxml2-2.9.3/lib -L/Users/pnorton/local/curl-7.49.1/lib -lnetcdf")

# find_package(OpenMP)
# if(OPENMP_FOUND)
#   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
#   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
# endif()

# Set gfortran compile flags
# message(STATUS "*** Compiler: ${CMAKE_Fortran_COMPILER_ID}")

# ===============================================
# GNU
# ===============================================
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  message(STATUS "Getting gfortran flags")

  # Set flags for all build types
  # set(CMAKE_Fortran_FLAGS " -std=f2008 -ffree-line-length-none -fno-common -fall-intrinsics -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -ffree-line-length-none -fno-common -fall-intrinsics -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")

  if(BUILD_SHARED_LIBS)
    # Add any shared library related stuff here

    # if(NOT WIN32)
    if(APPLE)
      # Taken from: https://cmake.org/Wiki/CMake_RPATH_handling#Mac_OS_X_and_the_RPATH
      # use, i.e. don't skip the full RPATH for the build tree
      set(CMAKE_SKIP_BUILD_RPATH FALSE)

      # when building, don't use the install RPATH already (but later on when installing)
      set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

      set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

      # add the automatically determined parts of the RPATH
      # which point to directories outside the build tree to the install RPATH
      set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

      # the RPATH to be used when installing, but only if it's not a system directory
      list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
      if("${isSystemDir}" STREQUAL "-1")
         set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
      endif("${isSystemDir}" STREQUAL "-1")
    endif()
  else()
    # Static build options
    if(APPLE)
      # gcc on OS X defaults to the dynamic quadmath library instead of the static
      # NOTE: LIBRARY_PATH environment variable but be properly defined for the
      #   current compiler or find_library(quadmath) will return the static
      #   system version of libquadmath.a
      set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
      find_library(LIB_QUADMATH quadmath)
      message(STATUS "LIB_QUADMATH: ${LIB_QUADMATH}")
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran -static-libgcc -lgfortran -lgcc -lSystem -nodefaultlibs ${LIB_QUADMATH}")

      # Apple's ar and ranlib commands toss out 'no symbols' warnings
      # The following two lines quiets those warnings
      set(CMAKE_Fortran_ARCHIVE_CREATE "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
      set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
    elseif(${LINUX})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    endif()
  endif()

  # set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -funroll-all-loops -finline-functions")
  # set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -funroll-all-loops -finline-functions")

  #set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -pg -fbacktrace -fcheck=all -finit-real=nan -ffpe-trap=zero,overflow,underflow -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant")
  #set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -pg -fbacktrace -fcheck=all -ffpe-trap=zero,overflow,underflow -Wall  -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pg -fbacktrace -fcheck=all -ffpe-trap=zero,overflow,underflow -Wall -Wno-unused-dummy-argument")

  if(APPLE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-underscoring")
  endif()

# ===============================================
# INTEL
# ===============================================
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  message(STATUS "Getting ifort flags")

  # Set flags for all build types
  #set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

  if(WIN32)
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -nologo -fpp -O3 -heap-arrays1024 -QaxCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -Qipo -fp:fast=2 -Qdiag-disable:remark -Qmkl")
    set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -nologo -fpp -g -Od  -heap-arrays1024 -traceback -CB -Qfp-stack-check -Qmkl -warn:all -warn:nounused")
  endif()

  if(${LINUX})
    if(BUILD_SHARED_LIBS)
      # Add any shared library related stuff here
    else()
      # see: https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/753635
      # a better alternative is to use "-static-intel" which allows for dynamic system libraries
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    endif()

    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
  endif()

  if(APPLE)
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_RELEASE} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
  endif()
endif()


##
## Display information to the user
##
message(STATUS "Build type is ${CMAKE_BUILD_TYPE} use option -DCMAKE_BUILD_TYPE=[DEBUG RELEASE] to switch")

if(BT STREQUAL "RELEASE")
  message(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE}")
elseif(BT STREQUAL "DEBUG")
  message(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()
