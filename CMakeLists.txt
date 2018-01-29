cmake_minimum_required (VERSION 3.5)
project(prms6)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-line-length-0 -ffpe-trap=zero,overflow,underflow -finit-real=nan -std=f2008")
    set(bounds "-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -fcheck=all -fbounds-check")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-fp-model precise -free -std08")
    set(bounds "-g -traceback -debug extended -gen-interfaces -warn interfaces -check all -diag-disable 5268 -check bounds")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

# Compile everything
file(GLOB_RECURSE sources src/*.f90)
add_executable(prms6 ${sources})

