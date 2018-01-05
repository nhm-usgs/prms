#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([prms6], [1.0], [pnorton@usgs.gov])

AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs.
#AC_PROG_F77
AC_PROG_RANLIB
AC_PROG_AWK
AC_PROG_INSTALL
AM_PROG_AR

# Check for fortran compiler
#AC_LANG(Fortran)
AC_PROG_FC([ifort gfortran])
AC_FC_FREEFORM
AC_FC_SRCEXT([f90])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# FCFLAGS="-Wall -fbacktrace -fimplicit-none"


# Intel ifort
# -diag-disable 5268 disables warning about lines longer than 132 characters
FCFLAGS="-g -traceback -debug extended -gen-interfaces -warn interfaces -check all -diag-disable 5268 -fp-model precise -free -std

# gfortran
#FCFLAGS="-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan
AC_SYS_LARGEFILE

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
