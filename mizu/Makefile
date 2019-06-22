#
# Makefile for mizuRoute
#

include ../makelist

# Define subroutines
#
# data types
DATATYPES = \
		nrtype.f90 \
		reachparam.f90 \
		reachstate.f90 \
		reach_flux.f90 \
		lake_param.f90 \
		lakes_flux.f90
#
# define utilities
UTILS = \
		nrutil.f90 \

# perform model routing
QROUTE = \
		kwt_route.f90

# concatanate model subroutines
MODSUB = $(DATATYPES) $(UTILS) $(QROUTE)

MIZUOBJS = ${MODSUB:.f90=.o}

#
# Standard Targets for Users
#

all: compile install makelib

# compile
compile:
# Create inc directory, if necessary
	@if [ ! -d $(MIZUDIR)/include ]   ; then        \
   	   mkdir $(MIZUDIR)/include ;                   \
	   echo  Created directory $(MIZUDIR)/include ; \
	fi
	$(FC) $(FFLAGS) $(MODSUB) -c 

makelib:
# Create lib directory, if necessary
	@if [ ! -d $(LIBDIR) ]   ; then        \
   	   mkdir $(LIBDIR) ;                   \
	   echo  Created directory $(LIBDIR) ; \
	fi
	$(AR) $(MIZULIB) $(MIZUOBJS)
	$(RANLIB) $(MIZULIB)

# Copy the .mod files to the include directory
install:
		mv *.mod $(MIZUDIR)/include

clean:
	$(RM) $(MIZULIB) $(MIZUOBJS)
	$(RM) $(MIZUDIR)/include/*.mod
