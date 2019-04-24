#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile 3270 2011-05-24 20:06:13Z rsregan $
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: standard

standard:
	cd ./mmf; $(MAKE);
	cd ./prms; $(MAKE);

clean:
	cd ./mmf; $(MAKE) clean;
	cd ./prms; $(MAKE) clean;
