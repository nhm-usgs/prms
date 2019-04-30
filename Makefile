#
# Makefile --
#
# Top-level makefile for the PRMS
#

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

