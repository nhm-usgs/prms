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
	cd $(MMFDIR); $(MAKE);
	cd $(MIZU); $(MAKE);
	cd $(PRMSDIR); $(MAKE);

clean:
	cd $(MMFDIR); $(MAKE) clean;
	cd $(MIZU); $(MAKE); clean;
	cd $(PRMSDIR); $(MAKE) clean;

