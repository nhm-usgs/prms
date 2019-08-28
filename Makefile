#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: prmsglrip prmsgl

prmsglrip:
# Create lib directory, if necessary
	@if [ ! -d $(MMFDIR) ]   ; then        \
	  mkdir $(MMFDIR) ;                   \
	  echo  Created directory $(MMFDIR) ; \
	fi
# Create bin directory, if necessary
	@if [ ! -d $(BINDIR) ]   ; then        \
	  mkdir $(BINDIR) ;                   \
	  echo  Created directory $(BINDIR) ; \
	fi
	cd $(MMFDIR); $(MAKE);
	cd $(MIZUDIR); $(MAKE);
	cd $(PRMSRDIR); $(MAKE);

prmsgl:
# Create lib directory, if necessary
	@if [ ! -d $(MMFDIR) ]   ; then        \
	  mkdir $(MMFDIR) ;                   \
	  echo  Created directory $(MMFDIR) ; \
	fi
# Create bin directory, if necessary
	@if [ ! -d $(BINDIR) ]   ; then        \
	  mkdir $(BINDIR) ;                   \
	  echo  Created directory $(BINDIR) ; \
	fi
	cd $(MMFDIR); $(MAKE);
	cd $(MIZUDIR); $(MAKE);
	cd $(PRMSDIR); $(MAKE);


clean:
	cd $(MMFDIR); $(MAKE) clean;
	cd $(MIZUDIR); $(MAKE) clean;
	cd $(PRMSDIR); $(MAKE) clean;
	cd $(PRMSRDIR); $(MAKE) clean;
	$(RM) $(BINDIR)/prms*
