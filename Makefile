#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile 5149 2012-12-19 20:05:12Z rsregan $
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: standard

#prms_ws: prmswatersmart

standard:
# Create lib directory, if necessary
#	@if [ ! -d $(LIBDIR) ]   ; then        \
#	  mkdir $(LIBDIR) ;                   \
#	  echo  Created directory $(LIBDIR) ; \
#	fi
# Create bin directory, if necessary
#	@if [ ! -d $(BINDIR) ]   ; then        \
#	  mkdir $(BINDIR) ;                   \
#	  echo  Created directory $(BINDIR) ; \
#	fi
	cd ./mmf; $(MAKE);
	cd ./prms; $(MAKE);

prmswatersmart:
# Create lib directory, if necessary
#	@if [ ! -d $(LIBDIR) ]   ; then        \
#	  mkdir $(LIBDIR) ;                   \
#	  echo  Created directory $(LIBDIR) ; \
#	fi
## Create bin directory, if necessary
#	@if [ ! -d $(BINDIR) ]   ; then        \
#	  mkdir $(BINDIR) ;                   \
#	  echo  Created directory $(BINDIR) ; \
#	fi
#	cd $(MMFDIR); $(MAKE);
#	cd $(PRMSTRUNK); $(MAKE);
#	cd $(PRMSBRANCH); $(MAKE) prms_ws;

clean:
	cd ./mmf; $(MAKE) clean;
	cd ./prms; $(MAKE) clean;
#	cd $(PRMSBRANCH); $(MAKE) clean;
#	$(RM) $(BINDIR)/prmsIV $(BINDIR)/prmsIV_ws *~

