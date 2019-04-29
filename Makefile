#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile 6950 2014-10-30 22:50:23Z rsregan $
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: standard

#prms_ws: prmsdynwu

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

#md:
#	cd $(MMFDIR); $(MAKE);
#	cd $(PRMSTRUNKMD); $(MAKE);

#prmsdynwu:
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
#	cd $(PRMSTRUNKMD); $(MAKE);
#	cd $(PRMSBRANCH); $(MAKE) prms_dynwu

clean:
	cd ./mmf; $(MAKE) clean;
	cd ./prms; $(MAKE) clean;
#	cd $(PRMSTRUNKMD); $(MAKE) clean;
#	cd $(PRMSBRANCH); $(MAKE) clean;
#	$(RM) $(BINDIR)/prmsIV $(BINDIR)/prmsIV_ws *~

