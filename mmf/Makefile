#
# Makefile for mmf
#

MOWSDIR = ./
MLIBDIR = $(MOWSDIR)
MMFLIB = $(MLIBDIR)/libmmf.a
CC      = gcc
RM      = rm -f
AR      = ar ruv
RANLIB  = ranlib

CFLAGS  = -O -Wall -Werror-implicit-function-declaration

# Archive properties
SHA_MMF:="$(shell git rev-parse HEAD)"
ORIGIN_MMF:="$(shell git config --get remote.origin.url)"
TAG_MMF:="$(shell git tag)"
COMMITDATE_MMF:="$(shell git log -1 --format=%cd)"
CVER:="$(shell gcc --version | head -n1)"
SVER:="$(shell uname -a)"
#WINNAME=`cmd //c systeminfo | sed -n '/^OS Name/p'`
#WINVER=`cmd //c systeminfo | sed -n '/^OS Version/p'`
#WINTYPE=`cmd //c systeminfo | sed -n '/^System Type/p'`
BUILDER:="$(shell whoami)"
BUILDDATE:="$(shell date)"

DFLAGS = -D'SHA_MMF=$(SHA_MMF)' -D'ORIGIN_MMF=$(ORIGIN_MMF)' -D'TAG_MMF=$(TAG_MMF)' -D'COMMITDATE_MMF=$(COMMITDATE_MMF)' -D'SVER=$(SVER)' -D'BUILDER=$(BUILDER)' -D'BUILDDATE=$(BUILDDATE)' -D'CFLAGS="$(CFLAGS)"' -D'SHA_PRMS="$(SHA_PRMS)"' -D'ORIGIN_PRMS="$(ORIGIN_PRMS)"' -D'TAG_PRMS="$(TAG_PRMS)"' -D'COMMITDATE_PRMS="$(COMMITDATE_PRMS)"' -D'COMPILER=$(CVER)' -D'FFLAGS="$(FFLAGS)"' -D'LDFLAGS="$(LDFLAGS)"'

SRCS = 	mmf.c parse_args.c alloc_space.c build_lists.c \
	setup_cont.c decl_control.c control_addr.c \
	control_var.c read_params.c sort_dims.c sort_params.c sort_vars.c \
	var_addr.c declvar.c str_to_vals.c\
	declparam.c param_addr.c getdim.c timing.c getparam.c umalloc_etc.c \
	julday.c getvar.c julconvert.c readvar.c decldim.c \
	get_times.c batch_run.c read_control.c dim_addr.c reset_dim.c read_line.c \
	get_elem_add.c read_vars.c getdimname.c \
	save_params.c load_param.c check_vars.c \
	create_vstats.c free_vstats.c write_vstats.c \
	call_modules.c call_setdims.c \
	read_datainfo.c putvar.c print_params.c print_vars.c \
	print_model_info.c batch_run_functions.c graph_single_run.c \
	control_array.c call_setdims.c call_modules.c


MMFOBJS = ${SRCS:.c=.o}

.c.o:
	$(CC) $(CFLAGS) $(DFLAGS) -c $<

#
# Private makefile defines
#
MMSLIB = $(MLIBDIR)/libmmf.a

#
# Standard Targets for Users
#
all: $(MMFLIB)

print:
	@echo "SHA_MMF:  " $(SHA_MMF)
	@echo "SHA_PRMS: " $(SHA_PRMS)
	@echo "ORIGIN_MMF:  " $(ORIGIN_MMF)
	@echo "ORIGIN_PRMS: " $(ORIGIN_PRMS)
	@echo "TAG_MMF:  " $(TAG_MMF)
	@echo "TAG_PRMS: " $(TAG_PRMS)
	@echo "COMMITDATE_MMF:  " $(COMMITDATE_MMF)
	@echo "COMMITDATE_PRMS: " $(COMMITDATE_PRMS)
	@echo "COMPILER: " $(CVER)
	@echo "OS Version: " $(SVER)
	@echo "BUILDER: " $(BUILDER)
	@echo "BUILDDATE: " $(BUILDDATE)
	@echo "CFLAGS:  " $(CFLAGS)
	@echo "FFLAGS:  " $(FFLAGS)
	@echo "LDFLAGS: " $(LDFLAGS)


$(MMFLIB): $(MMFOBJS)
# Create lib directory, if necessary
	@if [ ! -d $(LIBDIR) ]   ; then        \
   	   mkdir $(LIBDIR) ;                   \
	   echo  Created directory $(LIBDIR) ; \
	fi
	$(AR) $(MMFLIB) $(MMFOBJS)
	$(RANLIB) $(MMFLIB)

clean:
	$(RM) $(MMFLIB) $(MMFOBJS) *~
