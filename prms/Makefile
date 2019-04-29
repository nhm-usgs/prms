# prms libraray makefile
# $Id: Makefile 4894 2012-10-05 17:28:28Z rsregan $

include ../makelist
LIBDIR	= ./
PRMSLIB = ./libprmsiv.a
MMFLIB	 = ../mmf/libmmf.a
TARGET 	= ./prms
LIBS	= $(PRMSLIB) $(MMFLIB) $(FLIBS)

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MMFLIB) $(PRMSLIB) call_modules.o
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) call_modules.o climateflow.o $(LIBOBJS4) $(LIBOBJS2) $(LIBS)

$(MMFLIB):
	$(CD) ../mmf;make

#
# Define all object files which make up the library
#

LIBSNOW = \
        potet_jh_hru_mo.o \
        snowcomp.o

LIBOBJS = \
	basin.o

LIBOBJS8 = \
        cascade.o \
        soltab.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pan.o \
        ddsolrad.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        precip_1sta.o \
        precip_laps.o \
        precip_dist2.o \
        climate_hru.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
        potet_hamon.o \
        utils_prms.o

LIBOBJS7 = \
        climateflow.o

LIBOBJS4 = \
        obs.o

LIBOBJS2 = \
        srunoff_carea.o \
        srunoff_smidx.o \
        soilzone.o \
        gwflow.o \
        muskingum.o \
        musroute.o

LIBOBJS5 = \
        strmflow.o \
        strmflow_lake.o \
        subbasin.o \
        map_results.o \
        write_climate_hru.o \
        basin_sum.o \
        prms_summary.o

LIBOBJS1 = \
        temp_1sta.o \
        temp_laps.o \
        ide_dist.o \
        xyz_dist.o \
        temp_dist2.o \
        intcp.o

LIBOBJS3 = \
        soltab_prms.o \
        temp_2sta_prms.o \
        ddsolrad_prms.o \
        ccsolrad_prms.o \
        potet_hamon_prms.o \
        smbal_prms.o \
        hru_sum_prms.o \
        ssflow_prms.o

all: lib

install: lib

lib: $(PRMSLIB)

$(PRMSLIB): call_modules.o $(LIBOBJS) $(LIBOBJS7) $(LIBOBJS4) $(LIBOBJS1) $(LIBOBJS8) $(LIBSNOW) $(LIBOBJS2) $(LIBOBJS3) $(LIBOBJS5)
	$(RM) $(PRMSLIB)
#	$(AR) $(PRMSLIB) $(LIBOBJS) $(LIBOBJS1) $(LIBSNOW) $(LIBOBJS3)
	$(AR) $(PRMSLIB) call_modules.o $(LIBOBJS) $(LIBOBJS7) $(LIBOBJS4) $(LIBOBJS1) $(LIBOBJS8) $(LIBSNOW) $(LIBOBJS2) $(LIBOBJS3) $(LIBOBJS5)
	$(RANLIB) $(PRMSLIB)

clean:
	$(RM) $(PRMSLIB)
	$(RM) *.o *.mod *~

gwflow.o: gwflow.f90
	gfortran -c $(FFLAGS) gwflow.f90

utils_prms.o: utils_prms.f90
	gfortran -c $(FFLAGS) utils_prms.f90

hru_sum_prms.o: hru_sum_prms.f90
	gfortran -c $(FFLAGS) hru_sum_prms.f90

prms_summary.o: prms_summary.f90
	gfortran -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90
	gfortran -c $(FFLAGS) muskingum.f90

intcp.o: intcp.f90
	gfortran -c $(FFLAGS) intcp.f90

musroute.o: musroute.f90
	gfortran -c $(FFLAGS) musroute.f90

map_results.o: map_results.f90
	gfortran -c $(FFLAGS) map_results.f90

soltab.o: soltab.f90
	gfortran -c $(FFLAGS) soltab.f90

frost_date.o: frost_date.f90
	gfortran -c $(FFLAGS) frost_date.f90

precip_1sta.o: precip_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_1sta.f90

transp_tindex.o: transp_tindex.f90
	gfortran -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90
	gfortran -c $(FFLAGS) transp_frost.f90

temp_1sta.o: temp_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_1sta.f90

temp_laps.o: temp_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_laps.f90

precip_laps.o: precip_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_laps.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hs.f90

potet_jh_hru_mo.o: potet_jh_hru_mo.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh_hru_mo.f90

potet_pan.o: potet_pan.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) write_climate_hru.f90

climate_hru.o: climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) climate_hru.f90

climateflow.o: climateflow.f  prms_module.mod prms_basin.mod cascade.o
	gfortran -c $(FFLAGS) climateflow.f

cascade.o: cascade.f prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) cascade.f

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_obs.mod: obs.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
