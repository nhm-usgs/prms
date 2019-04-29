# prms libraray makefile
# $Id: Makefile 5629 2013-04-24 21:50:20Z rsregan $

include ../makelist
MMFLIB	= ../mmf/libmmf.a
TARGET 	= ./prms
PRMSBASE	= ./libprms.a
LIBS	= $(PRMSBASE) $(MMFLIB)

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): call_modules.o basin.o obs.o climateflow.o intcp.o snowcomp.o srunoff.o gwflow.o $(LIBS)
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) call_modules.o basin.o obs.o climateflow.o intcp.o snowcomp.o srunoff.o gwflow.o $(LIBS) $(FLIBS)

$(MMFLIB):
	$(CD) ../mmf/;make

#
# Define all object files which make up the library
#

LIBOBJS = \
        cascade.o \
        soltab.o \
        soltab_prms.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pan.o \
        potet_hamon.o \
        potet_hamon_prms.o \
        ddsolrad.o \
        ddsolrad_prms.o \
        ccsolrad.o \
        ccsolrad_prms.o \
        ide_dist.o \
        xyz_dist.o \
        climate_hru_combined.o \
        precip_1sta_laps.o \
        precip_dist2.o \
        temp_1sta_laps.o \
        temp_2sta_prms.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
        soilzone.o \
        smbal_prms.o \
        ssflow_prms.o \
        strmflow.o \
        strmflow_in_out.o \
        strmflow_lake.o \
        subbasin.o \
        map_results.o \
        muskingum.o \
        write_climate_hru.o \
        prms_summary.o \
        hru_sum_prms.o \
        basin_sum.o \
        utils_prms.o

all: lib

install: lib

lib: $(LIBS)

$(PRMSBASE): call_modules.o $(LIBOBJS)
	$(RM) $(PRMSBASE)
	$(AR) $(PRMSBASE) $(LIBOBJS)
	$(RANLIB) $(PRMSBASE)

clean:
	$(RM) $(PRMSBASE)
	$(RM) *.o *.mod *~

call_modules.o: call_modules.f90
	gfortran -c $(FFLAGS) call_modules.f90

basin_sum.o: basin_sum.f90
	gfortran -c $(FFLAGS) basin_sum.f90

subbasin.o: subbasin.f90
	gfortran -c $(FFLAGS) subbasin.f90

ddsolrad.o: ddsolrad.f90
	gfortran -c $(FFLAGS) ddsolrad.f90

ccsolrad.o: ccsolrad.f90
	gfortran -c $(FFLAGS) ccsolrad.f90

snowcomp.o: snowcomp.f90
	gfortran -c $(FFLAGS) snowcomp.f90

gwflow.o: gwflow.f90
	gfortran -c $(FFLAGS) gwflow.f90

utils_prms.o: utils_prms.f90
	gfortran -c $(FFLAGS) utils_prms.f90

prms_summary.o: prms_summary.f90
	gfortran -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90
	gfortran -c $(FFLAGS) muskingum.f90

intcp.o: intcp.f90 prms_obs.mod
	gfortran -c $(FFLAGS) intcp.f90

map_results.o: map_results.f90
	gfortran -c $(FFLAGS) map_results.f90

soltab.o: soltab.f90
	gfortran -c $(FFLAGS) soltab.f90

frost_date.o: frost_date.f90
	gfortran -c $(FFLAGS) frost_date.f90

precip_1sta_laps.o: precip_1sta_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_1sta_laps.f90

transp_tindex.o: transp_tindex.f90
	gfortran -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90
	gfortran -c $(FFLAGS) transp_frost.f90

temp_1sta_laps.o: temp_1sta_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_1sta_laps.f90

temp_dist2.o: temp_dist2.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_dist2.f90

precip_dist2.o: precip_dist2.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_dist2.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow.f90

strmflow_in_out.o: strmflow_in_out.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow_in_out.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hs.f90

potet_pan.o: potet_pan.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) write_climate_hru.f90

climate_hru_combined.o: climate_hru_combined.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) climate_hru_combined.f90

cascade.o: cascade.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) cascade.f90

basin.o: basin.f90 prms_module.mod
	gfortran -c $(FFLAGS) basin.f90

obs.o: obs.f90 prms_module.mod
	gfortran -c $(FFLAGS) obs.f90

srunoff.o: srunoff.f90 prms_intcp.mod prms_snow.mod prms_cascade.mod
	gfortran -c $(FFLAGS) srunoff.f90

climateflow.o: climateflow.f90  prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) climateflow.f90

soilzone.o: soilzone.f90  prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) soilzone.f90

hru_sum_prms.o: hru_sum_prms.f90  prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) hru_sum_prms.f90

strmflow_lake.o: strmflow_lake.f90  prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) strmflow_lake.f90

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_obs.mod: obs.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_intcp.mod: intcp.o
prms_snow.mod: snowcomp.o
prms_cascade.mod: cascade.o
