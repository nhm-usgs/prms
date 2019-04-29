# prms libraray makefile
# $Id: Makefile 7050 2014-12-02 19:06:41Z rsregan $

include ../makelist
TARGET 	= ./prms
PRMSLIBMD	= ./libprms.a
MMFLIB	= ../mmf/libmmf.a
LIBS	= $(PRMSLIBMD) $(MMFLIB)

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): call_modules.o intcp_2d.o snowcomp_2d.o srunoff.o climate_hru_2d.o soilzone.o routing.o $(LIBS)
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) call_modules.o climate_hru_2d.o intcp_2d.o snowcomp_2d.o srunoff.o soilzone.o routing.o $(LIBS) $(FLIBS)

$(MMFLIB):
	$(CD) ../mmf/;make

#
# Define all object files which make up the library
#

LIBOBJS = \
        prms_time.o \
        basin_2d.o \
        climateflow_2d.o \
        cascade.o \
        soltab.o \
        obs.o \
        check_nhru_params.o \
        potet_jh_2d.o \
        potet_pt.o \
        potet_hs.o \
        potet_pm.o \
        potet_pan_2d.o \
        potet_hamon_2d.o \
        check_nhru_params.o \
        cloud_cover.o \
        ddsolrad_2d.o \
        ccsolrad_2d.o \
        ide_dist_2d.o \
        xyz_dist_2d.o \
        precip_1sta_laps_2d.o \
        precip_dist2_2d.o \
        temp_1sta_laps_2d.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date_2d.o \
        gwflow.o \
        water_balance.o \
        lake_route.o \
        strmflow.o \
        strmflow_in_out.o \
        muskingum.o \
        strmflow_lake.o \
        subbasin.o \
        map_results.o \
        nhru_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sum.o \
        utils_prms.o

all: lib

install: lib

lib: $(LIBS)

$(PRMSLIBMD): call_modules.o $(LIBOBJS)
	$(RM) $(PRMSLIBMD)
	$(AR) $(PRMSLIBMD) $(LIBOBJS)
	$(RANLIB) $(PRMSLIBMD)

clean:
	$(RM) $(PRMSLIBMD) $(TARGET)
	$(RM) *.o *.mod *~

call_modules.o: call_modules.f90
	gfortran -c $(FFLAGS) call_modules.f90

basin_sum.o: basin_sum.f90
	gfortran -c $(FFLAGS) basin_sum.f90

subbasin.o: subbasin.f90
	gfortran -c $(FFLAGS) subbasin.f90

ddsolrad_2d.o: ddsolrad_2d.f90 call_modules.o check_nhru_params.o
	gfortran -c $(FFLAGS) ddsolrad_2d.f90

ccsolrad_2d.o: ccsolrad_2d.f90
	gfortran -c $(FFLAGS) ccsolrad_2d.f90

snowcomp_2d.o: snowcomp_2d.f90
	gfortran -c $(FFLAGS) snowcomp_2d.f90

gwflow.o: gwflow.f90 soilzone.o
	gfortran -c $(FFLAGS) gwflow.f90

utils_prms.o: utils_prms.f90
	gfortran -c $(FFLAGS) utils_prms.f90

prms_summary.o: prms_summary.f90
	gfortran -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90
	gfortran -c $(FFLAGS) muskingum.f90

intcp_2d.o: intcp_2d.f90 prms_obs.mod prms_climatevars.mod prms_flowvars.mod
	gfortran -c $(FFLAGS) intcp_2d.f90

map_results.o: map_results.f90
	gfortran -c $(FFLAGS) map_results.f90

nhru_summary.o: nhru_summary.f90
	gfortran -c $(FFLAGS) nhru_summary.f90

soltab.o: soltab.f90
	gfortran -c $(FFLAGS) soltab.f90

frost_date_2d.o: frost_date_2d.f90
	gfortran -c $(FFLAGS) frost_date_2d.f90

precip_1sta_laps_2d.o: precip_1sta_laps_2d.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_1sta_laps_2d.f90

transp_tindex.o: transp_tindex.f90
	gfortran -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90
	gfortran -c $(FFLAGS) transp_frost.f90

temp_1sta_laps_2d.o: temp_1sta_laps_2d.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_1sta_laps_2d.f90

temp_dist2.o: temp_dist2.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_dist2.f90

precip_dist2_2d.o: precip_dist2_2d.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_dist2_2d.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow.f90

strmflow_in_out.o: strmflow_in_out.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow_in_out.f90

potet_jh_2d.o: potet_jh_2d.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh_2d.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hs.f90

potet_pm.o: potet_pm.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pm.f90

potet_pan_2d.o: potet_pan_2d.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pan_2d.f90

potet_hamon_2d.o: potet_hamon_2d.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hamon_2d.f90

write_climate_hru.o: write_climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) write_climate_hru.f90

climate_hru_2d.o: climate_hru_2d.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod
	gfortran -c $(FFLAGS) climate_hru_2d.f90

cascade.o: cascade.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) cascade.f90

basin_2d.o: basin_2d.f90 prms_module.mod
	gfortran -c $(FFLAGS) basin_2d.f90

obs.o: obs.f90 prms_module.mod prms_set_time.mod
	gfortran -c $(FFLAGS) obs.f90

srunoff.o: srunoff.f90 prms_intcp.mod prms_snow.mod prms_cascade.mod
	gfortran -c $(FFLAGS) srunoff.f90

climateflow_2d.o: climateflow_2d.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	gfortran -c $(FFLAGS) climateflow_2d.f90

soilzone.o: soilzone.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) soilzone.f90

strmflow_lake.o: strmflow_lake.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) strmflow_lake.f90

lake_route.o: lake_route.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) lake_route.f90

routing.o: routing.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod
	gfortran -c $(FFLAGS) routing.f90

prms_time.o: prms_time.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) prms_time.f90

cloud_cover.o: cloud_cover.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) cloud_cover.f90

check_nhru_params.o: check_nhru_params.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) check_nhru_params.f90

water_balance.o: water_balance.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) water_balance.f90

prms_climatevars.mod: climateflow_2d.o
prms_flowvars.mod: climateflow_2d.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_obs.mod: obs.o
prms_basin.mod: basin_2d.o
prms_soltab.mod: soltab.o
prms_intcp.mod: intcp_2d.o
prms_snow.mod: snowcomp_2d.o
prms_cascade.mod: cascade.o
prms_set_time.mod: prms_time.o
