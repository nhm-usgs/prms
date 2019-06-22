# PRMS V makefile

include ../makelist
TARGET 	= $(BINDIR)/prmsgl
TARGET2 	= $(BINDIR)/prmsglrip

####################################################
# Rules for targets
####################################################
all: $(TARGET) $(TARGET2)

orig: $(TARGET)

rip: $(TARGET2)

#
# Define all object files which make up the library
#


OBJS = \
        call_modules.o \
        basin.o \
        climateflow.o \
        cascade.o \
        soltab.o \
        setup_param.o \
        convert_params.o \
        prms_time.o \
        obs.o \
        climate_hru.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pm.o \
        potet_pm_sta.o \
        potet_pan.o \
        potet_hamon.o \
        ddsolrad.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        precip_1sta_laps.o \
        precip_dist2.o \
        temp_1sta_laps.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
	glacr_melt.o \
        intcp.o \
        snowcomp.o \
        srunoff.o \
        soilzone.o \
        gwflow.o \
        water_use_read.o \
        dynamic_param_read.o \
        water_balance.o \
        routing.o \
        strmflow.o \
        strmflow_in_out.o \
        muskingum.o \
        muskingum_lake.o \
        mizuroute.o \
        subbasin.o \
        map_results.o \
        nhru_summary.o \
        nsub_summary.o \
        nsegment_summary.o \
        basin_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sum.o \
        stream_temp.o \
        utils_prms.o

RIP = \
        call_modulesRip.o \
        basin.o \
        climateflow.o \
        cascade.o \
        soltab.o \
        setup_param.o \
        convert_params.o \
        prms_time.o \
        obs.o \
        climate_hru.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pm.o \
        potet_pm_sta.o \
        potet_pan.o \
        potet_hamon.o \
        ddsolrad.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        precip_1sta_laps.o \
        precip_dist2.o \
        temp_1sta_laps.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
	glacr_melt.o \
        intcp.o \
        snowcomp.o \
        srunoff.o \
        soilzone.o \
        gwflow.o \
        water_use_read.o \
        dynamic_param_read.o \
        water_balance.o \
        routingRip.o \
        strmflow.o \
        strmflow_in_outCopy.o \
        muskingumRip.o \
        muskingum_lakeCopy.o \
        mizurouteRip.o \
        subbasin.o \
        map_results.o \
        nhru_summary.o \
        nsub_summary.o \
        nsegment_summary.o \
        basin_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sumCopy.o \
        utils_prms.o \
        stream_tempCopy.o

$(TARGET): $(OBJS)
	$(RM) $(TARGET)
	$(FC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(MMFLIB) $(MIZULIB) $(INCMIZU) $(FLIBS)

$(TARGET2): $(RIP)
	$(RM) $(TARGET2)
	$(FC) $(LDFLAGS) -o $(TARGET2) $(RIP) $(MMFLIB) $(MIZULIB) $(INCMIZU) $(FLIBS)

#
# Define all object files which make up the library
#

clean:
	$(RM) $(OBJS) $(RIP) *.mod *~

call_modules.o: call_modules.f90
	$(FC) -c $(FFLAGS) call_modules.f90

setup_param.o: setup_param.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) setup_param.f90

snowcomp.o: snowcomp.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_set_time.mod prms_intcp.mod
	$(FC) -c $(FFLAGS) snowcomp.f90

basin_sum.o: basin_sum.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_routing.mod
	$(FC) -c $(FFLAGS) basin_sum.f90

ddsolrad.o: ddsolrad.f90 prms_module.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ddsolrad.f90

ccsolrad.o: ccsolrad.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ccsolrad.f90

prms_summary.o: prms_summary.f90 prms_module.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_soilzone.mod prms_gwflow.mod
	$(FC) -c $(FFLAGS) prms_summary.f90

intcp.o: intcp.f90 prms_obs.mod prms_climatevars.mod prms_flowvars.mod prms_module.mod prms_basin.mod prms_water_use.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) intcp.f90

nhru_summary.o: nhru_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) nhru_summary.f90

nsub_summary.o: nsub_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) nsub_summary.f90

nsegment_summary.o: nsegment_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) nsegment_summary.f90

basin_summary.o: basin_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) basin_summary.f90

precip_1sta_laps.o: precip_1sta_laps.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) precip_1sta_laps.f90

climate_hru.o: climate_hru.f90 prms_module.mod prms_basin.mod prms_soltab.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) climate_hru.f90

cascade.o: cascade.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) cascade.f90

basin.o: basin.f90 prms_module.mod
	$(FC) -c $(FFLAGS) basin.f90

srunoff.o: srunoff.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_intcp.mod prms_snow.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) srunoff.f90

climateflow.o: climateflow.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) climateflow.f90

soilzone.o: soilzone.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snow.mod prms_cascade.mod prms_climatevars.mod prms_set_time.mod prms_srunoff.mod
	$(FC) -c $(FFLAGS) soilzone.f90

subbasin.o: subbasin.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_intcp.mod prms_srunoff.mod prms_soilzone.mod prms_muskingum_lake.mod prms_snow.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) subbasin.f90

muskingum_lake.o: muskingum_lake.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_srunoff.mod prms_gwflow.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) muskingum_lake.f90

strmflow_in_out.o: strmflow_in_out.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_routing.mod prms_obs.mod prms_srunoff.mod prms_gwflow.mod 
	$(FC) -c $(FFLAGS) strmflow_in_out.f90

water_balance.o: water_balance.f90 prms_module.mod prms_basin.mod prms_srunoff.mod prms_flowvars.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_cascade.mod prms_intcp.mod prms_snow.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) water_balance.f90

dynamic_param_read.o: dynamic_param_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_flowvars.mod prms_potet_jh.mod prms_potet_pm.mod prms_potet_hs.mod prms_potet_pt.mod prms_potet_hamon.mod transp_tindex.o transp_frost.o prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_soilzone.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) dynamic_param_read.f90

water_use_read.o: water_use_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_flowvars.mod
	$(FC) -c $(FFLAGS) water_use_read.f90

utils_prms.o: utils_prms.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) utils_prms.f90

gwflow.o: gwflow.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_srunoff.mod prms_soilzone.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) gwflow.f90

map_results.o: map_results.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) map_results.f90

frost_date.o: frost_date.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) frost_date.f90

transp_tindex.o: transp_tindex.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) transp_frost.f90

temp_1sta_laps.o: temp_1sta_laps.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) temp_1sta_laps.f90

temp_dist2.o: temp_dist2.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) temp_dist2.f90

precip_dist2.o: precip_dist2.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) precip_dist2.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_srunoff.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) strmflow.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) potet_jh.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_climate_hru.mod prms_soltab.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) potet_hs.f90

potet_pm.o: potet_pm.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_climate_hru.mod prms_soltab.mod
	$(FC) -c $(FFLAGS) potet_pm.f90

potet_pm_sta.o: potet_pm_sta.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_obs.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) potet_pm_sta.f90

potet_pan.o: potet_pan.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90 prms_module.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) write_climate_hru.f90

prms_time.o: prms_time.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) prms_time.f90

soltab.o: soltab.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) soltab.f90

convert_params.o: convert_params.f90 prms_module.mod
	$(FC) -c $(FFLAGS) convert_params.f90

obs.o: obs.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) obs.f90

ide_dist.o: ide_dist.f prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ide_dist.f

xyz_dist.o: xyz_dist.f prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) xyz_dist.f

stream_temp.o: stream_temp.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_soltab.mod prms_snow.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) stream_temp.f90

glacr_melt.o: glacr_melt.f90 prms_snow.mod prms_intcp.mod prms_soltab.mod
	$(FC) -c $(FFLAGS) glacr_melt.f90

call_modulesRip.o: call_modulesRip.f90
	$(FC) -c $(FFLAGS) call_modulesRip.f90

muskingum.o: muskingum.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_srunoff.mod prms_gwflow.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) muskingum.f90

routing.o: routing.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_water_use.mod prms_srunoff.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) routing.f90

mizuroute.o: mizuroute.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_srunoff.mod prms_gwflow.mod prms_glacr.mod 
	$(FC) -c $(FFLAGS) $(INCMIZU) mizuroute.f90

muskingumRip.o: muskingumRip.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoff.mod prms_gwflow.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) muskingumRip.f90

routingRip.o: routingRip.f90 prms_moduleRip.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_water_use.mod prms_srunoff.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) routingRip.f90

mizurouteRip.o: mizurouteRip.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoff.mod prms_gwflow.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) $(INCMIZU) mizurouteRip.f90

basin_sumCopy.o: basin_sumCopy.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod
	$(FC) -c $(FFLAGS) basin_sumCopy.f90

muskingum_lakeCopy.o: muskingum_lakeCopy.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoff.mod prms_gwflow.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) muskingum_lakeCopy.f90

strmflow_in_outCopy.o: strmflow_in_outCopy.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_routingRip.mod prms_obs.mod prms_srunoff.mod prms_gwflow.mod 
	$(FC) -c $(FFLAGS) strmflow_in_outCopy.f90

stream_tempCopy.o: stream_tempCopy.f90 prms_module.mod prms_basin.mod prms_routingRip.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_soltab.mod prms_climatevars.mod prms_snow.mod
	$(FC) -c $(FFLAGS) stream_tempCopy.f90

prms_routingRip.mod: routingRip.o
prms_moduleRip.mod: call_modulesRip.o
prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_muskingum.mod: muskingum.o
prms_intcp.mod: intcp.o
prms_snow.mod: snowcomp.o
prms_cascade.mod: cascade.o
prms_srunoff.mod: srunoff.o
prms_soilzone.mod: soilzone.o
prms_snowcomp.mod: snowcomp.o
prms_routing.mod: routing.o
prms_water_use.mod: water_use_read.o
prms_obs.mod: obs.o
prms_set_time.mod: prms_time.o
prms_glacr.mod: glacr_melt.o
