# PRMS V makefile

include ../makelist
TARGET2 	= $(BINDIR)/prmsrip

####################################################
# Rules for targets
####################################################
all: $(TARGET2)

#
# Define all object files which make up the library
#


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
        snowcompCfgim.o \
        srunoffCfgim.o \
        soilzoneCfgim.o \
        gwflow.o \
        water_use_read.o \
        dynamic_param_read.o \
        water_balance.o \
        routingRip.o \
        strmflow.o \
        strmflow_in_out.o \
        muskingumRip.o \
        muskingum_lake.o \
        mizurouteRip.o \
        subbasin.o \
        map_results.o \
        nhru_summary.o \
        nsub_summary.o \
        nsegment_summary.o \
        basin_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sum.o \
        utils_prms.o \
        stream_temp.o


$(TARGET2):$(RIP)
	$(RM) $(TARGET2)
	$(FC) $(LDFLAGS) -o $(TARGET2) $(RIP) $(MMFLIB) $(MIZULIB) $(INCMIZU) $(FLIBS)

#
# Define all object files which make up the library
#

clean:
	$(RM) $(RIP) *.mod *~

setup_param.o: $(PRMSDIR)/setup_param.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/setup_param.f90

ddsolrad.o: $(PRMSDIR)/ddsolrad.f90 prms_module.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/ddsolrad.f90

ccsolrad.o: $(PRMSDIR)/ccsolrad.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/ccsolrad.f90

intcp.o: $(PRMSDIR)/intcp.f90 prms_obs.mod prms_climatevars.mod prms_flowvars.mod prms_module.mod prms_basin.mod prms_water_use.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/intcp.f90

nhru_summary.o: $(PRMSDIR)/nhru_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/nhru_summary.f90

nsub_summary.o: $(PRMSDIR)/nsub_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/nsub_summary.f90

nsegment_summary.o: $(PRMSDIR)/nsegment_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/nsegment_summary.f90

basin_summary.o: $(PRMSDIR)/basin_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/basin_summary.f90

precip_1sta_laps.o: $(PRMSDIR)/precip_1sta_laps.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/precip_1sta_laps.f90

climate_hru.o: $(PRMSDIR)/climate_hru.f90 prms_module.mod prms_basin.mod prms_soltab.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/climate_hru.f90

cascade.o: $(PRMSDIR)/cascade.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/cascade.f90

basin.o: $(PRMSDIR)/basin.f90 prms_module.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/basin.f90

climateflow.o: $(PRMSDIR)/climateflow.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/climateflow.f90

water_use_read.o: $(PRMSDIR)/water_use_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_flowvars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/water_use_read.f90

utils_prms.o: $(PRMSDIR)/utils_prms.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/utils_prms.f90

map_results.o: $(PRMSDIR)/map_results.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/map_results.f90

frost_date.o: $(PRMSDIR)/frost_date.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/frost_date.f90

transp_tindex.o: $(PRMSDIR)/transp_tindex.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/transp_tindex.f90

transp_frost.o: $(PRMSDIR)/transp_frost.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/transp_frost.f90

temp_1sta_laps.o: $(PRMSDIR)/temp_1sta_laps.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/temp_1sta_laps.f90

temp_dist2.o: $(PRMSDIR)/temp_dist2.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/temp_dist2.f90

precip_dist2.o: $(PRMSDIR)/precip_dist2.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/precip_dist2.f90

potet_jh.o: $(PRMSDIR)/potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_jh.f90

potet_pt.o: $(PRMSDIR)/potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_climate_hru.mod prms_soltab.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_pt.f90

potet_hs.o: $(PRMSDIR)/potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_hs.f90

potet_pm.o: $(PRMSDIR)/potet_pm.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_climate_hru.mod prms_soltab.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_pm.f90

potet_pm_sta.o: $(PRMSDIR)/potet_pm_sta.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_obs.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_pm_sta.f90

potet_pan.o: $(PRMSDIR)/potet_pan.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_pan.f90

potet_hamon.o: $(PRMSDIR)/potet_hamon.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/potet_hamon.f90

write_climate_hru.o: $(PRMSDIR)/write_climate_hru.f90 prms_module.mod prms_set_time.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/write_climate_hru.f90

prms_time.o: $(PRMSDIR)/prms_time.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/prms_time.f90

soltab.o: $(PRMSDIR)/soltab.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/soltab.f90

convert_params.o: $(PRMSDIR)/convert_params.f90 prms_module.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/convert_params.f90

obs.o: $(PRMSDIR)/obs.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/obs.f90

ide_dist.o: $(PRMSDIR)/ide_dist.f prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/ide_dist.f

xyz_dist.o: $(PRMSDIR)/xyz_dist.f prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/xyz_dist.f

call_modulesRip.o: call_modulesRip.f90
	$(FC) -c $(FFLAGS) call_modulesRip.f90

muskingumRip.o: muskingumRip.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoffCfgim.mod prms_gwflow.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) muskingumRip.f90

routingRip.o: routingRip.f90 prms_moduleRip.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_water_use.mod prms_srunoffCfgim.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) routingRip.f90

mizurouteRip.o: mizurouteRip.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoffCfgim.mod prms_gwflow.mod prms_glacr.mod
	$(FC) -c $(FFLAGS) $(INCMIZU) mizurouteRip.f90

basin_sum.o: $(PRMSDIR)/basin_sum.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snowCfgim.mod prms_srunoffCfgim.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/basin_sum.f90

muskingum_lake.o: $(PRMSDIR)/muskingum_lake.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routingRip.mod prms_srunoffCfgim.mod prms_gwflow.mod prms_soilzoneCfgim.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/muskingum_lake.f90

strmflow_in_out.o: $(PRMSDIR)/strmflow_in_out.f90 prms_moduleRip.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_routingRip.mod prms_obs.mod prms_srunoffCfgim.mod prms_gwflow.mod 
	$(FC) -c $(FFLAGS) $(PRMSDIR)/strmflow_in_out.f90

stream_temp.o: $(PRMSDIR)/stream_temp.f90 prms_module.mod prms_basin.mod prms_routingRip.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_soltab.mod prms_climatevars.mod prms_snowCfgim.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/stream_temp.f90

srunoffCfgim.o: srunoffCfgim.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_intcp.mod prms_snow.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) srunoffCfgim.f90

soilzoneCfgim.o: soilzoneCfgim.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snowCfgim.mod prms_cascade.mod prms_climatevars.mod prms_set_time.mod prms_srunoffCfgim.mod
	$(FC) -c $(FFLAGS) soilzoneCfgim.f90

snowcompCfgim.o: snowcompCfgim.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_set_time.mod prms_intcp.mod
	$(FC) -c $(FFLAGS) snowcompCfgim.f90

prms_summary.o: $(PRMSDIR)/prms_summary.f90 prms_module.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_intcp.mod prms_snowCfgim.mod prms_srunoffCfgim.mod prms_soilzoneCfgim.mod prms_gwflow.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/prms_summary.f90

subbasin.o: $(PRMSDIR)/subbasin.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod prms_set_time.mod prms_intcp.mod prms_srunoffCfgim.mod prms_soilzoneCfgim.mod prms_muskingum_lake.mod prms_snowCfgim.mod prms_climatevars.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/subbasin.f90

water_balance.o: $(PRMSDIR)/water_balance.f90 prms_module.mod prms_basin.mod prms_srunoffCfgim.mod prms_flowvars.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_cascade.mod prms_intcp.mod prms_snowCfgim.mod prms_soilzoneCfgim.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/water_balance.f90

dynamic_param_read.o: $(PRMSDIR)/dynamic_param_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_flowvars.mod prms_potet_jh.mod prms_potet_pm.mod prms_potet_hs.mod prms_potet_pt.mod prms_potet_hamon.mod transp_tindex.o transp_frost.o prms_intcp.mod prms_snowCfgim.mod prms_srunoffCfgim.mod prms_soilzoneCfgim.mod prms_climate_hru.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/dynamic_param_read.f90

gwflow.o: $(PRMSDIR)/gwflow.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_srunoffCfgim.mod prms_soilzoneCfgim.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/gwflow.f90

strmflow.o: $(PRMSDIR)/strmflow.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_srunoffCfgim.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/strmflow.f90

glacr_melt.o: $(PRMSDIR)/glacr_melt.f90 prms_snowCfgim.mod prms_intcp.mod prms_soltab.mod
	$(FC) -c $(FFLAGS) $(PRMSDIR)/glacr_melt.f90

prms_routingRip.mod: routingRip.o
prms_moduleRip.mod: call_modulesRip.o
prms_srunoffCfgim.mod: srunoffCfgim.o
prms_soilzoneCfgim.mod: soilzoneCfgim.o
prms_snowCfgim.mod: snowcompCfgim.o
prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_gwflow.mod: gwflow.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_intcp.mod: intcp.o
prms_cascade.mod: cascade.o
prms_water_use.mod: water_use_read.o
prms_obs.mod: obs.o
prms_set_time.mod: prms_time.o
prms_glacr.mod: glacr_melt.o
