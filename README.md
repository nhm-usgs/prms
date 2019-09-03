# prms
Precipitation Runoff Modeling System
This fork makes two executables when doing 'make'. The executable /bin/prms will have capabilities of turning on the glacier_flag, the stream_temp_flag, the frozen_flag (that uses the old CFGI with no frozen depth just binary frozen state), and using the strmflow_module muskingum, muskingum_mann, or mizuroute. The  executable /bin/prmsrip will have capabilities of turning on the glacier_flag, the stream_temp_flag, the frozen_flag (that uses the new CFGImod with frozen depth), the ripst_flag, and using the strmflow_module muskingum, muskingum_mann, or mizuroute. 



Glacier params, calculate:

abl_elev_range-- from RGI == Randolph Glacier Inventory and modify, /Roland has code
glacier_frac_init-- from RGI /Roland has code
glrette_frac_init-- from RGI, fraction of HRU that is small glacier and not delineated into special glacier HRU /Roland has code
hru_length--geometric map tools/Roland has code
hru_width--geometric map tools/Roland has code
tohru--from glacier HRU delineation/Roland has code
(Note, some standard PRMS params change on glaciers, like hru_type =4 on all possible to be glacierized HRUs). Roland's code figured out all this



Glacier params, defaults, could calibrate some of these (I did in the Copper paper):

albedo_coef
albedo_ice
glacr_freeh2o_cap
glacr_layer
glacrva_coef-- will go away if update code with Delta h area change and then might add some
glacrva_exp-- will go away if update code with Delta h area change and then might add some
max_gldepth
stor_firn
stor_ice
stor_snow



Routing params calculate:

mann_n-- can be from slope if nothing better, that is the method in CONUS and then we calibrate
seg_slope-- DEM
Muskingum: seg_depth-- from regression models or? I got it from a paper in CONUS 
Dynamic: seg_width-- from regression models or NARwidth of ? I got it from a paper in CONUS for the streams smaller than 30 m wide, big is NARwidths
seg_length-- DEM? needs to be length including the vertical drop
(does not use K_coef)



Frozen ground params, defaults, can calibrate to permafrost probability maps:

cfgi_decay -- calibrate this maybe
cfgi_thrshld-- calibrate this definitely
porosity_hru-- soils data porosity
soil_depth-- Unified North American Soil Map? STATSGO?
soil_den --Unified North American Soil Map? STATSGO?



Riparian needs all the new routing params (besides seg_length) and:
calculate:

porosity_seg-- soils data porosity
ripst_areafr_max-- riparian area data gives fraction of HRU riparian
specyield_seg-- might be data on this somewhere, haven't got it for CONUS yet but default is perhaps close enough
transmiss_seg-- soils data transmissivity



Riparian defaults, most of these have no good reason to calibrate:

bankfinite_hru
bankst_head_init
ripst_et_coef
ripst_frac_init
tr_ratio
bank_height_fac -- calibrate this or at least think about what want, it's 20 right now



Stream Temp calculate: 

seg_elev-- DEM
seg_lat-- DEM
width_m-- relate to seg_width somehow?



Stream Temp, I'd leave these at defaults ... ask Markstrom:

albedo 
alte 
altw
azrh
gw_tau
lat_temp_adj
maxiter_sntemp
melt_temp
ss_tau
vce
vcw
vdemn
vdemx
vdwmn
vdwmx
vdwmx
vhe
vhw
voe
vow
width_alpha



Calving glacier params calculate:
NOTE: currently this isn't going to get put in so you can ignore (there are calving parameters needed for MWBMglacier now)

ocean_depth -- offshore DEM at end of calving front, current idea is to set up offshore HRUs that contain the glacier tongue area and have properties
(these glaciers will have tohrus that then will be possible to tell that the upstream glacier part feeds a calving front, as long as tongue calculated to float)
