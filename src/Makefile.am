noinst_LIBRARIES = libprms.a

libprms_a_FFLAGS = -Bstatic-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,uverflow,underflow -finit-real=nan -std=f2008
libprms_a_FCLAGS = -Bstatic -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan -std=f2008
libprms_a_SOURCES = base_datatypes.f90 \
	constants.f90 \
	dimensions_mod.f90 \
	control_mod.f90 \
	conversions.f90 \
	time.f90 \
    utils_prms.f90 \
	prms_module.f90 \
    mmf_utils.f90 \
	file_io.f90 \
	basin.f90 \
	obs.f90 \
	read_control_file.f90 \
	read_data_file.f90 \
	prms_time.f90 \
	climateflow.f90 \
	climate_hru.f90 \
	nhru_summary.f90 \
	basin_summary.f90 \
	potet_jh.f90 \
	read_parameter_file.f90 \
	transp_tindex.f90 \
	soltab.f90 \
	ddsolrad.f90 \
	prms6.f90

bin_PROGRAMS = prms6
prms6_SOURCES = prms6.f90
prms6_LDFLAGS = -Bstatic  -Wall -fbacktrace -fimplicit-none 
prms6_LDADD = -L. -lprms -lm

CLEANFILES = *.mod *.o
