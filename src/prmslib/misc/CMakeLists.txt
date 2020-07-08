#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")
target_sources(${PROJECT_NAME} PRIVATE
    "${CMAKE_CURRENT_LIST_DIR}/c_outvar_list.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_outvar_node.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_prms_time.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_basin.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_utils_prms.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_constants.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_conversions.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_utils_cbh.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_utils_time.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/Datetime_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_FileIO.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_FileIO_netcdf.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_obs.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/c_restart.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_simulation.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_water_balance.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_outvar_list.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_outvar_node.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_prms_time.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_basin.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_FileIO_netcdf.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_obs.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/sm_restart.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_simulation.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_water_balance.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_utils_prms.f90")
    # "${CMAKE_CURRENT_LIST_DIR}/forpy_mod.F90")

# The simulation code must always be compiled without any optimization.
# Any optimization will result in a segault early in execution.
# The segfault occurs for compiles with gcc-7.2.0 or gcc-8.2.0, but not ifort 2018u3
SET_SOURCE_FILES_PROPERTIES("${CMAKE_CURRENT_LIST_DIR}/c_simulation.f90" PROPERTIES COMPILE_FLAGS -O0)
SET_SOURCE_FILES_PROPERTIES("${CMAKE_CURRENT_LIST_DIR}/sm_simulation.f90" PROPERTIES COMPILE_FLAGS -O0)

# 2019-08-15 PAN: Executable compiled by gfortran will segfault (SIGSEGV) on Macos (maybe others)
# if optimization is higher than -O2.
SET_SOURCE_FILES_PROPERTIES("${CMAKE_CURRENT_LIST_DIR}/sm_FileIO_netcdf.f90" PROPERTIES COMPILE_FLAGS -O2)
