#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")
target_sources(${PROJECT_NAME} PRIVATE
    "${CMAKE_CURRENT_LIST_DIR}/c_climate_hru.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_climateflow.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_ddsolrad.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_potet_jh.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_soltab.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_transp_tindex.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_climateflow.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_climate_hru.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_ddsolrad.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_potet_jh.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_soltab.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_transp_tindex.f90")
