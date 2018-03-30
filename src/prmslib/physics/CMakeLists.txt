#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")
target_sources(${PROJECT_NAME} PRIVATE
    "${CMAKE_CURRENT_LIST_DIR}/climate_hru.f90"
    "${CMAKE_CURRENT_LIST_DIR}/climateflow.f90"
    "${CMAKE_CURRENT_LIST_DIR}/ddsolrad.f90"
    "${CMAKE_CURRENT_LIST_DIR}/potet_jh.f90"
    "${CMAKE_CURRENT_LIST_DIR}/soltab.f90"
    "${CMAKE_CURRENT_LIST_DIR}/transp_tindex.f90")
