#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")
target_sources(${PROJECT_NAME} PRIVATE
    #"${CMAKE_CURRENT_LIST_DIR}/old/base_datatypes.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_prms_time.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_basin.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_Basin_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_constants.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_conversions.f90"
    "${CMAKE_CURRENT_LIST_DIR}/m_utils_cbh.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/Datetime_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_nhru_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_obs.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_Simulation.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_prms_time.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_basin.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_basin_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_obs.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_nhru_summary.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_simulation.f90"
  "${CMAKE_CURRENT_LIST_DIR}/m_utils_prms.f90")

#target_sources(${libName}
#	PUBLIC
#"${CMAKE_CURRENT_LIST_DIR}/base_datatypes.f90"
#"${CMAKE_CURRENT_LIST_DIR}/basin.f90"
#"${CMAKE_CURRENT_LIST_DIR}/basin_summary.f90"
#"${CMAKE_CURRENT_LIST_DIR}/constants.f90"
#"${CMAKE_CURRENT_LIST_DIR}/conversions.f90"
#"${CMAKE_CURRENT_LIST_DIR}/m_prms6.f90"
#"${CMAKE_CURRENT_LIST_DIR}/nhru_summary.f90"
#"${CMAKE_CURRENT_LIST_DIR}/obs.f90"
#"${CMAKE_CURRENT_LIST_DIR}/prms_module.f90"
#"${CMAKE_CURRENT_LIST_DIR}/prms_time.f90"
#"${CMAKE_CURRENT_LIST_DIR}/time.f90"
#"${CMAKE_CURRENT_LIST_DIR}/utils_prms.f90"
#)
