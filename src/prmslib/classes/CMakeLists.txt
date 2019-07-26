#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")

#MESSAGE(STATUS ${sources})

#target_sources(${PROJECT_NAME} PUBLIC ${sources})
target_sources(${PROJECT_NAME} PRIVATE
    "${CMAKE_CURRENT_LIST_DIR}/c_Abc.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/Array_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_Control.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_Object.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/c_Parameter.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_String.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_dArray.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_dVariable.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_iArray.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_iScalar.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_iVariable.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_ModelBase.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_rArray.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_rScalar.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_rVariable.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_sArray.f90"
    "${CMAKE_CURRENT_LIST_DIR}/c_sVariable.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_control.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/sm_parameters.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_ModelBase.f90"
)
