#file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")

#MESSAGE(STATUS ${sources})

#target_sources(${PROJECT_NAME} PUBLIC ${sources})
target_sources(${PROJECT_NAME} PRIVATE
    "${CMAKE_CURRENT_LIST_DIR}/Abc_class.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/Array_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/Control_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/Object_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/Parameter_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/String_class.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/control_mod.f90"
    "${CMAKE_CURRENT_LIST_DIR}/dArray_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/dVariable_class.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/dimensions_mod.f90"
    "${CMAKE_CURRENT_LIST_DIR}/iArray_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/iVariable_class.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/m_kinds.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/parameter_mod.f90"
    "${CMAKE_CURRENT_LIST_DIR}/rArray_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/rVariable_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sArray_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sVariable_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_control.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_parameters.f90"
#    "${CMAKE_CURRENT_LIST_DIR}/variables_mod.f90"
)
