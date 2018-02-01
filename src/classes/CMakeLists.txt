file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")

#MESSAGE(STATUS ${sources})

target_sources(${libName} PUBLIC ${sources})
#target_sources(${libName}
#	PUBLIC
#        "${CMAKE_CURRENT_LIST_DIR}/Abc_class.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/Object_class.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/control_mod.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/dimensions_mod.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/m_kinds.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/m_str_arr.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/parameter_mod.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/rArray_class.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/rVariable_class.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/variables_mod.f90"
#)
