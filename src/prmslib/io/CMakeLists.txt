# file(GLOB sources "${CMAKE_CURRENT_LIST_DIR}/*.f90")
target_sources(${PROJECT_NAME} PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/file_io.f90")
  # "${CMAKE_CURRENT_LIST_DIR}/read_control_file.f90")
    # "${CMAKE_CURRENT_LIST_DIR}/read_data_file.f90"
    # "${CMAKE_CURRENT_LIST_DIR}/read_parameter_file.f90")

# target_sources(${libName} PUBLIC "${CMAKE_CURRENT_LIST_DIR}/file_io.f90" #"${CMAKE_CURRENT_LIST_DIR}/read_control_file.f90" "${CMAKE_CURRENT_LIST_DIR}/read_data_file.f90" "${CMAKE_CURRENT_LIST_DIR}/read_parameter_file.f90")
