# Get the name of the component
get_filename_component( COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" ABSOLUTE)
get_filename_component( COMPONENT_NAME "${COMPONENT_NAME}" NAME)

# Set the name of the test
set ( TEST_NAME "${PROJECT_NAME}-${COMPONENT_NAME}-Test" )

# Set the name of the test executable
set ( TEST_EXECUTABLE_NAME "${TEST_NAME}.x" )

# Set the list of pFUnit Fortran files, regular Fortran files and include files
set( pfunit_sources
  Test_Timer_SubTimer.F90
)
set( test_sources     "" )

# Creating the test
create_pFUnit_test( ${TEST_NAME} "${pfunit_sources}" "${test_sources}"
  EXECUTABLE_NAME   ${TEST_EXECUTABLE_NAME}
  LIST_LIBRARIES    "${STATIC_LIBRARY}"
)
