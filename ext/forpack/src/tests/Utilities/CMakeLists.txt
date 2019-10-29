# Get the name of the component
get_filename_component( COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" ABSOLUTE)
get_filename_component( COMPONENT_NAME "${COMPONENT_NAME}" NAME)

# Set the name of the test
set ( TEST_NAME "${PROJECT_NAME}-${COMPONENT_NAME}-Test" )

# Set the name of the test executable
set ( TEST_EXECUTABLE_NAME "${TEST_NAME}.x" )

# Set the list of pFUnit Fortran files, regular Fortran files and include files
set( pfunit_sources
    Tests_Utilities_AddElementToArray.F90
    Tests_Utilities_GetOptArgValue.F90
#     *** TODO ***
#     Tests_Utilities_GetPosition.F90
#     Tests_Utilities_GetPositions.F90
#     Tests_Utilities_LocateValue.F90
#     Tests_Utilities_ParseNamesValues.F90
#     Tests_Utilities_PresentAndNotEmpty.F90
#     Tests_Utilities_PresentAndTrue.F90
#     Tests_Utilities_RemoveElementFromArray.F90
#     *** TODO ***
)
set( test_sources     "" )
set( included_sources
  test_AddVar0dToVar1d_Inline.F90
  test_AddVar1dToVar1d_Inline.F90
  test_AddVar1dToVar2d_Inline.F90
  test_AddVar2dToVar2d_Inline.F90
  test_AddVar3dToVar4d_Inline.F90
  GetOptArgValue-inline.F90
)



set( CurrentLogLevel "10" )
# set( CurrentLogLevel "10"   CACHE INTERNAL "" )
# Creating the test
create_pFUnit_test( ${TEST_NAME} "${pfunit_sources}" "${test_sources}"
  EXECUTABLE_NAME   ${TEST_EXECUTABLE_NAME}
  LIST_LIBRARIES    "${STATIC_LIBRARY}"
  INCLUDE_FILES     "${included_sources}"
)
