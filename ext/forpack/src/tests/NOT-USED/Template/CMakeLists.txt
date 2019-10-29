# Get the name of the component
get_filename_component( COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" ABSOLUTE)
get_filename_component( COMPONENT_NAME "${COMPONENT_NAME}" NAME)

# Set the name of the test
set ( TEST_NAME "${PROJECT_NAME}-${COMPONENT_NAME}-Test" )

# Set the name of the test executable
set ( TEST_EXECUTABLE_NAME "${TEST_NAME}.x" )

# Set the list of pFUnit Fortran files
set( pfunit_sources Template_TestCase.f90 )

# Set the list of regular Fortran files
set( test_sources TemplateTestCase_Class.f90 )

# Create a pFUnit-based test executable and ctest test
create_pFUnit_test( ${TEST_NAME} ${TEST_EXECUTABLE_NAME} "${pfunit_sources}" "${test_sources}")

# Linking the current library to the pFUnit-based test executable
target_link_libraries( ${TEST_EXECUTABLE_NAME} ${STATIC_LIBRARY} )