# Get the name of the component
get_filename_component( COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" ABSOLUTE)
get_filename_component( COMPONENT_NAME "${COMPONENT_NAME}" NAME)

# Set the name of the test
set ( TEST_NAME "${PROJECT_NAME}-${COMPONENT_NAME}-Test" )

# Set the name of the test executable
set ( TEST_EXECUTABLE_NAME "${TEST_NAME}.x" )

# Set the list of pFUnit Fortran files, regular Fortran files and include files
set( pfunit_sources
    Test_String_ParseNumbersLetters.F90
    Test_String_Vectrim.F90
    Test_String_Split.F90
    Test_String_IsExponent.F90
    Test_String_GetLettersRightOfNumbers.F90
    Test_String_Parse.F90
    Test_String_GetNumberOfItems.F90
    Test_String_SetLength.F90
    Test_String_SetSameLength.F90
    Test_String_GetSeconds.F90
    Test_String_IsFunction.F90
    Test_String_RemoveDuplicateCharacters.F90
    Test_String_NiceInteger.F90
    Test_String_ParseFunction.F90
    Test_String_ReplaceCharacter.F90
    Test_String_EscapeFileCharacters.F90
    Test_String_GetSubString.F90
    Test_String_GetNumberOfDigits.F90
    Test_String_GetFormat.F90
    Test_String_RemoveDuplicate.F90
    Test_String_IsIntegerNumber.F90
    Test_String_IsRealNumber.F90
#
#     Test_StringClass_Assign.F90
)
set( test_sources "" )
set( included_sources "" )

# Creating the test
create_pFUnit_test( ${TEST_NAME} "${pfunit_sources}" "${test_sources}"
  EXECUTABLE_NAME   ${TEST_EXECUTABLE_NAME}
  LIST_LIBRARIES    "${STATIC_LIBRARY}"
  INCLUDE_FILES     "${included_sources}"
  )