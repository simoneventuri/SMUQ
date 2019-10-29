#==========================================================================
#
# create_pFUnit_test
#
#   Does everything needed to create a pFUnit-based test.
#   This function wraps add executable, add_test, and define_pFUnit_failure.
#   Required input variables are:
#   - TEST_NAME:            test name,
#   - pf_file_list:         list of pfunit files,
#   - fortran_source_list:  list of Fortran files.
#   Optional input variables are GEN_OUTPUT_DIR and COMMAND (see usage notes at the
#   top of this file for details).
#   If executables need to be prefixed with an mpirun command, this prefix (e.g.,
#   "mpirun") should be given in the CMAKE variable PFUNIT_MPIRUN.
#
# Example
# *******
#   create_pFUnit_test( "My/App/Directory" )
#
#   create_pFUnit_test( "My/App/Directory"
#       EXECUTABLE_NAME   "MySparkApp"
#       TARGET_NAME       "CustomizedTargetName"
#       LIST_LIBRARIES    "-ltot -ltata"
#       INSTALL_DIR       "bin"
#   )

#
# Required arguments
# ******************
#   TEST_NAME           Name of the test to be generated.
#
# Optional arguments
# ******************
#   EXECUTABLE_NAME     Name of the executable associated with this application.
#                       Defaults to the name of the test with the ".x" extension.
#   GEN_OUTPUT_DIR      Directory where to store the generated Fortran files, both the
#                       files created by 'fpp' and 'pfunit'.
#                       Default value is CMAKE_CURRENT_BINARY_DIR.
#                       Defaults to EXECUTABLE_NAME in lowercase
#   LIST_LIBRARIES      List of libraries to link to the exectuable or list of
#                       target whose public librairies have to be linked.
#                       Default is an empty string.
#   INCLUDE_DIR         List of include directories.
#                       Default is an empty string.
#   INCLUDE_FILES       List of included source files.
#                       Default is an empty string.
#   VERBOSE             Verbose indicator. Defaul OFF
#
# Non-standard CMake functions/variables
# **************************************
#   LogHeavyDebug          Print log message
#
#==========================================================================
# # # Utilities for using pFUnit's preprocessor and provided driver file.
# # # This module relies upon the variables defined by the FindpFUnit module.
# # # define_pFUnit_failure
# # # Arguments:
# # # Defines FAIL_REGULAR_EXPRESSION and PASS_REGULAR_EXPRESSION for the given
# # # test, so that pFUnit's overall pass/fail status can be detected.

function( create_pFUnit_test
  TEST_NAME
  pf_file_list
  fortran_source_list
  )

  set( KEY "[create_pFUnit_test] " )
  LogHeavyDebug( "${KEY}Mandatory parameters" )
  LogHeavyDebug( "${KEY}-> TEST_NAME                ${TEST_NAME}" )
  LogHeavyDebug( "${KEY}-> pf_file_list             ${pf_file_list}" )
  LogHeavyDebug( "${KEY}-> fortran_source_list      ${fortran_source_list}" )


#   LogHeavyDebug( "${KEY}Default values for optional arguments" )
  set( DEF_EXECUTABLE_NAME  "${TEST_NAME}.x" )                                    # Set the name of the test executable
  set( DEF_GEN_OUTPUT_DIR   "${CMAKE_CURRENT_BINARY_DIR}/${DEF_GEN_OUTPUT_DIR}" ) # set absolute path
  set( DEF_LIST_LIBRARIES   "" )
  set( DEF_INCLUDE_DIR      "" )
  set( DEF_INCLUDE_FILES    "" )
  get_filename_component( DEF_GEN_OUTPUT_DIR "${DEF_GEN_OUTPUT_DIR}" ABSOLUTE)    # trick to remove the trailing slash
  LogHeavyDebug( "${KEY}-> DEF_EXECUTABLE_NAME     ${DEF_EXECUTABLE_NAME}" )
  LogHeavyDebug( "${KEY}-> DEF_GEN_OUTPUT_DIR      ${DEF_GEN_OUTPUT_DIR}" )
  LogHeavyDebug( "${KEY}-> DEF_LIST_LIBRARIES      ${DEF_LIST_LIBRARIES}" )
  LogHeavyDebug( "${KEY}-> DEF_INCLUDE_DIR         ${DEF_INCLUDE_DIR}" )
  LogHeavyDebug( "${KEY}-> DEF_INCLUDE_FILES       ${DEF_INCLUDE_FILES}" )



#   LogHeavyDebug( "${KEY}Processing optional arguments" )
  set( options        "" )
  set( oneValueArgs   EXECUTABLE_NAME GEN_OUTPUT_DIR EXAMPLE )
  set( multiValueArgs COMMAND LIST_LIBRARIES INCLUDE_DIR INCLUDE_FILES )
  cmake_parse_arguments( OPT "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  if (OPT_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unknown keywords given to create_pFUnit_test(): \"${OPT_UNPARSED_ARGUMENTS}\"")
  endif()
#   LogHeavyDebug( "${KEY}-> OPT_EXECUTABLE_NAME      ${OPT_EXECUTABLE_NAME}" )
#   LogHeavyDebug( "${KEY}-> OPT_GEN_OUTPUT_DIR       ${OPT_GEN_OUTPUT_DIR}" )
#   LogHeavyDebug( "${KEY}-> OPT_LIST_LIBRARIES       ${OPT_LIST_LIBRARIES}" )
#   LogHeavyDebug( "${KEY}-> OPT_INCLUDE_DIR          ${INCLUDE_DIR}" )
#   LogHeavyDebug( "${KEY}-> OPT_INCLUDE_FILES        ${OPT_INCLUDE_FILES}" )



  if ( DEFINED OPT_EXECUTABLE_NAME )
    set( EXECUTABLE_NAME  ${OPT_EXECUTABLE_NAME} )
  else()
    set( EXECUTABLE_NAME  ${DEF_EXECUTABLE_NAME} )
  endif()
  if ( DEFINED OPT_GEN_OUTPUT_DIR )
    set( GEN_OUTPUT_DIR  ${OPT_GEN_OUTPUT_DIR} )
  else()
    set( GEN_OUTPUT_DIR  ${DEF_GEN_OUTPUT_DIR} )
  endif()
  if ( DEFINED OPT_LIST_LIBRARIES )
    set( LIST_LIBRARIES  ${OPT_LIST_LIBRARIES} )
  else()
    set( LIST_LIBRARIES  ${DEF_LIST_LIBRARIES} )
  endif()
  if ( DEFINED OPT_INCLUDE_DIR )
    set( INCLUDE_DIR  ${OPT_INCLUDE_DIR} )
  else()
    set( INCLUDE_DIR  ${DEF_INCLUDE_DIR} )
  endif()
  if ( DEFINED OPT_INCLUDE_FILES )
    set( INCLUDE_FILES  ${OPT_INCLUDE_FILES} )
  else()
    set( INCLUDE_FILES  ${DEF_INCLUDE_FILES} )
  endif()
  LogHeavyDebug( "${KEY}Optional parameters" )
  LogHeavyDebug( "${KEY}-> EXECUTABLE_NAME          ${EXECUTABLE_NAME}" )
  LogHeavyDebug( "${KEY}-> GEN_OUTPUT_DIR           ${GEN_OUTPUT_DIR}" )
  LogHeavyDebug( "${KEY}-> LIST_LIBRARIES           ${LIST_LIBRARIES}" )
  LogHeavyDebug( "${KEY}-> INCLUDE_DIR              ${INCLUDE_DIR}" )
  LogHeavyDebug( "${KEY}-> INCLUDE_FILES            ${INCLUDE_FILES}" )

  # Setting pfunit command line
  LogHeavyDebug( "${KEY}Setting pfunit command line" )
  if (NOT OPT_COMMAND)
    set( OPT_COMMAND ${EXECUTABLE_NAME})
  endif()
  set(OPT_COMMAND ${PFUNIT_MPIRUN} ${OPT_COMMAND})
  set(OPT_COMMAND ${PFUNIT_MPIRUN} "${OPT_COMMAND}" "-robust" "-xml" "${EXECUTABLE_NAME}.xml"  ) # Lopez
  LogHeavyDebug( "${KEY}-> OPT_COMMAND              ${OPT_COMMAND}" )

  set( TestTarget ${EXECUTABLE_NAME} )

  # Handle source code generation, add to list of sources.
  LogHeavyDebug( "${KEY}Calling process_pFUnit_source_list" )
  process_pFUnit_source_list(
    "${pf_file_list}"
    ${GEN_OUTPUT_DIR}
    "${INCLUDE_FILES}"
    fortran_source_list
    generated_target_list
  )
  LogHeavyDebug( "${KEY}-> fortran_source_list     ${fortran_source_list}" )
  LogHeavyDebug( "${KEY}-> generated_target_list   ${generated_target_list}" )

  # Create the executable, add dependencies to the generated files,
  # link to pFunit library and add the pFunit include directory
  LogHeavyDebug( "${KEY}Calling add_executable" )
  add_executable( ${TestTarget} ${fortran_source_list} )
  add_dependencies( ${TestTarget} ${generated_target_list} )
  target_link_libraries( ${TestTarget} PUBLIC ${PFUNIT_LIBRARIES} )
  target_include_directories( ${TestTarget} PRIVATE ${PFUNIT_INCLUDE_DIRS} )

  # Link to the input libraries and add the input include directory
  LogHeavyDebug( "${KEY}-> LIST_LIBRARIES     ${LIST_LIBRARIES}" )
  target_link_libraries( ${EXECUTABLE_NAME} PUBLIC ${LIST_LIBRARIES} )
  target_include_directories( ${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIR} )

  target_include_directories( ${EXECUTABLE_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} )
  LogInfo( "${KEY}-> !@# CMAKE_CURRENT_BINARY_DIR         ${CMAKE_CURRENT_BINARY_DIR}" )

  # Creating the test
  add_test(
    NAME      ${TEST_NAME}
    COMMAND   ${OPT_COMMAND}
  )

  # Tells CTest what regular expressions are used to signal pass/fail from
  # pFUnit output. Set both pass and fail regular expressions to minimize
  # the chance that the system under test will interfere with output and
  # cause a false negative.
  set_tests_properties( ${TEST_NAME} PROPERTIES FAIL_REGULAR_EXPRESSION "FAILURES!!!" )
  set_tests_properties( ${TEST_NAME} PROPERTIES PASS_REGULAR_EXPRESSION "OK" )

endfunction(create_pFUnit_test)

include(CMakeParseArguments)

if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
  set( PREPROCESSOR_COMMAND "cpp" )
  set( FPP_FLAGS "--traditional" )
elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
  set( PREPROCESSOR_COMMAND "fpp" )
  set( FPP_FLAGS "" )
endif ()


# find_program( FPP_EXECUTABLE fpp )
find_program( FPP_EXECUTABLE ${PREPROCESSOR_COMMAND} )
if(NOT FPP_EXECUTABLE)
  message(SEND_ERROR "Could not find the Fortran Preprocessor FPP")
endif()


# This function manages most of the work involved in preprocessing pFUnit
# files. You provide every *.pf file for a given executable, an output
# directory where generated sources should be output, and a list name. It
# will generate the sources, and append them and the pFUnit driver to the
# named list.
function(process_pFUnit_source_list
  pf_file_list
  output_directory
  include_file_list
  fortran_list_name
  target_list )

  set( KEY "[process_pFUnit_source_list]:" )
  LogHeavyDebug( "${KEY}-> pf_file_list               ${pf_file_list}" )
  LogHeavyDebug( "${KEY}-> include_file_list          ${include_file_list}" )

  # Getting list of include directories for fpp
  get_directory_property( LIST_INCLUDE_DIR INCLUDE_DIRECTORIES )
  set( FPP_INCLUDE_FLAGS_TEST )
  foreach( i ${LIST_INCLUDE_DIR})
    set( FPP_INCLUDE_FLAGS_TEST "${FPP_INCLUDE_FLAGS_TEST}" "-I${i}")
  endforeach()
  LogHeavyDebug( "${KEY}-> FPP_INCLUDE_FLAGS_TEST     ${FPP_INCLUDE_FLAGS_TEST}" )

  # Getting list of definitions for fpp
  get_directory_property( LIST_DEFINITIONS COMPILE_DEFINITIONS )
  set( FPP_DEFINITION_FLAGS_TEST)
  foreach( i ${LIST_DEFINITIONS})
    set( FPP_DEFINITION_FLAGS_TEST "${FPP_DEFINITION_FLAGS_TEST}" "-D${i}" )
  endforeach()
  LogHeavyDebug( "${KEY}-> FPP_DEFINITION_FLAGS_TEST  ${FPP_DEFINITION_FLAGS_TEST}" )

  set( FPP_COMMAND_PREFIX ${FPP_EXECUTABLE} ${FPP_FLAGS} ${FPP_INCLUDE_FLAGS_TEST} ${FPP_DEFINITION_FLAGS_TEST} )
  LogHeavyDebug( "${KEY}-> FPP_COMMAND_PREFIX         ${FPP_COMMAND_PREFIX}" )


  list( LENGTH pf_file_list NumberSource )
  LogHeavyDebug( "${KEY}-> Number of pfunit source files: ${NumberSource}" )

  foreach( pf_file IN LISTS pf_file_list )

    get_filename_component( pf_file  "${pf_file}" ABSOLUTE )
    get_filename_component( BaseName "${pf_file}" NAME_WE )

    set( Input_File     ${pf_file} )
#     set( Temporary_File ${output_directory}/${BaseName}.FPP )
    set( Temporary_File ${BaseName}.FPP )
    set( Output_File    ${output_directory}/${BaseName}.F90 )
    set( FPP_COMMAND    ${FPP_COMMAND_PREFIX} ${Input_File} ${Temporary_File} )
    set( PFUNIT_COMMAND ${PFUNIT_PARSER} ${Temporary_File} ${Output_File} > /dev/null 2>&1 )

    LogHeavyDebug( "${KEY}  -> Input_File               ${Input_File}" )
    LogHeavyDebug( "${KEY}  -> Temporary_File           ${Temporary_File}" )
    LogHeavyDebug( "${KEY}  -> Output_File              ${Output_File}" )
    LogHeavyDebug( "${KEY}  -> FPP_COMMAND              ${FPP_COMMAND}" )
    LogHeavyDebug( "${KEY}  -> PFUNIT_COMMAND           ${PFUNIT_COMMAND}" )

    add_custom_command(
      OUTPUT        ${Output_File}
      COMMAND       ${FPP_COMMAND}
      COMMAND       ${PFUNIT_COMMAND}
      DEPENDS       ${Input_File} ${include_file_list}
      )

    set_source_files_properties( ${Output_File} PROPERTIES GENERATED TRUE )
    add_custom_target( ${BaseName} DEPENDS ${Output_File}  )
    list( APPEND ${target_list} ${BaseName} )

    LogHeavyDebug( "${KEY}  -> Appending Output_File='${Output_File}' to list of fortran files fortran_list_name" )
    list( APPEND ${fortran_list_name} ${Output_File} )
    set( testSuites_contents "${testSuites_contents}ADD_TEST_SUITE(${BaseName}_suite)\n" ) # Add the file to testSuites.inc

    LogHeavyDebug( "${KEY}  -> fortran_list_name   ${${fortran_list_name}}" )
    LogHeavyDebug( "${KEY}  -> testSuites_contents ${testSuites_contents}" )

    # Required to re-complie the Driver when a new pf file is added to the cmakeLists.txt file
    set_source_files_properties( ${PFUNIT_DRIVER} PROPERTIES
      OBJECT_DEPENDS  "${Output_File}"
    )

  endforeach()

  # Regenerate testSuites.inc if and only if necessary.
  if ( EXISTS ${output_directory}/testSuites.inc )
    file(READ ${output_directory}/testSuites.inc old_testSuites_contents)
  endif()

  if ( NOT testSuites_contents STREQUAL old_testSuites_contents )
    file(WRITE ${output_directory}/testSuites.inc ${testSuites_contents})
  endif()

  # Export ${fortran_list_name} to the caller, and add ${PFUNIT_DRIVER} to it.
  LogHeavyDebug( "${KEY}  -> Appending PFUNIT_DRIVER='${PFUNIT_DRIVER}' to list of fortran files fortran_list_name" )
#   list( APPEND ${fortran_list_name} ${PFUNIT_DRIVER} )
  set( ${fortran_list_name} "${${fortran_list_name}}" "${PFUNIT_DRIVER}" PARENT_SCOPE)
#   set( ${fortran_list_name} "${${fortran_list_name}}" "driver.F90" PARENT_SCOPE)
#   set( ${fortran_list_name} "${${fortran_list_name}}" "/home/blopez/tmp/driver.F90" PARENT_SCOPE)


  set( ${target_list} "${${target_list}}" PARENT_SCOPE )

endfunction(process_pFUnit_source_list)


