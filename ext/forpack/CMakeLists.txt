cmake_minimum_required (VERSION 3.7)
project( Forpack VERSION 1.2.6 )
set( PROJECT_ACRONYM_DEFINITION "Fortran Package" )


######################################################
# Setting the directory which contains CMake modules #
######################################################
# Appends the cmake path inside the MAKE_MODULE_PATH variable which stores the
# list of directories where to lookk for additional CMake modules.
set( CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules  ${CMAKE_MODULE_PATH} )


###########################
# Including cmake modules #
###########################
include( Logging )                    # Add function to write log cmake message
include( add_sources )                # Add function to add a set of source files.
include( EnsureOutOfSourceBuild )     # Add function to enforce out-of-source build
ensure_out_of_source_build()          # Ensures an out-of-source-build
setLogsOn()                           # Activate logs:  setLogsOn   setLogsOff
setLogLevel( "INFO" )                 # Setting a specific loglevel:  INFO  DEBUG  HEAVYDEBUG


##########################
# Setting git parameters #
##########################
LogInfo( "----------------------" )
LogInfo( "Setting git parameters" )
LogInfo( "----------------------" )
include( SetGitVersion )
string( REPLACE "/" "_" GIT_CONFIG "${GIT_BRANCH}" )    # @TODO: Set the GIT_CONFIG variable inside the SetGitVersion module
LogInfo( "-> PROJECT_FULL_VERSION:      ${PROJECT_FULL_VERSION}" )
LogInfo( "-> PROJECT_VERSION_MAJOR:     ${PROJECT_VERSION_MAJOR}" )
LogInfo( "-> PROJECT_VERSION_MINOR:     ${PROJECT_VERSION_MINOR}" )
LogInfo( "-> PROJECT_VERSION_PATCH:     ${PROJECT_VERSION_PATCH}" )
LogInfo( "-> PROJECT_VERSION:           ${PROJECT_VERSION}" )
LogInfo( "-> GIT_BRANCH:                ${GIT_BRANCH}" )
LogInfo( "-> GIT_SHA1:                  ${GIT_SHA1}" )
LogInfo( "-> GIT_TAG:                   ${GIT_TAG}" )
LogInfo( "-> GIT_CONFIG:                ${GIT_CONFIG}" )


##############################
# Setting project parameters #
##############################
LogInfo( "--------------------------" )
LogInfo( "Setting project parameters" )
LogInfo( "--------------------------" )
string( TOLOWER ${PROJECT_NAME} PROJECT_NAME_LC )
string( TOUPPER ${PROJECT_NAME} PROJECT_NAME_UC )
string( TOLOWER ${PROJECT_NAME} LIBRARY_NAME )
string( TIMESTAMP BUILD_DATE %Y-%m-%d )
LogInfo( "-> BUILD_DATE:                ${BUILD_DATE}" )
LogInfo( "-> PROJECT_NAME:              ${PROJECT_NAME}" )
LogInfo( "-> PROJECT_VERSION:           ${PROJECT_VERSION}" )
LogInfo( "-> LIBRARY_NAME:              ${LIBRARY_NAME}" )


#########################
# Setting build options #
#########################
LogInfo( "---------------------" )
LogInfo( "Setting build options" )
LogInfo( "---------------------" )
option( BUILD_STATIC_LIBRARY  "Build static library"          ON  )
option( BUILD_SHARED_LIBRARY  "Build shared library"          OFF )
option( BUILD_TESTS           "Build tests"             OFF )
option( BUILD_EXAMPLES        "Build examples"          OFF )
LogInfo( "-> BUILD_STATIC_LIBRARY       ${BUILD_STATIC_LIBRARY}" )
LogInfo( "-> BUILD_SHARED_LIBRARY       ${BUILD_SHARED_LIBRARY}" )
LogInfo( "-> BUILD_EXAMPLES             ${BUILD_EXAMPLES}" )
LogInfo( "-> BUILD_TESTS                ${BUILD_TESTS}" )


############################
# Setting build parameters #
############################
LogInfo( "---------------------------" )
LogInfo( "Setting build configuration" )
LogInfo( "---------------------------" )
if ( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select the type of build" FORCE )
endif()
set_property ( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" )
LogInfo( "-> CMAKE_BUILD_TYPE           ${CMAKE_BUILD_TYPE}" )

# The Fortran compiler is set here. Currently, only intel Fortran compiler is supported.
# If no Fortran compiler has been defined, then ifort is used by default.
# The selection of the Fortran compiler must be done BEFORE 'enable_language(Fortran)'.
#if ( NOT DEFINED CMAKE_Fortran_COMPILER)
#  set( CMAKE_Fortran_COMPILER "ifort" CACHE STRING "" FORCE)
#endif()
enable_language( Fortran )

# Setting the Fortran compiler version
# Workaround for intel version >= 19.0.1 which is not well detected by cmake (It founds version '19.0.0')
if ( ${CMAKE_Fortran_COMPILER_VERSION} VERSION_EQUAL "19.0.0.20181018" )
  LogInfo( "-> Fixing version: CMAKE_Fortran_COMPILER_VERSION '${CMAKE_Fortran_COMPILER_VERSION}' ->'19.0.1'" )
  set( CMAKE_Fortran_COMPILER_VERSION "19.0.1" )
elseif (  ${CMAKE_Fortran_COMPILER_VERSION} VERSION_EQUAL "19.0.0.20190117" )
  LogInfo( "-> Fixing version: CMAKE_Fortran_COMPILER_VERSION '${CMAKE_Fortran_COMPILER_VERSION}' ->'19.0.2'" )
  set( CMAKE_Fortran_COMPILER_VERSION "19.0.2" )
elseif (  ${CMAKE_Fortran_COMPILER_VERSION} VERSION_EQUAL "19.0.0.20190206" )
  LogInfo( "-> Fixing version: CMAKE_Fortran_COMPILER_VERSION '${CMAKE_Fortran_COMPILER_VERSION}' ->'19.0.3'" )
  set( CMAKE_Fortran_COMPILER_VERSION "19.0.3" )
endif()
string( REPLACE "." ";" VERSION_LIST ${CMAKE_Fortran_COMPILER_VERSION} )
list(GET VERSION_LIST 0 Fortran_COMPILER_VERSION_MAJOR)
list(GET VERSION_LIST 1 Fortran_COMPILER_VERSION_MINOR)
list(GET VERSION_LIST 2 Fortran_COMPILER_VERSION_PATCH)
set( CMAKE_Fortran_COMPILER_VERSION "${Fortran_COMPILER_VERSION_MAJOR}.${Fortran_COMPILER_VERSION_MINOR}.${Fortran_COMPILER_VERSION_PATCH}" )
set( Fortran_COMPILER_CONFIG ${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION} )
string( TOLOWER ${Fortran_COMPILER_CONFIG} Fortran_COMPILER_CONFIG )


##########################################
# Setting the build configuration string #
##########################################
if ( NOT CONFIG_SUFFIX )
  set( CONFIG_SUFFIX "" CACHE STRING "Name of suffix to apply to the configuration string")
else()
  set( CONFIG_SUFFIX "-${CONFIG_SUFFIX}" )
endif()
if ( NOT BRANCH_IN_CONFIG )
  set( PROJECT_BUILD_CONFIGURATION ${PROJECT_VERSION}${PARALLEL_TYPE}-${CMAKE_BUILD_TYPE}-${Fortran_COMPILER_CONFIG}${CONFIG_SUFFIX} )
else()
  set( PROJECT_BUILD_CONFIGURATION ${PROJECT_VERSION}-${GIT_CONFIG}${PARALLEL_TYPE}-${CMAKE_BUILD_TYPE}-${Fortran_COMPILER_CONFIG}${CONFIG_SUFFIX} )
endif()
string( TOLOWER ${PROJECT_BUILD_CONFIGURATION} PROJECT_BUILD_CONFIGURATION )
set( PROJECT_FULL_NAME ${PROJECT_NAME_LC}-${PROJECT_BUILD_CONFIGURATION} )
LogInfo( "-> CONFIG_SUFFIX                ${CONFIG_SUFFIX}" )
LogInfo( "-> PROJECT_BUILD_CONFIGURATION  ${PROJECT_BUILD_CONFIGURATION}" )
LogInfo( "-> PROJECT_FULL_NAME            ${PROJECT_FULL_NAME}" )


################################
# Setting required modulefiles #
################################
LogInfo( "----------------------------" )
LogInfo( "Setting required modulefiles" )
LogInfo( "----------------------------" )
# If cmake has been called with the '-DREQUIRED_MODULEFILES=file-1,...,file-n' option,
# then the value 'file-1,...,file-n' corresponds to the list of require modulefiles.
# First, the separator ',' is replaced by the ';' character so that this value can be stored
# in a cmake list. Then, this list of (input) required module files is added to the global
# list stored in the cmake variable 'LIST_REQUIRED_MODULEFILES'.
# Later, when other packages are loaded (ForPack, HDF5, Petsc, ...), if the packages define
# a variable '<PACKAGE_NAME>_MODULEFILE', then this variable is addes to the gloabl list
# 'LIST_REQUIRED_MODULEFILES' if the modulefile is not already present
macro( add_items_to_list items items_list )
  foreach( item ${${items}} )
    set( AddToGlobalList ON )
    if ( "${${items}}" IN_LIST ${items_list} )
      set( AddToGlobalList OFF )
    endif()
    if ( AddToGlobalList )
      list( APPEND ${items_list} "${item}" )
    endif()
  endforeach()
endmacro(add_items_to_list)
if ( DEFINED REQUIRED_MODULEFILES )
  LogInfo( "-> REQUIRED_MODULEFILES       ${REQUIRED_MODULEFILES}" )
  string( REPLACE "," ";" TMP "${REQUIRED_MODULEFILES}" )
  set( REQUIRED_MODULEFILES ${TMP} )
  add_items_to_list( REQUIRED_MODULEFILES LIST_REQUIRED_MODULEFILES )
endif()
LogInfo( "-> LIST_REQUIRED_MODULEFILES  ${LIST_REQUIRED_MODULEFILES}" )


############################
# Setting Fortran compiler #
############################
# The Fortran compiler flags are set in the module 'SetFortranFlags'.
# The 'SetFortranFlags' need to be called after all the dependent packages have
# beend found
LogInfo( "------------------------" )
LogInfo( "Setting Fortran compiler" )
LogInfo( "------------------------" )
include( SetFortranFlags )


#####################################
# Setting path of build directories #
#####################################
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib )  # Set the name of the directory where to store libraries
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin )  # Set the name of the directory where to store executables
set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod )  # Set the name of the directory where to store Fortran module files '*.mod'
set( SOURCE_BUILD_DIRECTORY         ${CMAKE_CURRENT_BINARY_DIR}/src )  # Set the name of the directory where to store build source files


########################################
# Setting installation directory paths #
########################################
# Setup the directories where to install the code
# if ( CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT )
#   LogDebug( "Changing the default installation prefix is used")
#   LogDebug( "-> from: CMAKE_INSTALL_PREFIX   = ${CMAKE_INSTALL_PREFIX}")
# #   set( CMAKE_INSTALL_PREFIX "/opt/${PROJECT_NAME_LC}/${PROJECT_BUILD_CONFIGURATION}" CACHE PATH "Installation prefix" FORCE  )
# #   set( CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/../install/${PROJECT_BUILD_CONFIGURATION}" CACHE PATH "Installation prefix" FORCE  )
#   set( CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/../install/Test" CACHE PATH "Installation prefix" FORCE  )
#   LogDebug( "-> to:   CMAKE_INSTALL_PREFIX   = ${CMAKE_INSTALL_PREFIX}")
# endif()
LogInfo( "------------------------------------" )
LogInfo( "Setting installation directory paths" )
LogInfo( "------------------------------------" )
set( PROJECT_INSTALL_DIR      ${CMAKE_INSTALL_PREFIX} )#/${PROJECT_FULL_NAME} )         # Installation directory for libraries (both static and shared)
get_filename_component( PROJECT_INSTALL_DIR "${PROJECT_INSTALL_DIR}" ABSOLUTE)
set( LIBRARY_INSTALL_DIR      ${PROJECT_INSTALL_DIR}/lib )          # Installation directory for libraries (static/shared)
set( MODULES_INSTALL_DIR      ${PROJECT_INSTALL_DIR}/mod )          # Installation directory for Fortran modules
set( INCLUDE_INSTALL_DIR      ${PROJECT_INSTALL_DIR}/mod )          # Installation directory for include files
set( EXECUTABLE_INSTALL_DIR   ${PROJECT_INSTALL_DIR}/bin )          # Installation directory for executables
set( SCRIPT_INSTALL_DIR       ${PROJECT_INSTALL_DIR}/scripts )      # Installation directory for scripts
set( CMAKE_INSTALL_DIR        ${PROJECT_INSTALL_DIR}/cmake )        # Installation directory for cmake configuration files
set( DATABASE_INSTALL_DIR     ${PROJECT_INSTALL_DIR}/dtb )          # Installation directory for database files
LogInfo( "-> CMAKE_INSTALL_PREFIX       ${CMAKE_INSTALL_PREFIX}")
LogInfo( "-> PROJECT_INSTALL_DIR        ${PROJECT_INSTALL_DIR}")
LogInfo( "-> LIBRARY_INSTALL_DIR        ${LIBRARY_INSTALL_DIR}")
LogInfo( "-> MODULES_INSTALL_DIR        ${MODULES_INSTALL_DIR}")
LogInfo( "-> INCLUDE_INSTALL_DIR        ${INCLUDE_INSTALL_DIR}")
LogInfo( "-> EXECUTABLE_INSTALL_DIR     ${EXECUTABLE_INSTALL_DIR}")
LogInfo( "-> CMAKE_INSTALL_DIR          ${CMAKE_INSTALL_DIR}")
LogInfo( "-> SCRIPT_INSTALL_DIR         ${SCRIPT_INSTALL_DIR}")
if ( DEFINED MODULEFILE_INSTALL_DIR )
LogInfo( "-> MODULEFILE_INSTALL_DIR     ${MODULEFILE_INSTALL_DIR}")
endif()


######################################
# Setting path of source directories #
######################################
set( FORPACK_INCLUDE_FILE     "forpack-include.inc" )
set( INCLUDE_SOURCE_DIR       ${PROJECT_SOURCE_DIR}/src/include )                                      # Directory containing the include files within the sources
LogInfo( "Path of sub-directories within the source directory")
LogInfo( "-> Directory for include files:             INCLUDE_SOURCE_DIR       ${INCLUDE_SOURCE_DIR}"  )


#######################################################
# Adding path to directories containing include files #
#######################################################
include_directories( ${INCLUDE_SOURCE_DIR} )


######################
# Building libraries #
######################
LogInfo( "------------------" )
LogInfo( "Building libraries" )
LogInfo( "------------------" )

set( DIRECTORY "src/lib" )
add_subdirectory( ${DIRECTORY} )
get_sources( SRCS )
list( APPEND LIST_LIBRARIES_FFLAGS "-l${LIBRARY_NAME}" )

if ( BUILD_STATIC_LIBRARY )
LogInfo( "-> Building static library: ${DIRECTORY}" )
  set( STATIC_LIBRARY ${LIBRARY_NAME}-static )
  set( TARGET_NAME ${STATIC_LIBRARY} )
  add_library( ${TARGET_NAME} STATIC ${SRCS} )
  set_target_properties( ${TARGET_NAME} PROPERTIES OUTPUT_NAME ${LIBRARY_NAME})
  install( TARGETS ${TARGET_NAME} DESTINATION ${LIBRARY_INSTALL_DIR} EXPORT ${LIBRARY_NAME}-targets )
endif()

if ( BUILD_SHARED_LIBRARY )
  LogInfo( "Building shared library: ${DIRECTORY}" )
  set( SHARED_LIBRARY ${LIBRARY_NAME}-shared )
  set( TARGET_NAME ${SHARED_LIBRARY} )
  add_library( ${TARGET_NAME} SHARED ${SRCS} )
  set_target_properties( ${TARGET_NAME} PROPERTIES OUTPUT_NAME ${LIBRARY_NAME})
  install( TARGETS ${TARGET_NAME} DESTINATION ${LIBRARY_INSTALL_DIR} EXPORT ${LIBRARY_NAME}-targets )
endif()


#####################
# Building examples #
#####################
if ( BUILD_EXAMPLES )
  LogInfo( "-----------------" )
  LogInfo( "Building examples" )
  LogInfo( "-----------------" )
  add_subdirectory( examples )
endif()


##################
# Building tests #
##################
if ( BUILD_TESTS )
  LogInfo( "--------------" )
  LogInfo( "Building tests" )
  LogInfo( "--------------" )
  set( PACKAGE_NAME "pFUnit" )
  find_package( ${PACKAGE_NAME} QUIET )
  LogInfo( "-> PFUNIT_FOUND         = ${PFUNIT_FOUND}")
  if ( ${PFUNIT_FOUND} )
    LogDebug( "-> PFUNIT_LIBRARY       = ${PFUNIT_LIBRARY}")
    LogDebug( "-> PFUNIT_INCLUDE_DIR   = ${PFUNIT_INCLUDE_DIR}" )
    LogDebug( "-> PFUNIT_DRIVER        = ${PFUNIT_DRIVER}" )
    LogDebug( "-> PFUNIT_MODULE_DIR    = ${PFUNIT_MODULE_DIR}" )
    LogDebug( "-> PFUNIT_PARSER        = ${PFUNIT_PARSER}" )
    include(pFUnit_tools)       # Includes tools for pFUnit
    include_directories( ${PFUNIT_MODULE_DIR} )
    link_directories( ${PFUNIT_LIBRARY} )
    enable_testing()
    set( DIRECTORY         "src/tests" )
    LogInfo( "-> Building tests: ${DIRECTORY}" )
    add_subdirectory( ${DIRECTORY} )
  else()
    LogInfo( "-> PFUnit not found => disenabling tests" )
    set( BUILD_TESTS OFF )
  endif()
else()
  LogInfo( "Tests not build" )
endif()


#####################################################
# Setting variable for env-var script and modulfile #
#####################################################
LogInfo( "-------------------------------------------------" )
LogInfo( "Setting variable for env-var script and modulfile" )
LogInfo( "-------------------------------------------------" )
# Here, we want to create the set of variables which will be used in the environement
# variable scriopt and in the module file to build external applications or tests.
# the following variable are created:
# * LIBRARIES_FFLAGS        List of libraries required during linking
# * FORTRAN_FLAGS           List of Fortran compilation flags used to compiled the code
# * DEFINITION_FFLAGS       List of Fortran definitions used to compiled the code
# * INCLUDE_DIR_FFLAGS      List of include directories required during compilation (-I)
# * LIBRARY_DIR_FFLAGS      List of libraries directories required during linking (-L)
# * REQUIRED_MODULEFILES    List of modulefiles required to build an app#

# List of libraries required during linking
list( APPEND LIST_LIBRARIES_FFLAGS "-llapack" )    # Do this correctly: find_Package(LAPACK) and add the lib to LIST_LIBRARIES_FFLAGS
list( REVERSE LIST_LIBRARIES_FFLAGS )
string( REPLACE ";" " " LIBRARIES_FFLAGS "${LIST_LIBRARIES_FFLAGS}" )
LogInfo( "-> LIBRARIES_FFLAGS          ${LIBRARIES_FFLAGS}" )



# List of Fortran definitions used to compiled the code
get_directory_property( DEFINITION_LIST DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
foreach( d ${DEFINITION_LIST} )
  list( APPEND LIST_DEFINITION_FFLAGS "-D${d}" )
endforeach()
#
#
# LogInfo( "-> EXTRA_DEFINITION_LIST          ${EXTRA_DEFINITION_LIST}" )
# foreach( d ${EXTRA_DEFINITION_LIST} )
#   list( APPEND LIST_DEFINITION_FFLAGS "-D${d}" )
# endforeach()


string( REPLACE ";" " " DEFINITION_FFLAGS "${LIST_DEFINITION_FFLAGS}" )
# LogInfo( "-> DEFINITION_FFLAGS          ${DEFINITION_FFLAGS}" )
string( REPLACE "\"" "\\\"" DEFINITION_FFLAGS "${DEFINITION_FFLAGS}" )
LogInfo( "-> DEFINITION_FFLAGS          ${DEFINITION_FFLAGS}" )




# List of include directories required during compilation
list( APPEND LIST_INCLUDE_DIR_FFLAGS "-I${MODULES_INSTALL_DIR}" )
if ( NOT ${INCLUDE_INSTALL_DIR} MATCHES "${MODULES_INSTALL_DIR}" )
  list( APPEND LIST_INCLUDE_DIR_FFLAGS "-I${INCLUDE_INSTALL_DIR}" )
endif()
string(REPLACE ";" " " INCLUDE_DIR_FFLAGS   "${LIST_INCLUDE_DIR_FFLAGS}"  )
LogInfo( "-> INCLUDE_DIR_FFLAGS         ${INCLUDE_DIR_FFLAGS}" )

# List of libraries directories required during linking
list( APPEND LIST_LIBRARY_DIR_FFLAGS "-L${LIBRARY_INSTALL_DIR}" )
string(REPLACE ";" " " LIBRARY_DIR_FFLAGS  "${LIST_LIBRARY_DIR_FFLAGS}"  )
LogInfo( "-> LIBRARY_DIR_FFLAGS         ${LIBRARY_DIR_FFLAGS}" )

# List of modulefiles required to build an app
string(REPLACE ";" " " REQUIRED_MODULEFILES "${LIST_REQUIRED_MODULEFILES}" )
LogInfo( "-> REQUIRED_MODULEFILES       ${REQUIRED_MODULEFILES}" )

# Modulefile name
if ( NOT MODULEFILE_NAME)
  set( MODULEFILE_NAME ${PROJECT_BUILD_CONFIGURATION} CACHE STRING "Name of the modulefile to be installed")
endif()
set( MODULEFILE_FULLNAME    "${PROJECT_NAME_LC}/${MODULEFILE_NAME}" )
LogInfo( "-> MODULEFILE_NAME            ${MODULEFILE_NAME}" )
LogInfo( "-> MODULEFILE_FULLNAME        ${MODULEFILE_FULLNAME}" )


#####################
# Configuring files #
#####################
LogInfo( "-----------------" )
LogInfo( "Configuring files" )
LogInfo( "-----------------" )

set( InputFile  "${PROJECT_SOURCE_DIR}/cmake/modulefile.in" )
set( OutputFile "${CMAKE_BINARY_DIR}/scripts/${PROJECT_FULL_NAME}.modulefile" )
LogInfo( "-> Creating the modulefile: '${InputFile}' -> '${OutputFile}'" )
configure_file( ${InputFile} ${OutputFile} )
string(REPLACE ";" " " REQUIRED_MODULEFILES "${LIST_REQUIRED_MODULEFILES}" )
if ( DEFINED MODULEFILE_INSTALL_DIR )
  install(  FILES         ${OutputFile}
            DESTINATION   ${MODULEFILE_INSTALL_DIR}
            RENAME        ${MODULEFILE_NAME} )
endif()

set( InputFile  "${PROJECT_SOURCE_DIR}/cmake/modulefile-version.in" )
set( OutputFile "${CMAKE_BINARY_DIR}/scripts/version.modulefile" )
LogInfo( "-> Creating the modulefile: '${InputFile}' -> '${OutputFile}'" )
if ( (DEFINED MODULEFILE_INSTALL_DIR) AND (DEFINED DEFAULT_MODULEFILE) )
configure_file( ${InputFile} ${OutputFile} )
  install(  FILES         ${OutputFile}
            DESTINATION   ${MODULEFILE_INSTALL_DIR}
            RENAME        ".version" )
endif()

set( DEF_ENVVAR_SCRIPT_NAME "${PROJECT_NAME_LC}-env-var.sh" )
if ( NOT ENVVAR_SCRIPT_NAME)
  set( ENVVAR_SCRIPT_NAME ${DEF_ENVVAR_SCRIPT_NAME} CACHE STRING "Name of the environment variables script to be installed")
endif()
set( InputFile  "${PROJECT_SOURCE_DIR}/cmake/env-var.in" )
set( OutputFile "${CMAKE_BINARY_DIR}/scripts/${DEF_ENVVAR_SCRIPT_NAME}" )
LogInfo( "-> Creating the env-var script: '${InputFile}' -> '${OutputFile}'" )
configure_file( ${InputFile} ${OutputFile} )
install(  FILES         ${OutputFile}
          DESTINATION   ${SCRIPT_INSTALL_DIR}
          RENAME        ${ENVVAR_SCRIPT_NAME} )

set( InputFile  "${PROJECT_SOURCE_DIR}/scripts/Utilities/build-app.sh" )
set( OutputFile "${CMAKE_BINARY_DIR}/scripts/${PROJECT_NAME_LC}-build-app" )
LogInfo( "-> Creating the build-app script: '${InputFile}' -> '${OutputFile}'" )
configure_file( ${InputFile} ${OutputFile} @ONLY )
install(  FILES         ${OutputFile}
          DESTINATION   ${EXECUTABLE_INSTALL_DIR}
          PERMISSIONS   OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )


####################################
# Installing files and directories #
####################################

LogInfo( "--------------------------------" )
LogInfo( "Installing files and directories" )
LogInfo( "--------------------------------" )

LogInfo( "-> Installing directory with Fortran module files" )
install(  DIRECTORY     ${CMAKE_Fortran_MODULE_DIRECTORY}/
          DESTINATION   ${MODULES_INSTALL_DIR} )

LogInfo( "-> Installing directory with include files" )
install(  DIRECTORY ${INCLUDE_SOURCE_DIR}/
          DESTINATION ${INCLUDE_INSTALL_DIR} )


#################################################################
# Creating and installing the Cmake Package Configuration Files #
#################################################################
# The package configuration and version files are required so that other
# projects can locate the current project by using "find_package".
include(CMakePackageConfigHelpers)
configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/package/config.cmake.in
  ${PROJECT_BINARY_DIR}/cmake/package/config.cmake
  INSTALL_DESTINATION     ${CMAKE_INSTALL_DIR}
  PATH_VARS               LIBRARY_INSTALL_DIR MODULES_INSTALL_DIR INCLUDE_INSTALL_DIR CMAKE_INSTALL_DIR
)
write_basic_package_version_file(
  ${PROJECT_BINARY_DIR}/cmake/package/config-version.cmake
  VERSION                 1.0.0
  COMPATIBILITY           AnyNewerVersion
)
install(
  EXPORT        ${LIBRARY_NAME}-targets
  DESTINATION   ${CMAKE_INSTALL_DIR}
)
install(
  FILES         ${PROJECT_BINARY_DIR}/cmake/package/config.cmake
  DESTINATION   ${CMAKE_INSTALL_DIR}
  RENAME        ${LIBRARY_NAME}-config.cmake
)
install(
  FILES         ${PROJECT_BINARY_DIR}/cmake/package/config-version.cmake
  DESTINATION   ${CMAKE_INSTALL_DIR}
  RENAME        ${LIBRARY_NAME}-config-version.cmake
)

