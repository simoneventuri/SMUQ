######################################################
# Assumes Makefile generator #
######################################################
cmake_minimum_required (VERSION 3.15)
project(SMUQ 
  VERSION 0.4
  LANGUAGES Fortran C CXX
  DESCRIPTION "Stochastic Modeling and Uncertainty Quantification (SMUQ) toolbox"
)

######################################################
# Adding custom modules directory #
######################################################
list( APPEND CMAKE_MODULE_PATH "${SMUQ_SOURCE_DIR}/cmake/modules" )

######################################################
# Adding required modules #
######################################################
include( GNUInstallDirs )
include( AddTargetSources )
include( ExternalProject )

######################################################
# Ensuring out of source build #
######################################################
if ( $<STREQUAL:${SMUQ_SOURCE_DIR},${SMUQ_BINARY_DIR}> )
  message( FATAL_ERROR
    "    ERROR:
    In source builds of this project are not allowed.
    A separate build directory is required.
    Please create one and run cmake from the build directory.
    Note that cmake has just added files to your source code directory
    and therefore suggest getting a new copy of the source code."
  )
endif()

######################################################
# Build type definitions and initialization #
######################################################
set( AllowableBuildTypes Debug Release )
set_property( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug;Release" )
if ( NOT CMAKE_BUILD_TYPE )
  set(CMAKE_BUILD_TYPE Release CACHE STRING "" FORCE)
elseif ( NOT CMAKE_BUILD_TYPE IN_LIST AllowableBuildTypes )
  message( FATAL_ERROR
    "    ERROR:
    Build type must be one of the following: ${AllowableBuildTypes}"
  )
endif()

######################################################
# Adding main target and alias #
######################################################
add_executable( smuqApp )
add_executable( smuq::smuq ALIAS smuqApp )

######################################################
# Setting compilation flags of the smuqApp target #
######################################################
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  target_compile_options( smuqApp
    PRIVATE
    "-cpp"
    "-std=gnu"
    "-fno-unsafe-math-optimizations"
    "-ffree-line-length-none"
    "-ffixed-line-length-none"
    "-frealloc-lhs"
    "-frecursive"
    $<$<CONFIG:Debug>:-O0>
    $<$<CONFIG:Debug>:-g>
    $<$<CONFIG:Debug>:-fcheck=all>
    $<$<CONFIG:Debug>:-fdump-core>
    $<$<CONFIG:Debug>:-fbacktrace>
    $<$<CONFIG:Debug>:-ggdb>
    $<$<CONFIG:Debug>:-pg>
    $<$<CONFIG:Debug>:-Wall>
    $<$<CONFIG:Debug>:-Wno-unused-dummy-argument>
    $<$<CONFIG:Debug>:-Wno-unused-function>
    $<$<CONFIG:Debug>:-Wno-uninitialized>
    $<$<CONFIG:Debug>:-Wno-maybe-uninitialized> 
    $<$<CONFIG:Debug>:-Wno-unused-variable>
    $<$<CONFIG:Debug>:-Wno-tabs>
    $<$<CONFIG:Debug>:-Warray-temporaries> 
    $<$<CONFIG:Release>:-O3>
  )
else()
  message( FATAL_ERROR 
    "    Non-GNU compilers not yet supported by the CMakeLists file."
  )
endif()

######################################################
# Adding sources to smuqApp #
######################################################
add_subdirectory( src )

######################################################
# Linking required dependencies in ext to smuqApp #
######################################################
add_subdirectory( ext )

######################################################
# Installing #
######################################################
if ( "${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  set( CMAKE_INSTALL_PREFIX "${SMUQ_SOURCE_DIR}" )
endif()

install ( TARGETS smuqApp
          EXPORT smuqExport
          RUNTIME
            DESTINATION ${CMAKE_INSTALL_BINDIR}
        )

install ( EXPORT smuqExport
          DESTINATION ${CMAKE_INSTALL_BINDIR}/cmake/smuq
          NAMESPACE smuq::
          FILE smuqConfig.cmake
        )
