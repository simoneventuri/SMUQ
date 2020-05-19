######################################################
# Assumes Makefile generator #
######################################################
cmake_minimum_required ( VERSION 3.15.4 )
project( CDF 
  LANGUAGES Fortran
  DESCRIPTION "CDF Library Module Wrapper"
)

######################################################
# Adding custom modules directory #
######################################################
list( APPEND CMAKE_MODULE_PATH "${CDF_SOURCE_DIR}/cmake/modules" )

######################################################
# Adding required modules #
######################################################
include( GNUInstallDirs )
include( AddTargetSources )

######################################################
# Ensuring out of source build #
######################################################
if ( $<STREQUAL:${CDF_SOURCE_DIR},${CDF_BINARY_DIR}> )
  message( FATAL_ERROR
    "ERROR:
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
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "" FORCE)
elseif ( NOT CMAKE_BUILD_TYPE IN_LIST AllowableBuildTypes )
  message( FATAL_ERROR
    "    ERROR:
    Build type must be one of the following: ${AllowableBuildTypes}"
  )
endif()

######################################################
# Adding main target and alias #
######################################################
set( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_INCLUDEDIR}")
add_library( cdf STATIC src/CDF_Library.f90 )
add_library( cdf::cdf ALIAS cdf )

######################################################
# Setting compilation flags of the cdf target #
######################################################
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

  target_compile_options( cdf
    PRIVATE
    "-cpp"
    "-std=legacy"
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
    $<$<CONFIG:Release>:-O2>
  )

else()
  message( FATAL_ERROR 
    "Non-GNU compilers not yet supported by the CMakeLists file."
  )
endif()

######################################################
# Installing #
######################################################
if ( "${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  set( CMAKE_INSTALL_PREFIX "${CDF_SOURCE_DIR}" )
endif()

install ( TARGETS cdf
          EXPORT cdfExport
          ARCHIVE
)

install ( DIRECTORY "${CDF_BINARY_DIR}/${CMAKE_Fortran_MODULE_DIRECTORY}/"
          DESTINATION "$<TARGET_PROPERTY:cdf,Fortran_MODULE_DIRECTORY>"
)

install ( EXPORT cdfExport
          DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/cdf"
          NAMESPACE cdf::
          FILE cdfConfig.cmake
)
