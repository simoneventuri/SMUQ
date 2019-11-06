######################################################
# Assumes Makefile generator #
######################################################
cmake_minimum_required ( VERSION 3.15.4 )
project( Brent 
  LANGUAGES Fortran
  DESCRIPTION "SOBOL Library Module Wrapper"
)

######################################################
# Adding custom modules directory #
######################################################
list( APPEND CMAKE_MODULE_PATH "${Brent_SOURCE_DIR}/cmake/modules" )

######################################################
# Adding required modules #
######################################################
include( GNUInstallDirs )
include( AddTargetSources )

######################################################
# Ensuring out of source build #
######################################################
if ( $<STREQUAL:${Brent_SOURCE_DIR},${Brent_BINARY_DIR}> )
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
if ( NOT $<CONFIG> )
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "" FORCE)
elseif ( NOT $<CONFIG> IN_LIST AllowableBuildTypes )
  message( FATAL_ERROR
    "    ERROR:
    Build type must be one of the following: ${AllowableBuildTypes}"
  )
endif()

######################################################
# Adding main target and alias #
######################################################
set( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_INCLUDEDIR}")
add_library( brent STATIC src/Brent_Library.f90 )
add_library( brent::brent ALIAS brent )

######################################################
# Setting compilation flags of the brent target #
######################################################
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

  target_compile_options( brent
    PRIVATE
    "-cpp"
    "-std=gnu"
    "-fno-unsafe-math-optimizations"
    "-ffree-line-length-none"
    "-ffixed-line-length-none"
    "-frealloc-lhs"
    "-frecursive"
  )

  if ( $<CONFIG:Debug> )
    target_compile_options( brent
      PRIVATE
      "-O0" 
      "-g" 
      "-fbounds-check" 
      "-fbacktrace" 
      "-fdump-core" 
      "-ggdb" 
      "-pg" 
      "-Wall" 
      "-fcheck-array-temporaries" 
      "-fcheck=all" 
      "-Wno-unused-dummy-argument" 
      "-Wno-unused-function" 
      "-Wno-uninitialized" 
      "-Wno-maybe-uninitialized" 
      "-Wno-unused-variable" 
      "-Wno-tabs" 
    )
  elseif ( $<CONFIG:Release> )
    target_compile_options( brent
      PRIVATE
      "-O3"
    )
  endif()

else()
  message( FATAL_ERROR 
    "Non-GNU compilers not yet supported by the CMakeLists file."
  )
endif()

######################################################
# Installing #
######################################################
if ( "${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  set( CMAKE_INSTALL_PREFIX "${Brent_SOURCE_DIR}" )
endif()

install ( TARGETS brent
          EXPORT brentExport
          ARCHIVE
)

install ( DIRECTORY "${Brent_BINARY_DIR}/${CMAKE_Fortran_MODULE_DIRECTORY}/"
          DESTINATION "$<TARGET_PROPERTY:brent,Fortran_MODULE_DIRECTORY>"
)

install ( EXPORT brentExport
          DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/brent"
          NAMESPACE brent::
          FILE brentConfig.cmake
)
