######################################################
# Assumes Makefile generator #
######################################################
cmake_minimum_required ( VERSION 3.15.4 )
project( QuadPack 
  LANGUAGES Fortran
  DESCRIPTION "QUADPACK Library Module Wrapper"
)

######################################################
# Adding custom modules directory #
######################################################
list( APPEND CMAKE_MODULE_PATH "${QuadPack_SOURCE_DIR}/cmake/modules" )

######################################################
# Adding required modules #
######################################################
include( GNUInstallDirs )
include( AddTargetSources )

######################################################
# Ensuring out of source build #
######################################################
if ( $<STREQUAL:${QuadPack_SOURCE_DIR},${QuadPack_BINARY_DIR}> )
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
add_library( quadpack STATIC src/QuadPack_Library.f90 )
add_library( quadpack::quadpack ALIAS quadpack )

######################################################
# Setting compilation flags of the quadpack target #
######################################################
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

  target_compile_options( quadpack
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
    target_compile_options( quadpack
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
    target_compile_options( quadpack
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
  set( CMAKE_INSTALL_PREFIX "${QuadPack_SOURCE_DIR}" )
endif()

install ( TARGETS quadpack
          EXPORT quadpackExport
          ARCHIVE
)

install ( DIRECTORY "${QuadPack_BINARY_DIR}/${CMAKE_Fortran_MODULE_DIRECTORY}/"
          DESTINATION "$<TARGET_PROPERTY:quadpack,Fortran_MODULE_DIRECTORY>"
)

install ( EXPORT quadpackExport
          DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/quadpack"
          NAMESPACE quadpack::
          FILE quadpackConfig.cmake
)
