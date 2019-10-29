#----------------------------------------------------------------
# Generated CMake target import file for configuration "Debug".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "sobol::sobol" for configuration "Debug"
set_property(TARGET sobol::sobol APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
set_target_properties(sobol::sobol PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_DEBUG "Fortran"
  IMPORTED_LOCATION_DEBUG "${_IMPORT_PREFIX}/lib/libsobol.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS sobol::sobol )
list(APPEND _IMPORT_CHECK_FILES_FOR_sobol::sobol "${_IMPORT_PREFIX}/lib/libsobol.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
