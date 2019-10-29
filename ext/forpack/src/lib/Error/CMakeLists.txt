add_sources (
  Error_Class.F90
)
if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
  add_sources (
    Error_SubClass.F90
  )
endif ()
