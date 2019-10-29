add_subdirectory( Parameter )
add_subdirectory( Section )
# add_sources (
#   InputReader_Class.F90
# #   InputReader_SubClass.F90
#   Input_Library.F90
# )


add_sources (
  InputReader_Class.F90
)
if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
  add_sources (
    InputReader_SubClass.F90
  )
endif ()
add_sources (
  Input_Library.F90
)
