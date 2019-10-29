add_subdirectory( Tools )
add_subdirectory( Font  )
add_subdirectory( Arrow )
add_subdirectory( File  )
add_subdirectory( Graph )
add_subdirectory( Multiplot )

add_subdirectory( DataBlocks )

add_sources (
  GPF_Figure_Class.F90
    GPF_Figure_SubClass.F90
  GPF_Parameters_Class.F90    GPF_Parameters_SubClass.F90
  gpf_movie_class.f90
  gpf_module.f90
)
