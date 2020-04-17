add_target_sources (smuqApp
  Model_Class.f90
  ModelInterface_Class.f90
  ModelTransform_Class.f90
  Model_Factory_Class.f90
  ModelInternal_Class.f90
  ModelExternal_Class.f90
)

add_subdirectory(null)
add_subdirectory(pcesm)
add_subdirectory(smd)
add_subdirectory(tfun)
