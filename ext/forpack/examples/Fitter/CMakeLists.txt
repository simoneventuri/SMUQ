cmake_minimum_required (VERSION 3.7)
project( FitterTest Fortran )
find_package( FORPACK       CONFIG REQUIRED )
link_directories ( ${FORPACK_LIBRARY_DIR} )
set( CMAKE_Fortran_COMPILER ${FORPACK_FORTRAN_COMPILER_NAME} )
set ( TARGET_NAME "${PROJECT_NAME}.x" )
add_executable ( ${TARGET_NAME}
  Test_Utilities_Module.f90
  FitModelTest_Module.f90
    ConstantFitModelTest_SubModule.f90
    LinearFitModelTest_SubModule.f90
    QuadraticFitModelTest_SubModule.f90
    ArrheniusFitModelTest_SubModule.f90
    NASA9FitModelTest_SubModule.f90
  Main.f90
)
target_include_directories( ${TARGET_NAME} PUBLIC ${FORPACK_INCLUDE_DIR} )
target_link_libraries( ${TARGET_NAME} LINK_PUBLIC ${FORPACK_LIBRARY} )
target_link_libraries( ${TARGET_NAME} LINK_PUBLIC "lapack" )
