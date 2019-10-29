#! /bin/bash
set -e

module purge
module load forpack/1.2.6-release-intel-18.0.3

# First, let's create a 'CMakeLists.txt' files
cat <<EOF > CMakeLists.txt
cmake_minimum_required (VERSION 3.7)
project( FitterTest Fortran )
find_package( FORPACK       CONFIG REQUIRED )
link_directories ( \${FORPACK_LIBRARY_DIR} )
set( CMAKE_Fortran_COMPILER \${FORPACK_FORTRAN_COMPILER_NAME} )
set ( TARGET_NAME "\${PROJECT_NAME}-cmake.x" )
add_executable ( \${TARGET_NAME}
  sources/Test_Utilities_Module.f90
  sources/FitModelTest_Module.f90
    sources/ConstantFitModelTest_SubModule.f90
    sources/LinearFitModelTest_SubModule.f90
    sources/QuadraticFitModelTest_SubModule.f90
    sources/ArrheniusFitModelTest_SubModule.f90
    sources/NASA9FitModelTest_SubModule.f90
  sources/Main.f90 )
target_include_directories( \${TARGET_NAME} PUBLIC \${FORPACK_INCLUDE_DIR} )
target_link_libraries( \${TARGET_NAME} LINK_PUBLIC \${FORPACK_LIBRARIES} )
target_link_libraries( \${TARGET_NAME} LINK_PUBLIC "lapack" )
install( TARGETS \${TARGET_NAME} DESTINATION "$(pwd)" )
EOF

# Now, lets build the code
mkdir -p build
cd build
cmake ..
make
make install
cd ../
rm -Rf build CMakeLists.txt

