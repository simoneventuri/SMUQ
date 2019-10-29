#! /bin/bash
set -e

module purge
module load forpack/1.2.6-release-intel-18.0.3

# # To see all avaialable options use:
# forpack-build-app -h

cd sources


forpack-build-app  \
  Test_Utilities_Module.f90             \
  FitModelTest_Module.f90               \
    ConstantFitModelTest_SubModule.f90  \
    LinearFitModelTest_SubModule.f90    \
    QuadraticFitModelTest_SubModule.f90 \
    ArrheniusFitModelTest_SubModule.f90 \
    NASA9FitModelTest_SubModule.f90     \
  Main.f90                              \
  -x FitterTest-script.x

mv FitterTest-script.x ..
cd ../
