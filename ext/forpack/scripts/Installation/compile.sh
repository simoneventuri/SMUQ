#! /bin/bash
set -e

CodeName="forpack"
ModuleDirectory="~/Modules/${CodeName}"     # Default: Inside the build directory
InstallScript="${CodeName}/scripts/Installation/${CodeName}-install.sh"

./${InstallScript} \
  --build-type        "Release"  \
  --fortran-compiler  "ifort" \
  --modulefile-dir    "${ModuleDirectory}"

# ./${InstallScript} \
#   --build-type        "Debug"  \
#   --fortran-compiler  "ifort" \
#   --modulefile        "intel/18.0.0,pfunit/3.1.1-release-intel-18.0.0" \
#   --modulefile-dir    "${ModuleDirectory}" \
#   --build-tests

# ./${InstallScript} \
#   --build-type        "Release"  \
#   --fortran-compiler  "ifort" \
#   --modulefile        "intel/18.0.0,pfunit/3.1.1-release-intel-18.0.0" \
#   --modulefile-dir    "${ModuleDirectory}" \
#   --build-tests

# ./${InstallScript} \
#   --build-type        "Debug"  \
#   --fortran-compiler  "gfortran" \
#   --modulefile        "gcc/7.3.0,pfunit/3.1.1-release-gnu-7.2.0" \
#   --modulefile-dir    "${ModuleDirectory}" \
#   --build-tests
#
# ./${InstallScript} \
#   --build-type        "Release"  \
#   --fortran-compiler  "gfortran" \
#   --modulefile        "gcc/7.3.0,pfunit/3.1.1-release-gnu-7.2.0" \
#   --modulefile-dir    "${ModuleDirectory}" \
#   --build-tests