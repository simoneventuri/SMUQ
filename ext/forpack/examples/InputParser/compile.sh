#! /bin/bash
set -e
# module load forpack/1.2.6-release-intel-18.0.0
# # module load forpack/1.2.6-debug-intel-18.0.0
# ExecutableName="$(basename $(pwd))"
# ${FORPACK_FORTRAN_COMPILER_NAME} -I${FORPACK_MODULES_DIR} -c Main.F90
# ${FORPACK_FORTRAN_COMPILER_NAME} -L${FORPACK_LIBRARY_DIR} Main.o -o ${ExecutableName} ${FORPACK_LIBRARIES}
# rm -f *.o *.mod

module purge
module load forpack

# # To see all avaialable options use:
# forpack-build-app -h

forpack-build-app Main.F90
./Main.x
