#! /bin/bash
set -e

module purge
module load forpack

# # To see all avaialable options use:
# forpack-build-app -h

forpack-build-app Main.F90
./Main.x
