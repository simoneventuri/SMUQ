#! /bin/bash
set -e

module purge
module load forpack/1.2.6-release-intel-18.0.3

Compile() {
  mkdir -p build
  cd build
  cmake ..
  make
  cd ../
}

Run() {
  mkdir -p run
  cd run
  ./../build/FitterTest.x
  gnuplot *.gp
  cd ../
}

Cleanup() {
  rm -Rf build
  rm -Rf run
}

Compile
Run
# Cleanup
