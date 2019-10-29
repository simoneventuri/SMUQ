#! /bin/sh
set -e

# rm -Rf build-simple

mkdir -p build-simple

cd build-simple

cmake ../forpack -DCMAKE_INSTALL_PREFIX=../forpack-install \

make

make install