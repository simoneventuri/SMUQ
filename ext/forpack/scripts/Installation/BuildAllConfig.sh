#!/bin/bash
set -e
cd $(dirname "$0")

module load gcc/7.2.0
module load ifort/18.0.0

BuildTypesList="Debug Release"
FortranCompilerList="ifort gfortran"
LogDirectory="build"
BuildScript="forpack-install"
mkdir -p ${LogDirectory}

echo ""
echo "Build parameters"
echo "-> List of build types: BuildTypesList  = ${BuildTypesList}"
echo "-> List of modulefiles: FortranCompilerList = ${FortranCompilerList}"

echo ""
echo "Counting the number of different build configurations"
NBuilds=0
for f in ${FortranCompilerList}; do
  for b in ${BuildTypesList}; do
    NBuilds=$((${NBuilds}+1))
  done
done
echo "-> Number of builds: NBuilds = ${NBuilds}"


echo ""
echo "Building all configurations"
i=0
for FortranCompiler in ${FortranCompilerList}; do
  for BuildType in ${BuildTypesList}; do
    i=$((${i}+1))
    LogFile="${LogDirectory}/build-${BuildType}-${FortranCompiler}.log"
    Command="./${BuildScript} --build-type=$BuildType --fortran-compiler=${FortranCompiler} &> ${LogFile} &"
    echo "-> i = ${i}/${NBuilds}: Command ${Command}"
    eval "${Command}"
  done
done