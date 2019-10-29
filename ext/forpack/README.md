# ForPack

Fortran Package

## Getting Started

This section describe how to get started with the ForPack library.


### Prerequisites

What things you need to install the software and how to install them

* [cmake](https://cmake.org/) - The CMake open-source, cross-platform family of tools for building, testing and packing softwares.
* [ifort](https://software.intel.com/en-us/intel-compilers) - The intel Fortran compiler.

The optional dependencies are

* [pfunit](http://pfunit.sourceforge.net/) - A Fortran unit testing framework.
* [git](https://git-scm.com/) - A distributed version control system.

### Installing

The build process uses cmake.
Only out-of-source builds are allowed.

In order to compile/install the ForPack, the following step are required:

+ Create an out-of-source build directory
+ Move into the build directory and configure using 'cmake'
+ Compile the code using 'make'
+ Install the code using 'make install'

Type

```bash
mkdir -p build
cd build
cmake ../forpack
make all
make install
```

In the following, the bash variables

For commodity, two bash scripts have been created to easily build/compile/install ```ForPack```.

These scripts are located in ```${FORPACK_SOURCE_DIR}/scripts```, where ```FORPACK_SOURCE_DIR``` denote the path of the ```ForPack``` source files.\

The first build script is ```SimpleBuild.sh``` and is a very simple build script.
```bash
cd ${FORPACK_SOURCE_DIR}/script/SimpleBuild.sh ~
cd ~
vim SimpleBuild.sh # Edit the build script
./SimpleBuild.sh
```


The build processes also create a curstom modulefile at the root of the build directory.
The default name for this modulefile is:
```bash
spark-<version>-<build_type>-<fortran_compiler_name>-<fortran_compiler_version>.modulefile
```
where

+ `<version>` is the version of Spark
+ `<build_type>` is the buidl type: `debug` or `release`
+ `<fortran_compiler_name>` is the name of the Fortran compiler: `intel`
+ `<fortran_compiler_version>` is the version of the Fortran compiler

The modulefile can be copied to the modulefile path to load the spark module.
The copy can be done automatically if the path of the directory where to copy the modulefile is provided when calling the cmake command using the option:
```bash
-DMODULEFILE_INSTALL_DIR=<path>
```
Also, the name of the modulefile can be modified using the following cmake option:
```bash
-DMODULEFILE_NAME=<modulefile-name>
```

## Running the tests

Explain how to run the automated tests for this system

## Using modulefiles

```bash
module list
module avail
module load <name-modulefile>
module unload <name-modulefile>
module switch <name-modulefile-1> <name-modulefile-2>
module help <name-modulefile>
```
A typeical output of the `module help` command would be

```bash

```

## Current status

[![Build Status](http://128.174.133.164:8080/buildStatus/icon?job=ForPack)](http://128.174.133.164:8080/job/ForPack/)

## Authors

* **Bruno Lopez** - *lopezbruno@hotmail.com*
