#! /bin/bash
set -e

module purge
module load forpack/1.2.6-release-intel-18.0.3

# First, let's create a 'CMakeLists.txt' files
cat <<EOF > makefile
FORTRAN_COMPILER=\${FORPACK_FORTRAN_COMPILER_NAME}
INCLUDE_DIR_FLAGS=\${FORPACK_INCLUDE_DIR_FFLAGS}
LIBRARY_DIR_FLAGS=\$(FORPACK_LIBRARY_DIR_FFLAGS)
LIBRARIES_FLAGS=\$(FORPACK_LIBRARIES_FFLAGS)
FFLAG=
EXECUTABLE="FitterTest-makefile.x"

SOURCE_FILES = sources/Test_Utilities_Module.f90 sources/FitModelTest_Module.f90 sources/ConstantFitModelTest_SubModule.f90 sources/LinearFitModelTest_SubModule.f90 sources/QuadraticFitModelTest_SubModule.f90 sources/ArrheniusFitModelTest_SubModule.f90 sources/NASA9FitModelTest_SubModule.f90 sources/Main.f90
OBJECT_FILES = \$(SOURCE_FILES:.f90=.o)

all: \$(EXECUTABLE)

\$(EXECUTABLE): \$(OBJECT_FILES)
	\$(FORTRAN_COMPILER) \${LIBRARY_DIR_FLAGS} \$(OBJECT_FILES) -o \$@ \$(LIBRARIES_FLAGS)

%.o: %.f90
	\$(FORTRAN_COMPILER) \$(FFLAG) \${INCLUDE_DIR_FLAGS} -c \$< -o \$*.o

clean:
	@rm -f sources/*~ sources/*.o sources/*.mod sources/*.smod
	@rm -f *~ *.o *.mod *.smod
EOF

# Now, lets build the code
make
make clean
rm -Rf makefile

