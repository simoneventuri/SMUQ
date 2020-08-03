# SMUQ: Fortran 2008 + MATLAB Toolbox for Stochastic Modeling and Uncertainty Quantification


## CODE OBJECTIVE, THEORY, APPLICATIONS:
======================================================================================================================================

1. Venturi - Physics-Based Stochastic Framework for the Quantification of Uncertainty in Non-equilibrium Hypersonic Flows - MS Thesis - 2016.pdf (Accessible from SMUQ/doc/)

2. Rostkowski - Uncertainty quantification of vista charring ablator material database using Bayesian inference - MS Thesis - 2017.pdf (Accessible from SMUQ/doc/)

3. T. A. Oliver, G. Terejanu, C. Simmons, S. Christopher, and R. D. Moser, "Validating predictions of unobserved quantities", Computer Methods in Applied Mechanics and Engineering, Vol. 283 (2015) http://dx.doi.org/10.1016/j.cma.2014.08.023

4. P. Rostkowski, S. Venturi†, M. Panesi, A. Omidy, H. Weng, and A. Martin, "Calibration and Uncertainty Quantification of VISTA Ablator Material Database Using Bayesian Inference", Journal of Thermophysics and Heat Transfer, Vol. 33, No. 2 (2019) https://doi.org/10.2514/1.T5396


## COMPILATION AND EXECUTION
======================================================================================================================================

### Requirements:
  Cmake (3.15.5 minimum )
  GCC ( 9.1.0 )

### A) COMPILING SMUQ
——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

1. Crate a build directory inside of the main SMUQ directory ( ex. build )

2. Change current directory to the build directory ( ex. cd ${SMUQDIR}/build )

3. Type in " cmake .. "

4. Build and install with " make install " ( Can use parallel build with make -jn install where n is the number of processors )

### B) TESTING SMUQ:
——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

1. Create a run directory anywhere on your system

2. Copy any one of the example cases in the ${SMUQDIR}/example directory

3. Run the SMUQ executable located in ${SMUQDIR}/bin/smuqApp

4. When the simulation is done, the results will be located by default in the output directory of ${SMUQDIR}
