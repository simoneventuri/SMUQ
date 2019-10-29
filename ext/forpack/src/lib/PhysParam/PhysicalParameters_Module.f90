Module PhysicalParameters_Module

  use KindParameters_Module
  use NumericalParameters_Module

  implicit none

!   Fundamental constants
!   =====================

  real(rkp) ,parameter  ::  me                =     9.10938291e-31_rkp              !< Electron mass [kg]
  real(rkp) ,parameter  ::  qe                =     1.602176565e-19_rkp             !< Elementary charge [C=J/eV]
  real(rkp) ,parameter  ::  kB                =     1.3806488e-23_rkp               !< Boltzmann constant [J/K]
  real(rkp) ,parameter  ::  hP                =     6.62606957e-34_rkp              !< Planck constant [J.s] = [kg.m2.s-1]
  real(rkp) ,parameter  ::  c0                =     2.99792458e+08_rkp              !< Speed of light in vacuum [m/s]
  real(rkp) ,parameter  ::  eps0              =     8.854187817e-12_rkp             !< Electric constant [F/m]
  real(rkp) ,parameter  ::  NA                =     6.02214129e+23_rkp              !< Avogadro constant [ptc/mol]
  real(rkp) ,parameter  ::  Rugc              =     kB * NA                         !< Universal gas constant: 8.3145 [J/mol/K]
  real(rkp) ,parameter  ::  hc                =     hP * c0                         !< Product of Planck cst by speed of light [J.m]
  real(rkp) ,parameter  ::  a0                =     5.291772109217E-11_rkp          !< Bohr radius [m]
  real(rkp) ,parameter  ::  SigSB             =     5.670373e-08_rkp                !< Stefan-Boltzmann constant [W.m-2.K-4]
  real(rkp) ,parameter  ::  c1                =     TwoPi * hP * c0**2              !< First radiation constant [W.m2] (Value =  3.74177152466413e-16)
  real(rkp) ,parameter  ::  c2                =     hP * c0 / kB                    !< Second radiation constant [m.K] 1.43877695998382E-02
  real(rkp) ,parameter  ::  mu0               =     12.566370614e-7_rkp             !< Magnetic constant [N.A-2]
  real(rkp) ,parameter  ::  Faraday           =     9.64853365e+05_rkp              !< Faraday constant [C/mol]
  real(rkp) ,parameter  ::  P0                =     101325.0_rkp                    !< Atmospheric pressure [Pa]

!   Product of fundamental constants
!   ================================

  real(rkp) ,parameter  ::  TwoPikB           =     TwoPi * kB
  real(rkp) ,parameter  ::  TwoPi_h2          =     TwoPi / hP**2
  real(rkp) ,parameter  ::  TwoPikB_h2        =     TwoPi * kB / hP**2              !< This is the factor which appear in the translational partition function when using the species mass:       Qt = ( TwoPikB_h2      * This%Mass      * T )**1.5_rkp
  real(rkp) ,parameter  ::  TwoPiRugc_h2NA2   =     TwoPi * Rugc / (hP**2 * NA**2)  !< [ Mole/J-s^2 ] This is the factor which appear in the translational partition function when using the species molar mass: Qt = ( TwoPiRugc_h2NA2 * This%MolarMass * T )**1.5_rkp
  real(rkp) ,parameter  ::  P0_o_Kb           =     P0 / kB

!   Air constants
!   =============

  real(rkp) ,parameter  ::  gamma_Air         =     1.4_rkp                         !< Air ratio of specific heats [-]
  real(rkp) ,parameter  ::  Rg_Air            =     287.053_rkp                     !< Air constant [J/K/kg]

  Type                  ::  PhysicalParameters_Type
    real(rkp)           ::  me                =     me                              !< Electron mass [kg]
    real(rkp)           ::  qe                =     qe                              !< Elementary charge [C=J/eV]
    real(rkp)           ::  kB                =     kB                              !< Boltzmann constant [J/K]
    real(rkp)           ::  hP                =     hP                              !< Planck constant [J.s] = [kg.m2.s-1]
    real(rkp)           ::  c0                =     c0                              !< Speed of light in vacuum [m/s]
    real(rkp)           ::  eps0              =     eps0                            !< Electric constant [F/m]
    real(rkp)           ::  NA                =     NA                              !< Avogadro constant [ptc/mol]
    real(rkp)           ::  Rugc              =     Rugc                            !< Universal gas constant [J/K/mol]
    real(rkp)           ::  hc                =     hc                              !< Product of Planck cst by speed of light [J.m]
    real(rkp)           ::  a0                =     a0                              !< Bohr radius [m]
    real(rkp)           ::  P0                =     P0                              !< Atmospheric pressure [Pa]
    real(rkp)           ::  SigSB             =     SigSB                           !< Stefan-Boltzmann constant [W.m-2.K-4]
    real(rkp)           ::  c1                =     c1                              !< First radiation constant [W.m2] (Value =  3.74177152466413e-16)
    real(rkp)           ::  c2                =     c2                              !< Second radiation constant [m.K] 1.43877695998382E-02
    real(rkp)           ::  mu0               =     mu0                             !< Magnetic constant [N.A-2]
    real(rkp)           ::  Faraday           =     Faraday                         !< Faraday constant [C/mol]
    real(rkp)           ::  TwoPikB           =     TwoPikB
    real(rkp)           ::  TwoPikB_h2        =     TwoPikB_h2
    real(rkp)           ::  TwoPi_h2          =     TwoPi_h2
    real(rkp)           ::  gamma_Air         =     gamma_Air                       !< Air ratio of specific heats [-]
    real(rkp)           ::  Rg_Air            =     Rg_Air                          !< Air constant [J/K/kg]
  End Type

End Module
