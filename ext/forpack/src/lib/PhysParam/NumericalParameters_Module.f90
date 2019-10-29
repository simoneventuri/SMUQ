Module NumericalParameters_Module

  use KindParameters_Module

  implicit none

! Simple numbers
  real(rkp) ,parameter  ::  Zero              =     0.0_rkp
  real(rkp) ,parameter  ::  One               =     1.0_rkp
  real(rkp) ,parameter  ::  Two               =     2.0_rkp
  real(rkp) ,parameter  ::  Three             =     3.0_rkp
  real(rkp) ,parameter  ::  Four              =     4.0_rkp
  real(rkp) ,parameter  ::  Five              =     5.0_rkp
  real(rkp) ,parameter  ::  Six               =     6.0_rkp
  real(rkp) ,parameter  ::  Seven             =     7.0_rkp
  real(rkp) ,parameter  ::  Eight             =     8.0_rkp
  real(rkp) ,parameter  ::  Nine              =     9.0_rkp
  real(rkp) ,parameter  ::  Ten               =     10.0_rkp
  real(rkp) ,parameter  ::  Twenty            =     20.0_rkp
  real(rkp) ,parameter  ::  Thirty            =     30.0_rkp
  real(rkp) ,parameter  ::  Forty             =     40.0_rkp
  real(rkp) ,parameter  ::  Fifty             =     50.0_rkp
  real(rkp) ,parameter  ::  Sixty             =     60.0_rkp
  real(rkp) ,parameter  ::  Seventy           =     70.0_rkp
  real(rkp) ,parameter  ::  Eighty            =     80.0_rkp
  real(rkp) ,parameter  ::  Ninety            =     90.0_rkp
  real(rkp) ,parameter  ::  Hundred           =     100.0_rkp
  real(rkp) ,parameter  ::  Thousand          =     1000.0_rkp

! Fractions
  real(rkp) ,parameter  ::  f_1o2             =     0.5_rkp
  real(rkp) ,parameter  ::  f_1o3             =     1.0_rkp / 3.0_rkp
  real(rkp) ,parameter  ::  f_1o4             =     0.25_rkp
  real(rkp) ,parameter  ::  f_1o5             =     0.2_rkp
  real(rkp) ,parameter  ::  f_1o6             =     1.0_rkp / 6.0_rkp
  real(rkp) ,parameter  ::  f_1o8             =     0.125_rkp
  real(rkp) ,parameter  ::  f_1o12            =     1.0_rkp / 12.0_rkp
  real(rkp) ,parameter  ::  f_1o20            =     0.05_rkp
  real(rkp) ,parameter  ::  f_2o3             =     2.0_rkp / 3.0_rkp
  real(rkp) ,parameter  ::  f_3o2             =     1.5_rkp
  real(rkp) ,parameter  ::  f_3o4             =     3.0_rkp / 4.0_rkp
  real(rkp) ,parameter  ::  f_4o3             =     4.0_rkp / 3.0_rkp
  real(rkp) ,parameter  ::  f_4o5             =     0.8_rkp
  real(rkp) ,parameter  ::  f_5o2             =     2.5_rkp
  real(rkp) ,parameter  ::  f_5o3             =     5.0_rkp / 3.0_rkp
  real(rkp) ,parameter  ::  f_8o3             =     8.0_rkp / 3.0_rkp
  real(rkp) ,parameter  ::  f_7o5             =     1.4_rkp
  real(rkp) ,parameter  ::  f_15o4            =     3.75_rkp
  real(rkp) ,parameter  ::  f_16o5            =     3.2_rkp

! Other mathematica numbers related to pi, log, ...
  real(rkp) ,parameter  ::  Pi                =     acos(-One)
  real(rkp) ,parameter  ::  invPi             =     One / Pi
  real(rkp) ,parameter  ::  TwoPi             =     Two * Pi
  real(rkp) ,parameter  ::  TwoOverPi         =     Two / Pi
  real(rkp) ,parameter  ::  PiOverTwo         =     Pi / Two
  real(rkp) ,parameter  ::  Pi2               =     Pi * Pi
  real(rkp) ,parameter  ::  Pi3               =     Pi * Pi * Pi
  real(rkp) ,parameter  ::  Degree_To_Radian  =     Pi / 180.0_rkp
  real(rkp) ,parameter  ::  Radian_To_Degree  =     180.0_rkp / Pi
  real(rkp) ,parameter  ::  log2              =     log(Two)


  Type                  ::  NumericalParameter_Type
    real(rkp)           ::  Zero              =     Zero
    real(rkp)           ::  One               =     One
    real(rkp)           ::  Two               =     Two
    real(rkp)           ::  Three             =     Three
    real(rkp)           ::  Four              =     Four
    real(rkp)           ::  Five              =     Five
    real(rkp)           ::  Six               =     Six
    real(rkp)           ::  Seven             =     Seven
    real(rkp)           ::  Eight             =     Eight
    real(rkp)           ::  Nine              =     Nine
    real(rkp)           ::  Ten               =     Ten
    real(rkp)           ::  Twenty            =     Twenty
    real(rkp)           ::  Thirty            =     Thirty
    real(rkp)           ::  Forty             =     Forty
    real(rkp)           ::  Fifty             =     Fifty
    real(rkp)           ::  Sixty             =     Sixty
    real(rkp)           ::  Seventy           =     Seventy
    real(rkp)           ::  Eighty            =     Eighty
    real(rkp)           ::  Ninety            =     Ninety
    real(rkp)           ::  Hundred           =     Hundred
    real(rkp)           ::  Thousand          =     Thousand
    real(rkp)           ::  f_1o2             =     f_1o2
    real(rkp)           ::  f_1o3             =     f_1o3
    real(rkp)           ::  f_1o4             =     f_1o4
    real(rkp)           ::  f_1o5             =     f_1o5
    real(rkp)           ::  f_1o6             =     f_1o6
    real(rkp)           ::  f_1o8             =     f_1o8
    real(rkp)           ::  f_1o12            =     f_1o12
    real(rkp)           ::  f_1o20            =     f_1o20
    real(rkp)           ::  f_2o3             =     f_2o3
    real(rkp)           ::  f_3o2             =     f_3o2
    real(rkp)           ::  f_3o4             =     f_3o4
    real(rkp)           ::  f_4o3             =     f_4o3
    real(rkp)           ::  f_4o5             =     f_4o5
    real(rkp)           ::  f_5o2             =     f_5o2
    real(rkp)           ::  f_5o3             =     f_5o3
    real(rkp)           ::  f_8o3             =     f_8o3
    real(rkp)           ::  f_7o5             =     f_7o5
    real(rkp)           ::  f_15o4            =     f_15o4
    real(rkp)           ::  f_16o5            =     f_16o5
    real(rkp)           ::  Pi                =     Pi
    real(rkp)           ::  TwoPi             =     TwoPi
    real(rkp)           ::  Pi2               =     Pi2
    real(rkp)           ::  Degree_To_Radian  =     Degree_To_Radian
    real(rkp)           ::  Radian_To_Degree  =     Radian_To_Degree
    real(rkp)           ::  log2              =     log2
  End Type

End Module