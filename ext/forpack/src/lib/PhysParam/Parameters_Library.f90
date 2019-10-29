Module Parameters_Library

  use KindParameters_Module
  use NumericalParameters_Module
  use PhysicalParameters_Module
  use UnitConversionParameters_Module

  implicit none

  public

  type(NumericalParameter_Type) ,protected  ::  NumParam
  type(PhysicalParameters_Type) ,protected  ::  PhyParam
!   type(UnitConverter_Type)      ,protected  ::  UnitConv

!   type(PhysicalParameters_Type) ,parameter  ::  PhyParam  =   &
!     PhysicalParameters_Type(            &
!         me          =     me          , &
!         qe          =     qe          , &
!         kB          =     kB          , &
!         hP          =     hP          , &
!         c0          =     c0          , &
!         eps0        =     eps0        , &
!         NA          =     NA          , &
!         Rugc        =     Rugc        , &
!         hc          =     hc          , &
!         a0          =     a0          , &
!         P0          =     P0          , &
!         SigSB       =     SigSB       , &
!         c1          =     c1          , &
!         c2          =     c2          , &
!         mu0         =     mu0         , &
!         Faraday     =     Faraday     , &
!         TwoPikB     =     TwoPikB     , &
!         TwoPikB_h2  =     TwoPikB_h2  , &
!         TwoPi_h2    =     TwoPi_h2    , &
!         gamma_Air   =     gamma_Air   , &
!         Rg_Air      =     Rg_Air        )

End Module
