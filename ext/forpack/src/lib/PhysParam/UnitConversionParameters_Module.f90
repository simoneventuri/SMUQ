Module UnitConversionParameters_Module

  use KindParameters_Module
  use NumericalParameters_Module
  use PhysicalParameters_Module

  implicit none

! Energy conversions
  real(rkp) ,parameter  ::  Unit_eV_to_K      =   qe / kB                     !< Conversion from [eV]   to [K]        (Value = 11604.5193028089)
  real(rkp) ,parameter  ::  Unit_K_to_eV      =   kB / qe                     !< Conversion from [K]    to [eV]       (Value = 8.61733238496096e-05)
  real(rkp) ,parameter  ::  Unit_eV_to_cm     =   qe / (hP * c0 * Hundred)    !< Conversion from [eV]   to [cm-1]     (Value = 8065.54429599670)
  real(rkp) ,parameter  ::  Unit_cmm1_to_eV   =   hP * c0 * Hundred / qe      !< Conversion from [cm-1] to [eV]       (Value = 1.23984192920042e-04)
  real(rkp) ,parameter  ::  Unit_cmm1_to_K    =   c2 * Hundred                !< Conversion from [cm-1] to [K]        (Value = 1.43877695998382)
  real(rkp) ,parameter  ::  Unit_K_to_cmm1    =   One / Unit_cmm1_to_K        !< Conversion from [cm-1] to [K]        (Value = )
  real(rkp) ,parameter  ::  Unit_eV_to_Jmolm  =   qe * Na                     !< Conversion from [eV]   to [J/mol]    (Value = 96485.3364595687 [J/mol/eV])
  real(rkp) ,parameter  ::  Unit_K_to_Jmolm   =   kB * Na                     !< Conversion from [K]    to [J/mol]    (Value = ? [J/mol/K])

! Pressure conversions
  real(rkp) ,parameter  ::  Unit_atm_to_Pa    =   P0                          !< Conversion from [atm]  to [Pa]       (Value = 101325.0 )
  real(rkp) ,parameter  ::  Unit_Pa_to_atm    =   One / Unit_atm_to_Pa        !< Conversion from [Pa]   to [atm]      (Value =  )
  real(rkp) ,parameter  ::  Unit_Torr_to_Pa   =   P0 / 760.0_rkp              !< Conversion from [Torr] to [Pa]       (Value = 133.322 )
  real(rkp) ,parameter  ::  Unit_Pa_to_Torr   =   One / Unit_Torr_to_Pa       !< Conversion from [Pa]   to [Torr]     (Value = 0.00750062 )

  Type                  ::  UnitConverter_Type
    real(rkp)           ::  eV_to_K           =   Unit_eV_to_K                !< Conversion from [eV]   to [K]        (Value = 11604.5193028089)
    real(rkp)           ::  K_to_eV           =   Unit_K_to_eV                !< Conversion from [K]    to [eV]       (Value = 8.61733238496096e-05)
    real(rkp)           ::  eV_to_cm          =   Unit_eV_to_cm               !< Conversion from [eV]   to [cm-1]     (Value = 8065.54429599670)
    real(rkp)           ::  cmm1_to_eV        =   Unit_cmm1_to_eV             !< Conversion from [cm-1] to [eV]       (Value = 1.23984192920042e-04)
    real(rkp)           ::  cmm1_to_K         =   Unit_cmm1_to_K              !< Conversion from [cm-1] to [K]        (Value = 1.43877695998382)
    real(rkp)           ::  K_to_cmm1         =   Unit_K_to_cmm1              !< Conversion from [cm-1] to [K]        (Value = )
    real(rkp)           ::  eV_to_Jmolm       =   Unit_eV_to_Jmolm            !< Conversion from [eV]   to [J/mol]    (Value = 96485.3364595687 [J/mol/eV])
    real(rkp)           ::  K_to_Jmolm        =   Unit_K_to_Jmolm             !< Conversion from [K]    to [J/mol]    (Value = ? [J/mol/K])
!   Pressure
    real(rkp)           ::  Pa_to_atm         =   Unit_Pa_to_atm              !< Conversion from [Pa]   to [atm]      (Value =  )
    real(rkp)           ::  Pa_to_Torr        =   Unit_Pa_to_Torr             !< Conversion from [Pa]   to [Torr]     (Value = 0.00750062 )
    real(rkp)           ::  atm_to_Pa         =   Unit_atm_to_Pa              !< Conversion from [atm]  to [Pa]       (Value = 101325.0 )
    real(rkp)           ::  Torr_to_Pa        =   Unit_Torr_to_Pa             !< Conversion from [Torr] to [Pa]       (Value = 133.322 )
  End Type

  type(UnitConverter_Type)      ,protected  ::  UnitConv

  contains

Subroutine ConvertPhysicalValue( Value, FromUnits, ToUnits )

!   use Parameters_Library  ,only:  One, UnitConv, Na

  real(rkp)                                             ,intent(inout)  ::  Value
  character(*)                                          ,intent(in)     ::  FromUnits
  character(*)                                          ,intent(in)     ::  ToUnits

  real(rkp)                                                             ::  Factor

  if ( len_trim(FromUnits) == 0 ) return
  if ( FromUnits == ToUnits ) return

  Factor    =   One

  select case (FromUnits)

!   Pressure
!   ========

    case ("Pa")
      select case (ToUnits)
        case ("Pa")
          Factor = One
        case ("atm")
          Factor = UnitConv%Pa_to_atm
        case ("Torr")
          Factor = UnitConv%Pa_to_Torr
      end select

    case ("atm")
      select case (ToUnits)
        case ("atm")
          Factor = One
        case ("Pa")
          Factor = UnitConv%atm_to_Pa
        case ("Torr")
          Factor = UnitConv%atm_to_Pa * UnitConv%Pa_to_Torr
      end select

    case ("Torr")
      select case (ToUnits)
        case ("Torr")
          Factor = One
        case ("Pa")
          Factor = UnitConv%Torr_to_Pa
        case ("atm")
          Factor = UnitConv%Torr_to_Pa * UnitConv%Pa_to_atm
      end select

!   Energy
!   ======

    case ("K")
      select case (ToUnits)
        case ("K")
          Factor = One
        case ("eV")
          Factor = UnitConv%K_to_eV
        case ("cm-1","1/cm","cm^-1","cm^{-1}")
          Factor = UnitConv%K_to_cmm1
        case ("J/mol","J.mol^-1","J.mol^{-1}")
          Factor = UnitConv%K_to_Jmolm
      end select

    case ("eV")
      select case (ToUnits)
        case ("eV")
          Factor = One
        case ("K")
          Factor = UnitConv%eV_to_K               ! Conversion from [eV] to [K]       11604.5193028089 K/eV
        case ("cm-1","1/cm","cm^-1","cm^{-1}")
          Factor = UnitConv%eV_to_cm              ! Conversion from [eV] to [cm-1]    8065.54429599670 cm-1/eV
        case ("J/mol","J.mol^-1","J.mol^{-1}")
          Factor = UnitConv%eV_to_Jmolm           ! Conversion from [eV] to [J/mol]   96485.3364595687 J/mol/eV
      end select

    case ("cm-1","1/cm","cm^-1","cm^{-1}")
      select case (ToUnits)
        case ("eV")
          Factor = UnitConv%cmm1_to_eV
        case ("K")
          Factor = UnitConv%cmm1_to_eV * UnitConv%eV_to_K
        case ("cm-1","1/cm","cm^-1","cm^{-1}")
          Factor = One
        case ("J/mol","J.mol^-1","J.mol^{-1}")
          Factor = UnitConv%cmm1_to_eV * UnitConv%eV_to_Jmolm
      end select

!   Rates
!   =====
    case ("m^3/mol/s")
      select case (ToUnits)
        case ("m^3/mol/s","m3/mol/s","m^3.mol^-1.s^-1")
          Factor = One
        case ("cm^3/s","cm3/s")
          Factor = 1.0E-06 * Na
      end select

  end select

  Value   =   Value * Factor

End Subroutine

End Module
