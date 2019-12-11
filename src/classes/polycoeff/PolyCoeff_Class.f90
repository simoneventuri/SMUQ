! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module PolyCoeff_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    PolyCoeff_Type

type, abstract                                                        ::    PolyCoeff_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_PolyCoeff), deferred, public                   ::    Initialize
  procedure(Reset_PolyCoeff), deferred, public                        ::    Reset
  procedure(SetDefaults_PolyCoeff), deferred, public                  ::    SetDefaults
  procedure(ConstructInput_PolyCoeff), deferred, private              ::    ConstructInput
  procedure(GetInput_PolyCoeff), deferred, public                     ::    GetInput
  procedure(GetValue_PolyCoeff), deferred, public                     ::    GetValue
  procedure(GetCharValue_PolyCoeff), deferred, public                 ::    GetCharValue
  procedure(Copy_PolyCoeff), deferred, public                         ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_PolyCoeff( This )
    import                                                            ::    PolyCoeff_Type
    class(PolyCoeff_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_PolyCoeff( This )
    import                                                            ::    PolyCoeff_Type
    class(PolyCoeff_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_PolyCoeff( This )
    import                                                            ::    PolyCoeff_Type
    class(PolyCoeff_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_PolyCoeff( This, Input, Prefix )
    import                                                            ::    PolyCoeff_Type
    import                                                            ::    InputSection_Type
    class(PolyCoeff_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_PolyCoeff( This, MainSectionName, Prefix, Directory )
    import                                                            ::    PolyCoeff_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_PolyCoeff
    class(PolyCoeff_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue_PolyCoeff( This, Input )
    use Parameters_Library
    import                                                            ::    InputDet_Type
    import                                                            ::    PolyCoeff_Type  
    real(rkp)                                                         ::    GetValue_PolyCoeff
    class(PolyCoeff_Type), intent(in)                                 ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue_PolyCoeff( This, Input )
    use Parameters_Library
    import                                                            ::    InputDet_Type
    import                                                            ::    PolyCoeff_Type
    character(:), allocatable                                         ::    GetCharValue_PolyCoeff
    class(PolyCoeff_Type), intent(in)                                 ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_PolyCoeff( LHS, RHS )
    import                                                            ::    PolyCoeff_Type
    class(PolyCoeff_Type), intent(out)                                ::    LHS
    class(PolyCoeff_Type), intent(in)                                 ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(PolyCoeff_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
