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

module CovFunction_Class

use Input_Library
use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    CovFunction_Type

type, abstract                                                        ::    CovFunction_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Evaluate                =>    Evaluate_1D
  procedure(Initialize_CovFunction), deferred, public                 ::    Initialize
  procedure(Reset_CovFunction), deferred, public                      ::    Reset
  procedure(SetDefaults_CovFunction), deferred, public                ::    SetDefaults
  procedure(ConstructInput_CovFunction), deferred, private            ::    ConstructInput
  procedure(GetInput_CovFunction), deferred, public                   ::    GetInput
  procedure(Evaluate_1D_CovFunction), deferred, public                ::    Evaluate_1D
  procedure(Copy_CovFunction), deferred, public                       ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_CovFunction( This )
    import                                                            ::    CovFunction_Type
    class(CovFunction_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_CovFunction( This )
    import                                                            ::    CovFunction_Type
    class(CovFunction_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_CovFunction( This )
    import                                                            ::    CovFunction_Type
    class(CovFunction_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_CovFunction( This, Input, Prefix )
    import                                                            ::    CovFunction_Type
    import                                                            ::    InputSection_Type
    class(CovFunction_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_CovFunction( This, MainSectionName, Prefix, Directory )
    import                                                            ::    CovFunction_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_CovFunction
    class(CovFunction_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Evaluate_1D_CovFunction( This, Coordinates, CoordinateLabels, Covariance )
    use                                                               ::    Parameters_Library
    import                                                            ::    CovFunction_Type
    import                                                            ::    String_Type
    class(CovFunction_Type), intent(in)                               ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Coordinates
    type(String_Type), dimension(:), intent(in)                       ::    CoordinateLabels
    real(rkp), dimension(:,:), intent(out)                            ::    Covariance
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_CovFunction( LHS, RHS )
    import                                                            ::    CovFunction_Type
    class(CovFunction_Type), intent(out)                              ::    LHS
    class(CovFunction_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(CovFunction_Type), intent(inout)                            ::    This
    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This )

    logical                                                           ::    IsConstructed
    class(CovFunction_Type), intent(inout)                            ::    This
    character(*), parameter                                           ::    ProcName='IsConstructed'

    IsConstructed = This%Constructed

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
