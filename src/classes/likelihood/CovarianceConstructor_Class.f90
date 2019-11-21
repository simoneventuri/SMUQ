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

module CovarianceConstructor_Class

use Input_Library
use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    CovarianceConstructor_Type

type, abstract                                                        ::    CovarianceConstructor_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_CovarianceConstructor), deferred, public       ::    Initialize
  procedure(Reset_CovarianceConstructor), deferred, public            ::    Reset
  procedure(SetDefaults_CovarianceConstructor), deferred, public      ::    SetDefaults
  procedure(ConstructInput_CovarianceConstructor), deferred, private  ::    ConstructInput
  procedure(GetInput_CovarianceConstructor), deferred, public         ::    GetInput
  procedure(AssembleCov_CovarianceConstructor), deferred, public      ::    AssembleCov
  procedure(IsStochastic_CovarianceConstructor), deferred, public     ::    IsStochastic
  procedure(Copy_CovarianceConstructor), deferred, public             ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_CovarianceConstructor( This, Debug )
    import                                                            ::    CovarianceConstructor_Type
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_CovarianceConstructor( This, Debug )
    import                                                            ::    CovarianceConstructor_Type
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_CovarianceConstructor( This, Debug )
    import                                                            ::    CovarianceConstructor_Type
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_CovarianceConstructor( This, Input, Prefix, Debug )
    import                                                            ::    CovarianceConstructor_Type
    import                                                            ::    InputSection_Type
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_CovarianceConstructor( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    CovarianceConstructor_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_CovarianceConstructor
    class(CovarianceConstructor_Type), intent(in)                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AssembleCov_CovarianceConstructor( This, Coordinates, CoordinateLabels, Input, Cov, Debug)
    use                                                               ::    Parameters_Library
    import                                                            ::    CovarianceConstructor_Type
    import                                                            ::    InputDet_Type
    import                                                            ::    String_Type
    class(CovarianceConstructor_Type), intent(in)                     ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Coordinates
    type(String_Type), dimension(:), intent(in)                       ::    CoordinateLabels
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Cov
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsStochastic_CovarianceConstructor( This, Input, Debug )
    import                                                            ::    CovarianceConstructor_Type
    import                                                            ::    Input_Type
    logical                                                           ::    IsStochastic_CovarianceConstructor
    class(CovarianceConstructor_Type), intent(in)                     ::    This
    class(Input_Type), intent(in)                                     ::    Input
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_CovarianceConstructor( LHS, RHS )
    import                                                            ::    CovarianceConstructor_Type
    class(CovarianceConstructor_Type), intent(out)                    ::    LHS
    class(CovarianceConstructor_Type), intent(in)                     ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This, Debug )

    logical                                                           ::    IsConstructed
    class(CovarianceConstructor_Type), intent(inout)                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsConstructed'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    IsConstructed = This%Constructed

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
