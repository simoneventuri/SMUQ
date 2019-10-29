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

module SpaceInput_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    SpaceInput_Type

type, abstract                                                        ::    SpaceInput_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbDim=0
  logical                                                             ::    Correlated=.false.
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsCorrelated
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    GetDistribution         =>    GetDistribution0D,  &
                                                                                                          GetDistribution1D
  generic, public                                                     ::    GetLabel                 =>   GetLabel0D,             &
                                                                                                          GetLabel1D
  generic, public                                                     ::    GetParamName             =>   GetParamName0D,        &
                                                                                                          GetParamName1D
  procedure(GetDistribution0D_SpaceInput), deferred, private          ::    GetDistribution0D
  procedure(GetDistribution1D_SpaceInput), deferred, private          ::    GetDistribution1D
  procedure(GetParamName0D_SpaceInput), deferred, private             ::    GetParamName0D
  procedure(GetParamName1D_SpaceInput), deferred, private             ::    GetParamName1D
  procedure(GetLabel0D_SpaceInput), deferred, private                 ::    GetLabel0D
  procedure(GetLabel1D_SpaceInput), deferred, private                 ::    GetLabel1D
  procedure(GetDistributionPointer_SpaceInput), deferred, public      ::    GetDistributionPointer
  procedure(GetCorrMat_SpaceInput), deferred, public                  ::    GetCorrMat
  procedure(Initialize_SpaceInput), deferred, public                  ::    Initialize
  procedure(Reset_SpaceInput), deferred, public                       ::    Reset
  procedure(SetDefaults_SpaceInput), deferred, public                 ::    SetDefaults
  procedure(Copy_SpaceInput), deferred, public                        ::    Copy
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SpaceInput( This, Debug )
    import                                                            ::    SpaceInput_Type
    class(SpaceInput_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SpaceInput( This, Debug )
    import                                                            ::    SpaceInput_Type
    class(SpaceInput_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SpaceInput( This, Debug )
    import                                                            ::    SpaceInput_Type
    class(SpaceInput_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution0D_SpaceInput( This, Num, Debug )
    use DistProb_Class                                            ,only:    DistProb_Type
    import                                                            ::    SpaceInput_Type
    class(DistProb_Type), allocatable                                 ::    GetDistribution0D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution1D_SpaceInput( This, Debug )
    use DistProb_Vec_Class                                        ,only:    DistProb_Vec_Type
    import                                                            ::    SpaceInput_Type
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    GetDistribution1D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistributionPointer_SpaceInput( This, Num, Debug )
    use DistProb_Class                                            ,only:    DistProb_Type
    import                                                            ::    SpaceInput_Type
    class(DistProb_Type), pointer                                     ::    GetDistributionPointer_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMat_SpaceInput( This, Debug )
    use Parameters_Library
    import                                                            ::    SpaceInput_Type
    real(rkp), allocatable, dimension(:,:)                            ::    GetCorrMat_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName0D_SpaceInput( This, Num, Debug )
    use String_Library
    import                                                            ::    SpaceInput_Type
    character(:), allocatable                                         ::    GetParamName0D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName1D_SpaceInput( This, Debug )
    use String_Library
    import                                                            ::    SpaceInput_Type
    type(String_Type), allocatable, dimension(:)                      ::    GetParamName1D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D_SpaceInput( This, Num, Debug )
    use String_Library
    import                                                            ::    SpaceInput_Type
    character(:), allocatable                                         ::    GetLabel0D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D_SpaceInput( This, Debug )
    use String_Library
    import                                                            ::    SpaceInput_Type
    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D_SpaceInput
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_SpaceInput( LHS, RHS )
    import                                                            ::    SpaceInput_Type
    class(SpaceInput_Type), intent(out)                               ::    LHS
    class(SpaceInput_Type), intent(in)                                ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDim( This, Debug )

    integer                                                           ::    GetNbDim
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetNbDim'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbDim = This%NbDim

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrelated( This, Debug )

    logical                                                           ::    IsCorrelated
    class(SpaceInput_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsCorrelated'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsCorrelated = This%Correlated

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
