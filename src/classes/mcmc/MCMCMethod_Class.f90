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

module MCMCMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use DistProb_Class                                                ,only:    DistProb_Type
use MCMCSamplingTarget_Module

implicit none

private

public                                                                ::    MCMCMethod_Type
public                                                                ::    MCMCSamplingTarget

type, abstract                                                        ::    MCMCMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsConstructed
  procedure, public                                                   ::    IsInitialized
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_MCMCMethod), deferred, public                  ::    Initialize
  procedure(Reset_MCMCMethod), deferred, public                       ::    Reset
  procedure(SetDefaults_MCMCMethod), deferred, public                 ::    SetDefaults
  procedure(ConstructInput_MCMCMethod), deferred, private             ::    ConstructInput
  procedure(GetInput_MCMCMethod), deferred, public                    ::    GetInput
  procedure(GenerateChain_MCMCMethod), deferred, public               ::    GenerateChain
  procedure(Copy_MCMCMethod), deferred, public                        ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_MCMCMethod( This, Debug )
    import                                                            ::    MCMCMethod_Type
    class(MCMCMethod_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_MCMCMethod( This, Debug )
    import                                                            ::    MCMCMethod_Type
    class(MCMCMethod_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_MCMCMethod( This, Debug )
    import                                                            ::    MCMCMethod_Type
    class(MCMCMethod_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_MCMCMethod( This, Input, SectionChain, Prefix, Debug )
    import                                                            ::    MCMCMethod_Type
    import                                                            ::    InputSection_Type
    class(MCMCMethod_Type), intent(inout)                             ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_MCMCMethod( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    MCMCMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_MCMCMethod
    class(MCMCMethod_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateChain_MCMCMethod( This, SamplingTarget, SpaceInput, ParameterChain, TargetChain, MiscChain, OutputDirectory, &
                                                                                                                           Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    MCMCMethod_Type
    import                                                            ::    SpaceInput_Type
    import                                                            ::    MCMCSamplingTarget
    class(MCMCMethod_Type), intent(inout)                             ::    This
    procedure(MCMCSamplingTarget), pointer                            ::    SamplingTarget
    class(SpaceInput_Type), intent(in)                                ::    SpaceInput
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    ParameterChain
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    MiscChain
    real(rkp), allocatable, dimension(:), optional, intent(out)       ::    TargetChain
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_MCMCMethod( LHS, RHS )
    import                                                            ::    MCMCMethod_Type
    class(MCMCMethod_Type), intent(out)                               ::    LHS
    class(MCMCMethod_Type), intent(in)                                ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(MCMCMethod_Type), intent(inout)                             ::    This
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
  function IsInitialized( This, Debug )

    logical                                                           ::    IsInitialized
    class(MCMCMethod_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsInitialized'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    if ( This%Initialized ) then
      IsInitialized = .true.
    else
      IsInitialized = .false.
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This, Debug )

    logical                                                           ::    IsConstructed
    class(MCMCMethod_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsConstructed'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    if ( This%Constructed ) then
      IsConstructed = .true.
    else
      IsConstructed = .false.
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
