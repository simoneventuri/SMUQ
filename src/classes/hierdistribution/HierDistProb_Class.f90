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

module HierDistProb_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistProb_Type

type, abstract                                                        ::    HierDistProb_Type
  character(:), allocatable                                           ::    Name
  real(rkp)                                                           ::    A=One
  real(rkp)                                                           ::    B=One
  character(:), allocatable                                           ::    ADependency
  character(:), allocatable                                           ::    BDependency
  logical                                                             ::    TruncatedLeft=.false.
  logical                                                             ::    TruncatedRight=.false.
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  private
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_HierDistProb), deferred, public                ::    Initialize
  procedure(Reset_HierDistProb), deferred, public                     ::    Reset
  procedure(SetDefaults_HierDistProb), deferred, public               ::    SetDefaults
  procedure(ConstructInput_HierDistProb), deferred, private           ::    ConstructInput
  procedure(GetInput_HierDistProb), deferred, public                  ::    GetInput
  procedure(Generate_HierDistProb), deferred, public                  ::    Generate
  procedure(Copy_HierDistProb), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_HierDistProb( This, Debug )
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_HierDistProb( This, Debug )
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_HierDistProb( This, Debug )
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_HierDistProb( This, Input, Prefix, Debug )
    import                                                            ::    HierDistProb_Type
    import                                                            ::    InputSection_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate_HierDistProb( This, Input, Distribution, Debug )
    import                                                            ::    HierDistProb_Type
    import                                                            ::    DistProb_Type
    import                                                            ::    InputDet_Type
    class(HierDistProb_Type), intent(in)                              ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_HierDistProb( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    HierDistProb_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_HierDistProb
    class(HierDistProb_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Directory
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_HierDistProb( LHS, RHS )
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(out)                             ::    LHS
    class(HierDistProb_Type), intent(in)                              ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(HierDistProb_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
