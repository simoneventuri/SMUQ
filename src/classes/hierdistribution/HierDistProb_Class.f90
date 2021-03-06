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
use Input_Class                                                   ,only:    Input_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistProb_Type

type, abstract                                                        ::    HierDistProb_Type
  logical                                                             ::    TruncatedLeft=.false.
  logical                                                             ::    TruncatedRight=.false.
  logical                                                             ::    Constructed=.false.
contains
  private
  procedure, public                                                   ::    IsTruncatedLeft
  procedure, public                                                   ::    IsTruncatedRight
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Reset_HierDistProb), deferred, public                     ::    Reset
  procedure(ConstructInput_HierDistProb), deferred, private           ::    ConstructInput
  procedure(GetInput_HierDistProb), deferred, public                  ::    GetInput
  procedure(Generate_HierDistProb), deferred, public                  ::    Generate
  procedure(Copy_HierDistProb), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_HierDistProb(This)
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_HierDistProb(This, Input, Prefix)
    import                                                            ::    HierDistProb_Type
    import                                                            ::    InputSection_Type
    class(HierDistProb_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate_HierDistProb(This, Input, Distribution)
    import                                                            ::    HierDistProb_Type
    import                                                            ::    DistProb_Type
    import                                                            ::    Input_Type
    class(HierDistProb_Type), intent(in)                              ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_HierDistProb(This, Name, Prefix, Directory)
    import                                                            ::    HierDistProb_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_HierDistProb
    class(HierDistProb_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Directory
    character(*), optional, intent(in)                                ::    Prefix
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_HierDistProb(LHS, RHS)
    import                                                            ::    HierDistProb_Type
    class(HierDistProb_Type), intent(out)                             ::    LHS
    class(HierDistProb_Type), intent(in)                              ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!--------------------------------------------------------------------------------------------------------------------------------
function IsTruncatedLeft(This)

  logical                                                             ::    IsTruncatedLeft

  class(HierDistProb_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='IsTruncatedLeft'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  IsTruncatedLeft = This%TruncatedLeft

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function IsTruncatedRight(This)

  logical                                                             ::    IsTruncatedRight

  class(HierDistProb_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='IsTruncatedRight'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  IsTruncatedRight = This%TruncatedRight

end function
!!--------------------------------------------------------------------------------------------------------------------------------

end module
