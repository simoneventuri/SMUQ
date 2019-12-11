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

module DistProbContainer_Class

use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use DistProb_Class                                                ,only:  DistProb_Type
use DistProb_Factory_Class                                        ,only:  DistProb_Factory

implicit none

private

public                                                                ::    DistProbContainer_Type

type                                                                  ::    DistProbContainer_Type
  class(DistProb_Type), pointer                                       ::    DistProb=>null()
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Set( This, Object )

    class(DistProbContainer_Type), intent(inout)                      ::    This
    class(DistProb_Type), intent(in)                                  ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    if ( associated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc)
    
    allocate(This%DistProb, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Get( This )

    class(DistProb_Type), allocatable                                 ::    Get

    class(DistProbContainer_Type), intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    if ( .not. associated(This%DistProb) ) call Error%Raise( Line='Probability distribution never defined', ProcName=ProcName)

    allocate(Get, source=This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPointer( This )

    class(DistProb_Type), pointer                                     ::    GetPointer

    class(DistProbContainer_Type), intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'

    if ( .not. associated(This%DistProb) ) call Error%Raise( Line='Probability distribution never defined', ProcName=ProcName)

    GetPointer => This%DistProb

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(DistProbContainer_Type), intent(out)                        ::    LHS
    type(DistProbContainer_Type), intent(in)                          ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    if ( associated(RHS%DistProb) ) then
      if ( associated(LHS%DistProb) ) deallocate( LHS%DistProb, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%DistProb', Procname=ProcName, stat=StatLoc )
      allocate(LHS%DistProb, source=RHS%DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%DistProb', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(DistProbContainer_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( associated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%DistProb', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
