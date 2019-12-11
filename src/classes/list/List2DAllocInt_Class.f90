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

module List2DAllocInt_Class

use Parameters_Library
use ArrayRoutines_Module
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    List2DAllocInt_Type

type                                                                  ::    List2DAllocInt_Type
  integer, dimension(:,:), allocatable                                ::    Values
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR2D
  procedure, private                                                  ::    GetR2D
  procedure, public                                                   ::    Purge
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Set( This, Values )

    class(List2DAllocInt_Type), intent(inout)                         ::    This
    integer, dimension(:,:), intent(in)                               ::    Values

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Purge()

    allocate( This%Values, source=Values, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetR2D( This, Values )

    class(List2DAllocInt_Type), intent(in)                            ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Values

    character(*), parameter                                           ::    ProcName='GetR2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    allocate( Values, source=This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Purge( This )

    class(List2DAllocInt_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Purge'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    This%Constructed=.false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(List2DAllocInt_Type), intent(out)                           ::    LHS
    class(List2DAllocInt_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Purge()

    if ( RHS%Constructed ) then
      allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Values', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine FinalizerList( This )

    type(List2DAllocInt_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
