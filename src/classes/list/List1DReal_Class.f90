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

module List1DAllocReal_Class

use Parameters_Library
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    List1DAllocReal_Type

type                                                                  ::    List1DAllocReal_Type
  real(rkp), dimension(:), allocatable                                ::    Values
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR1D
  procedure, private                                                  ::    GetR1D
  procedure, public                                                   ::    Purge
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Set(This, Values)

  class(List1DAllocReal_Type), intent(inout)                          ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Values

  character(*), parameter                                             ::    ProcName='Set'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Purge()

  allocate(This%Values, source=Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR1D(This, Values)

  class(List1DAllocReal_Type), intent(in)                             ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetR2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  call EnsureArraySize(Array=Values, Size1=size(This%Values,1), DefaultValue=.false.)
  Values = This%Values

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Purge(This)

  class(List1DAllocReal_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Purge'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

  This%Constructed=.false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(List1DAllocReal_Type), intent(out)                            ::    LHS
  class(List1DAllocReal_Type), intent(in)                             ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  call LHS%Purge()

  if (RHS%Constructed) then
    allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Values', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine FinalizerList(This)

  type(List1DAllocReal_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
