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

module List2D_Class

use Parameters_Library
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    List2D_Type

type                                                                  ::    List2D_Type
  class(*), dimension(:,:), allocatable                               ::    Values
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR42D,                &
                                                                                                          GetR82D,                &
                                                                                                          GetI42D,                &
                                                                                                          GetI82D,                &
                                                                                                          GetC2D,                 &
                                                                                                          GetL2D,                 &
                                                                                                          GetCX2D
  procedure, private                                                  ::    GetR42D
  procedure, private                                                  ::    GetR82D
  procedure, private                                                  ::    GetI42D
  procedure, private                                                  ::    GetI82D
  procedure, private                                                  ::    GetC2D
  procedure, private                                                  ::    GetL2D
  procedure, private                                                  ::    GetCX2D
  generic, public                                                     ::    GetPointer              =>    GetR42DPointer,         &
                                                                                                          GetR82DPointer,         &
                                                                                                          GetI42DPointer,         &
                                                                                                          GetI82DPointer,         &
                                                                                                          GetC2DPointer,          &
                                                                                                          GetL2DPointer,          &
                                                                                                          GetCX2DPointer
  procedure, private                                                  ::    GetR42DPointer
  procedure, private                                                  ::    GetR82DPointer
  procedure, private                                                  ::    GetI42DPointer
  procedure, private                                                  ::    GetI82DPointer
  procedure, private                                                  ::    GetC2DPointer
  procedure, private                                                  ::    GetL2DPointer
  procedure, private                                                  ::    GetCX2DPointer
  procedure, public                                                   ::    Purge
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Set(This, Values)

  class(List2D_Type), intent(inout)                                   ::    This
  class(*), dimension(:,:), intent(in)                                ::    Values

  character(*), parameter                                             ::    ProcName='Set'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Purge()

  allocate(This%Values, source=Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR42D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  real(4), dimension(:,:), allocatable, intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetR2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (real(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR82D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  real(8), dimension(:,:), allocatable, intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetR2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (real(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR42DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  real(4), dimension(:,:), pointer, intent(inout)                     ::    Values

  character(*), parameter                                             ::    ProcName='GetR42DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (real(4))
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR82DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  real(8), dimension(:,:), pointer, intent(inout)                     ::    Values

  character(*), parameter                                             ::    ProcName='GetR82DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (real(8))
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI42D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  integer(4), dimension(:,:), allocatable, intent(inout)              ::    Values

  character(*), parameter                                             ::    ProcName='GetI2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (integer(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI82D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  integer(8), dimension(:,:), allocatable, intent(inout)              ::    Values

  character(*), parameter                                             ::    ProcName='GetI2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (integer(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI42DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  integer(4), dimension(:,:), pointer, intent(inout)                  ::    Values

  character(*), parameter                                             ::    ProcName='GetI42DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (integer(4))
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI82DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  integer(8), dimension(:,:), pointer, intent(inout)                  ::    Values

  character(*), parameter                                             ::    ProcName='GetI82DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (integer(8))
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetC2D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  type(SMUQString_Type), dimension(:,:), allocatable, intent(inout)   ::    Values

  character(*), parameter                                             ::    ProcName='GetC2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (SMUQString_Type)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetC2DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  type(SMUQString_Type), dimension(:,:), pointer, intent(inout)       ::    Values

  character(*), parameter                                             ::    ProcName='GetC2DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (SMUQString_Type)
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetL2D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  logical, dimension(:,:), allocatable, intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetL2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (logical)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetL2DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  logical, dimension(:,:), pointer, intent(inout)                     ::    Values

  character(*), parameter                                             ::    ProcName='GetL2DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (logical)
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCX2D(This, Values)

  class(List2D_Type), intent(in)                                      ::    This
  complex, dimension(:,:), allocatable, intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetCX2D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (complex)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), Size2=size(Value,2), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCX2DPointer(This, Values)

  class(List2D_Type), target, intent(in)                              ::    This
  complex, dimension(:,:), pointer, intent(inout)                     ::    Values

  character(*), parameter                                             ::    ProcName='GetCX2DPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  select type (Value => This%Values)
    type is (complex)
      Values => Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Purge(This)

  class(List2D_Type), intent(inout)                                   ::    This

  character(*), parameter                                             ::    ProcName='Purge'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

  This%Constructed=.false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(List2D_Type), intent(out)                                     ::    LHS
  class(List2D_Type), intent(in)                                      ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  call LHS%Purge()

  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Values', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine FinalizerList(This)

  type(List2D_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
