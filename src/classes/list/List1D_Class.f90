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

module List1D_Class

use Parameters_Library
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    List1D_Type

type                                                                  ::    List1D_Type
  class(*), dimension(:), allocatable                                 ::    Values
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR41D,                &
                                                                                                          GetR81D,                &
                                                                                                          GetI41D,                &
                                                                                                          GetI81D,                &
                                                                                                          GetC1D,                 &
                                                                                                          GetL1D,                 &
                                                                                                          GetCX1D
  procedure, private                                                  ::    GetR41D
  procedure, private                                                  ::    GetR81D
  procedure, private                                                  ::    GetI41D
  procedure, private                                                  ::    GetI81D
  procedure, private                                                  ::    GetC1D
  procedure, private                                                  ::    GetL1D
  procedure, private                                                  ::    GetCX1D
  generic, public                                                     ::    GetPointer              =>    GetR41DPointer,         &
                                                                                                          GetR81DPointer,         &
                                                                                                          GetI41DPointer,         &
                                                                                                          GetI81DPointer,         &
                                                                                                          GetC1DPointer,          &
                                                                                                          GetL1DPointer,          &
                                                                                                          GetCX1DPointer
  procedure, private                                                  ::    GetR41DPointer
  procedure, private                                                  ::    GetR81DPointer
  procedure, private                                                  ::    GetI41DPointer
  procedure, private                                                  ::    GetI81DPointer
  procedure, private                                                  ::    GetC1DPointer
  procedure, private                                                  ::    GetL1DPointer
  procedure, private                                                  ::    GetCX1DPointer
  procedure, public                                                   ::    Purge
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Set(This, Values)

  class(List1D_Type), intent(inout)                                   ::    This
  class(*), dimension(:), intent(in)                                  ::    Values

  character(*), parameter                                             ::    ProcName='Set'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Purge()

  allocate(This%Values, source=Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR41D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  real(4), dimension(:), allocatable, intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetR41D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (real(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR81D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  real(8), dimension(:), allocatable, intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetR81D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (real(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetR41DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  real(4), dimension(:), pointer, intent(inout)                       ::    Values

  character(*), parameter                                             ::    ProcName='GetR41DPointer'
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
subroutine GetR81DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  real(8), dimension(:), pointer, intent(inout)                       ::    Values

  character(*), parameter                                             ::    ProcName='GetR81DPointer'
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
subroutine GetI41D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  integer(4), dimension(:), allocatable, intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='GetI41D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (integer(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI81D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  integer(8), dimension(:), allocatable, intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='GetI81D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (integer(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetI41DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  integer(4), dimension(:), pointer, intent(inout)                    ::    Values

  character(*), parameter                                             ::    ProcName='GetI41DPointer'
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
subroutine GetI81DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  integer(8), dimension(:), pointer, intent(inout)                    ::    Values

  character(*), parameter                                             ::    ProcName='GetI81DPointer'
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
subroutine GetC1D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  type(SMUQString_Type), dimension(:), allocatable, intent(inout)     ::    Values

  character(*), parameter                                             ::    ProcName='GetC1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (SMUQString_Type)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetC1DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  type(SMUQString_Type), dimension(:), pointer, intent(inout)         ::    Values

  character(*), parameter                                             ::    ProcName='GetC1DPointer'
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
subroutine GetL1D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  logical, dimension(:), allocatable, intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetL1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (logical)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetL1DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  logical, dimension(:), pointer, intent(inout)                       ::    Values

  character(*), parameter                                             ::    ProcName='GetL1DPointer'
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
subroutine GetCX1D(This, Values)

  class(List1D_Type), intent(in)                                      ::    This
  complex, dimension(:), allocatable, intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetCX1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object never constructed', ProcName=ProcName)

  select type (Value => This%Values)
    type is (complex)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested value does not match the requested type")
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCX1DPointer(This, Values)

  class(List1D_Type), target, intent(in)                              ::    This
  complex, dimension(:), pointer, intent(inout)                       ::    Values

  character(*), parameter                                             ::    ProcName='GetCX1DPointer'
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

  class(List1D_Type), intent(inout)                                   ::    This

  character(*), parameter                                             ::    ProcName='Purge'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

  This%Constructed=.false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(List1D_Type), intent(out)                                     ::    LHS
  class(List1D_Type), intent(in)                                      ::    RHS

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

  type(List1D_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
