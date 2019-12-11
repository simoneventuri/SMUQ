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
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    List2D_Type

type                                                                  ::    List2D_Type
  class(*), dimension(:,:), pointer                                   ::    Values=>null()
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR2D,                 &
                                                                                                          GetI2D,                 &
                                                                                                          GetC2D,                 &
                                                                                                          GetL2D,                 &
                                                                                                          GetCX2D
  procedure, private                                                  ::    GetR2D
  procedure, private                                                  ::    GetI2D
  procedure, private                                                  ::    GetC2D
  procedure, private                                                  ::    GetL2D
  procedure, private                                                  ::    GetCX2D
  generic, public                                                     ::    GetPointer              =>    GetR2DPointer,          &
                                                                                                          GetI2DPointer,          &
                                                                                                          GetC2DPointer,          &
                                                                                                          GetL2DPointer,          &
                                                                                                          GetCX2DPointer
  procedure, private                                                  ::    GetR2DPointer
  procedure, private                                                  ::    GetI2DPointer
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
  subroutine Set( This, Values )

    class(List2D_Type), intent(inout)                                 ::    This
    class(*), dimension(:,:), intent(in)                              ::    Values

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

    class(List2D_Type), intent(in)                                    ::    This
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    Values

    character(*), parameter                                           ::    ProcName='GetR2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (real(rkp))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetR2DPointer( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    real(rkp), dimension(:,:), pointer, intent(inout)                 ::    Values

    character(*), parameter                                           ::    ProcName='GetR2DPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (real(rkp))
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetI2D( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Values

    character(*), parameter                                           ::    ProcName='GetI2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (integer)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetI2DPointer( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    integer, dimension(:,:), pointer, intent(inout)                   ::    Values

    character(*), parameter                                           ::    ProcName='GetI2DPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (integer)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetC2D( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    character(:), dimension(:,:), allocatable, intent(out)            ::    Values

    character(*), parameter                                           ::    ProcName='GetC2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (character(*))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetC2DPointer( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    character(:), dimension(:,:), pointer, intent(inout)              ::    Values

    character(*), parameter                                           ::    ProcName='GetC2DPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (character(*))
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetL2D( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    logical, dimension(:,:), allocatable, intent(out)                 ::    Values

    character(*), parameter                                           ::    ProcName='GetL2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (logical)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetL2DPointer( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    logical, dimension(:,:), pointer, intent(inout)                   ::    Values

    character(*), parameter                                           ::    ProcName='GetL2DPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (logical)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCX2D( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    complex, dimension(:,:), allocatable, intent(out)                 ::    Values

    character(*), parameter                                           ::    ProcName='GetCX2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (complex)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCX2DPointer( This, Values )

    class(List2D_Type), intent(in)                                    ::    This
    complex, dimension(:,:), pointer, intent(inout)                   ::    Values

    character(*), parameter                                           ::    ProcName='GetCX2DPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (complex)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Purge( This )

    class(List2D_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Purge'
    integer                                                           ::    StatLoc=0

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    This%Constructed=.false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(List2D_Type), intent(out)                                   ::    LHS
    class(List2D_Type), intent(in)                                    ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Purge()

    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Values', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine FinalizerList( This )

    type(List2D_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
