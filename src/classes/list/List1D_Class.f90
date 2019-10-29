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
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    List1D_Type

type                                                                  ::    List1D_Type
  class(*), dimension(:), pointer                                     ::    Values=>null()
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Set
  generic, public                                                     ::    Get                     =>    GetR1D,                 &
                                                                                                          GetI1D,                 &
                                                                                                          GetC1D,                 &
                                                                                                          GetL1D,                 &
                                                                                                          GetCX1D
  procedure, private                                                  ::    GetR1D
  procedure, private                                                  ::    GetI1D
  procedure, private                                                  ::    GetC1D
  procedure, private                                                  ::    GetL1D
  procedure, private                                                  ::    GetCX1D
  generic, public                                                     ::    GetPointer              =>    GetR1DPointer,          &
                                                                                                          GetI1DPointer,          &
                                                                                                          GetC1DPointer,          &
                                                                                                          GetL1DPointer,          &
                                                                                                          GetCX1DPointer
  procedure, private                                                  ::    GetR1DPointer
  procedure, private                                                  ::    GetI1DPointer
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
  subroutine Set( This, Values, Debug )

    class(List1D_Type), intent(inout)                                 ::    This
    class(*), dimension(:), intent(in)                                ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Purge()

    allocate( This%Values, source=Values, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetR1D( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    real(rkp), dimension(:), allocatable, intent(out)                 ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetR1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (real(rkp))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetR1DPointer( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    real(rkp), dimension(:), pointer, intent(inout)                   ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetR1DPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (real(rkp))
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetI1D( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    integer, dimension(:), allocatable, intent(out)                   ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetI1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (integer)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetI1DPointer( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    integer, dimension(:), pointer, intent(inout)                     ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetI1DPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (integer)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetC1D( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    character(:), dimension(:), allocatable, intent(out)              ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetC1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (character(*))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetC1DPointer( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    character(:), dimension(:), pointer, intent(inout)                ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetC1DPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (character(*))
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetL1D( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    logical, dimension(:), allocatable, intent(out)                   ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetL1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (logical)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetL1DPointer( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    logical, dimension(:), pointer, intent(inout)                     ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetL1DPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (logical)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCX1D( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    complex, dimension(:), allocatable, intent(out)                   ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCX1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    select type (Value => This%Values)
      type is (complex)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCX1DPointer( This, Values, Debug )

    class(List1D_Type), intent(in)                                    ::    This
    complex, dimension(:), pointer, intent(inout)                     ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCX1DPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object never constructed', ProcName=ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    select type (Value => This%Values)
      type is (complex)
        Values => Value
      class default
        call Error%Raise("Requested value does not match the requested type")
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Purge( This, Debug )

    class(List1D_Type), intent(inout)                                 ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Purge'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    This%Constructed=.false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(List1D_Type), intent(out)                                   ::    LHS
    class(List1D_Type), intent(in)                                    ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Purge()

    if ( RHS%Constructed ) then
      allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Values', ProcName=ProcName, stat=StatLoc )
    end if

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine FinalizerList( This )

    type(List1D_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
