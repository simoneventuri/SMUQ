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

module LinkedList1D_Class

use Parameters_Library
use ArrayRoutines_Module
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    LinkedList1D_Type

type                                                                  ::    ListNode1D_Type
  class(*), dimension(:), allocatable                                 ::    Values
  type(ListNode1D_Type), pointer                                      ::    Back => null()
  type(ListNode1D_Type), pointer                                      ::    Next => null()
end type

type                                                                  ::    LinkedList1D_Type
  integer                                                             ::    BrowserLoc=0
  integer                                                             ::    NbNodes=0
  type(ListNode1D_Type), pointer                                      ::    Head => null()
  type(ListNode1D_Type), pointer                                      ::    Tail => null()
  type(ListNode1D_Type), pointer                                      ::    Browser => null()
contains
  generic, public                                                     ::    Append                  =>    ListAppend1D,           &
                                                                                                          ListAppend2D
  procedure, private                                                  ::    ListAppend1D
  procedure, private                                                  ::    ListAppend2D
  generic, public                                                     ::    Get                     =>    ListGetR1D,             &
                                                                                                          ListGetI1D,             &
                                                                                                          ListGetC1D,             &
                                                                                                          ListGetL1D,             &
                                                                                                          ListGetCX1D,            &
                                                                                                          ListGetR2D,             &
                                                                                                          ListGetI2D,             &
                                                                                                          ListGetC2D,             &
                                                                                                          ListGetL2D,             &
                                                                                                          ListGetCX2D
  procedure, private                                                  ::    ListGetR1D
  procedure, private                                                  ::    ListGetI1D
  procedure, private                                                  ::    ListGetC1D
  procedure, private                                                  ::    ListGetL1D
  procedure, private                                                  ::    ListGetCX1D
  procedure, private                                                  ::    ListGetR2D
  procedure, private                                                  ::    ListGetI2D
  procedure, private                                                  ::    ListGetC2D
  procedure, private                                                  ::    ListGetL2D
  procedure, private                                                  ::    ListGetCX2D
  generic, public                                                     ::    GetPointer              =>    ListGetR1DPointer,      &
                                                                                                          ListGetI1DPointer,      &
                                                                                                          ListGetC1DPointer,      &
                                                                                                          ListGetL1DPointer,      &
                                                                                                          ListGetCX1DPointer
  procedure, private                                                  ::    ListGetR1DPointer
  procedure, private                                                  ::    ListGetI1DPointer
  procedure, private                                                  ::    ListGetC1DPointer
  procedure, private                                                  ::    ListGetL1DPointer
  procedure, private                                                  ::    ListGetCX1DPointer
  procedure, public                                                   ::    GetLength               =>    ListLength
  procedure, public                                                   ::    GetNode                 =>    ListGetNode1D
  procedure, private                                                  ::    MoveBrowser
  procedure, public                                                   ::    Purge                   =>    PurgeList
  generic, public                                                     ::    Remove                  =>    RemoveNode,             &
                                                                                                          RemoveNodeRange
  procedure, private                                                  ::    RemoveNode
  procedure, private                                                  ::    RemoveNodeRange
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListAppend1D( This, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    class(*), dimension(:), intent(in)                                ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListAppend1D'
    type(ListNode1D_Type), pointer                                    ::    TempNext
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(This%Tail) ) then
      allocate( This%Tail%Next )
      TempNext => This%Tail%Next
      TempNext%Back => This%Tail
      This%Tail => TempNext
      nullify(TempNext)
    else
      allocate( This%Head )
      This%Tail => This%Head
      This%Browser => This%Head
      This%BrowserLoc = 1
    end if

    allocate( This%Tail%Values, source=Values, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

    This%NbNodes = This%NbNodes + 1

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListAppend2D( This, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    class(*), dimension(:,:), intent(in)                              ::    Values
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListAppend2D'
    integer                                                           ::    i
    integer                                                           ::    Length
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    Length = int(size(Values,2),8)

    i = 1
    do i = 1, Length
      call This%Append( Values=Values(:,i) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR1D( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    real(rkp), dimension(:), allocatable, intent(out)                 ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetR1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (real(rkp))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR2D( This, Values, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetR2D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    NodeMinLoc = 1
    if ( present(NodeMin) ) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if ( present(NodeMax) ) NodeMaxLoc = NodeMax

    call This%Get( Node=NodeMinLoc, Values=VarR1D ) 

    allocate( Values(size(VarR1D,1), NodeMaxLoc-NodeMinLoc+1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get( Node=i, Values=VarR1D ) 
      if ( size(VarR1D,1) /= size(Values,1) ) then
        call Error%Raise( Line='Linked list item not the same length as others', ProcName=ProcName )
      else
        Values(:,i-NodeMinLoc+1) = VarR1D
      end if
    end do

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR1DPointer( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    real(rkp), dimension(:), pointer, intent(inout)                   ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetR1DPointer'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (real(rkp))
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI1D( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, dimension(:), allocatable, intent(out)                   ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetI1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (integer)
        allocate( Values, source=Value, stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI2D( This, Values, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, allocatable, dimension(:,:), intent(out)                 ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetI2D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    NodeMinLoc = 1
    if ( present(NodeMin) ) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if ( present(NodeMax) ) NodeMaxLoc = NodeMax

    call This%Get( Node=NodeMinLoc, Values=VarI1D ) 

    allocate( Values(size(VarI1D,1), NodeMaxLoc-NodeMinLoc+1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get( Node=i, Values=VarI1D ) 
      if ( size(VarI1D,1) /= size(Values,1) ) then
        call Error%Raise( Line='Linked list item not the same length as others', ProcName=ProcName )
      else
        Values(:,i-NodeMinLoc+1) = VarI1D
      end if
    end do

    deallocate(VarI1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI1DPointer( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, dimension(:), pointer, intent(inout)                     ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetI1DPointer'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (integer)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC1D( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    character(:), dimension(:), allocatable, intent(out)              ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetC1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (character(*))
        allocate( Values, source=Value, stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC2D( This, Values, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    character(100), allocatable, dimension(:,:), intent(out)          ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetC2D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    NodeMinLoc = 1
    if ( present(NodeMin) ) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if ( present(NodeMax) ) NodeMaxLoc = NodeMax

    call This%Get( Node=NodeMinLoc, Values=VarC1D ) 

    allocate( Values(size(VarC1D,1), NodeMaxLoc-NodeMinLoc+1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get( Node=i, Values=VarC1D ) 
      if ( size(VarC1D,1) /= size(Values,1) ) then
        call Error%Raise( Line='Linked list item not the same length as others', ProcName=ProcName )
      else
        Values(:,i-NodeMinLoc+1) = VarC1D
      end if
    end do

    deallocate(VarC1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC1DPointer( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    character(:), dimension(:), pointer, intent(inout)                ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetC1DPointer'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (character(*))
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL1D( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    logical, dimension(:), allocatable, intent(out)                   ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetL1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (logical)
        allocate( Values, source=Value, stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL2D( This, Values, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    logical, allocatable, dimension(:,:), intent(out)                 ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetL2D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    logical, allocatable, dimension(:)                                ::    VarL1D
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    NodeMinLoc = 1
    if ( present(NodeMin) ) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if ( present(NodeMax) ) NodeMaxLoc = NodeMax

    call This%Get( Node=NodeMinLoc, Values=VarL1D ) 

    allocate( Values(size(VarL1D,1), NodeMaxLoc-NodeMinLoc+1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get( Node=i, Values=VarL1D ) 
      if ( size(VarL1D,1) /= size(Values,1) ) then
        call Error%Raise( Line='Linked list item not the same length as others', ProcName=ProcName )
      else
        Values(:,i-NodeMinLoc+1) = VarL1D
      end if
    end do

    deallocate(VarL1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarL1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL1DPointer( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    logical, dimension(:), pointer, intent(inout)                     ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetL1DPointer'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (logical)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX1D( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    complex, dimension(:), allocatable, intent(out)                   ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetCX1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (complex)
        allocate( Values, source=Value, stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Values', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX2D( This, Values, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    complex, allocatable, dimension(:,:), intent(out)                 ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetCX2D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    complex, allocatable, dimension(:)                                ::    VarCX1D
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    NodeMinLoc = 1
    if ( present(NodeMin) ) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if ( present(NodeMax) ) NodeMaxLoc = NodeMax

    call This%Get( Node=NodeMinLoc, Values=VarCX1D ) 

    allocate( Values(size(VarCX1D,1), NodeMaxLoc-NodeMinLoc+1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get( Node=i, Values=VarCX1D ) 
      if ( size(VarCX1D,1) /= size(Values,1) ) then
        call Error%Raise( Line='Linked list item not the same length as others', ProcName=ProcName )
      else
        Values(:,i-NodeMinLoc+1) = VarCX1D
      end if
    end do

    deallocate(VarCX1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarCX1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX1DPointer( This, Node, Values, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    complex, dimension(:), pointer, intent(inout)                     ::    Values
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetCX1DPointer'
    type(ListNode1D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (complex)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetNode1D( This, Node, NodePointer, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node
    type(ListNode1D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListGetNode1D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( Node < 1 ) call Error%Raise("Requested node index is below 0")
    if ( Node > This%NbNodes ) call Error%Raise("Requested node index is above the maximum")
    
    call This%MoveBrowser( Node )

    NodePointer => This%Browser

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine MoveBrowser( This, Node, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='MoveBrowser'
    integer                                                           ::    i, i_Max

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( Node > This%BrowserLoc ) then
      i = 1
      i_Max = Node - This%BrowserLoc
      do i = 1, i_Max
        This%Browser => This%Browser%Next
        This%BrowserLoc = This%BrowserLoc + 1
      end do
    else
      i = 1
      i_Max = This%BrowserLoc-Node
      do i = 1, i_Max
        This%Browser => This%Browser%Back
        This%BrowserLoc = This%BrowserLoc - 1
      end do
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ListLength( This, Debug )

    integer                                                           ::    ListLength
    class(LinkedList1D_Type), intent(in)                              ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ListLength'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    ListLength = This%NbNodes

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine PurgeList( This, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode1D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode1D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( .not. associated(This%Head) ) return

    TempCurrent => This%Head
    nullify(This%Head)
    nullify(This%Tail)
    nullify(This%Browser)

    NbNodesLoc = This%NbNodes

    i = 1
    do i = 1, NbNodesLoc
      if ( associated(TempCurrent%Next) ) TempNext => TempCurrent%Next
      deallocate(TempCurrent%Values)
      if ( associated(TempCurrent%Back) ) nullify(TempCurrent%Back)
      if ( associated(TempCurrent%Next) ) nullify(TempCurrent%Next)
      deallocate( TempCurrent )
      if ( associated(TempNext) ) TempCurrent => Tempnext
    end do

    This%BrowserLoc=0
    This%NbNodes=0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNode( This, Node, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode1D_Type), pointer                                    ::    TempNext => null()
    type(ListNode1D_Type), pointer                                    ::    TempBack => null()
    type(ListNode1D_Type), pointer                                    ::    TempCurrent => null()
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( This%NbNodes == 0 ) call Error%Raise( Line='Linked list contains 0 nodes', ProcName=ProcName )

    if ( Node > This%NbNodes .or. Node < 1 ) call Error%Raise( Line='Invalid node number removal spec', ProcName=ProcName )

    if ( This%NbNodes == 1 ) then
      call This%Purge()
    else
      call This%GetNode( Node=Node, NodePointer=TempCurrent )
      deallocate(TempCurrent%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Node%Values', ProcName=ProcName, stat=StatLoc )
      if ( Node == 1 ) then    
          TempNext => TempCurrent%Next
          This%Head => TempNext
          This%Browser => TempNext
          nullify(TempCurrent%Next)
          nullify(TempNext%Back)
          nullify(TempNext)
      elseif ( Node == This%NbNodes ) then
          TempBack => TempCurrent%Back
          This%Tail => TempBack
          This%Browser => TempBack
          This%BrowserLoc = This%NbNodes - 1
          nullify(TempCurrent%Back)
          nullify(TempBack%Next)
          nullify(TempBack)
      else
          TempNext => TempCurrent%Next
          TempBack => TempCurrent%Back
          nullify(TempCurrent%Next)
          nullify(TempCurrent%Back)
          TempBack%Next => TempNext
          TempNext%Back => TempBack
          This%Browser => TempNext
          nullify(TempNext)
          nullify(TempBack)
      end if
      deallocate(TempCurrent, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='TempCurrent', ProcName=ProcName, stat=StatLoc )
      This%NbNodes = This%NbNodes - 1
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNodeRange( This, NodeMin, NodeMax, Debug )

    class(LinkedList1D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NodeMin
    integer, intent(in)                                               ::    NodeMax
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='RemoveNodeRange'
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( NodeMax > This%NbNodes .or. NodeMin < 1 ) call Error%Raise( Line='Invalid node number removal spec', ProcName=ProcName )

    if ( (NodeMax - NodeMin + 1) == This%NbNodes ) then
      call This%Purge()
    else
      i = 1
      do i = NodeMin, NodeMax
        call This%Remove( Node=NodeMin )
      end do
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LinkedList1D_Type), intent(out)                             ::    LHS
    class(LinkedList1D_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    type(ListNode1D_Type), pointer                                    ::    NodePointer=>null()

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Purge()

    if ( RHS%NbNodes > 0 ) then
      NodePointer => RHS%Head

      i = 1
      do i = 1, RHS%NbNodes-1
        call LHS%Append( Values=NodePointer%Values )
        nullify( NodePointer )
        NodePointer => NodePointer%Next
      end do

      call LHS%Append( Values=NodePointer%Values )
      nullify( NodePointer )
    end if

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine FinalizerList( This )

    type(LinkedList1D_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    type(ListNode1D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode1D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. associated(This%Head) ) return

    TempCurrent => This%Head
    nullify(This%Head)
    nullify(This%Tail)
    nullify(This%Browser)

    NbNodesLoc = This%NbNodes

    i = 1
    do i = 1, NbNodesLoc
      if ( associated(TempCurrent%Next) ) TempNext => TempCurrent%Next
      deallocate(TempCurrent%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='TempCurrent%Values', ProcName=ProcName, stat=StatLoc )
      if ( associated(TempCurrent%Back) ) nullify(TempCurrent%Back)
      if ( associated(TempCurrent%Next) ) nullify(TempCurrent%Next)
      deallocate( TempCurrent, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='TempCurrent', ProcName=ProcName, stat=StatLoc )
      if ( associated(TempNext) ) TempCurrent => Tempnext
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
