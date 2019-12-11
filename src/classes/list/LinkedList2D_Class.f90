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

module LinkedList2D_Class

use Parameters_Library
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    LinkedList2D_Type

type                                                                  ::    ListNode2D_Type
  class(*), dimension(:,:), allocatable                               ::    Values
  type(ListNode2D_Type), pointer                                      ::    Back => null()
  type(ListNode2D_Type), pointer                                      ::    Next => null()
end type

type                                                                  ::    LinkedList2D_Type
  integer                                                             ::    BrowserLoc=0
  integer                                                             ::    NbNodes=0
  type(ListNode2D_Type), pointer                                      ::    Head => null()
  type(ListNode2D_Type), pointer                                      ::    Tail => null()
  type(ListNode2D_Type), pointer                                      ::    Browser => null()
contains
  generic, public                                                     ::    Append                  =>    ListAppend2D,           &
                                                                                                          ListAppend3D
  procedure, private                                                  ::    ListAppend2D
  procedure, private                                                  ::    ListAppend3D
  generic, public                                                     ::    Get                     =>    ListGetR2D,             &
                                                                                                          ListGetI2D,             &
                                                                                                          ListGetC2D,             &
                                                                                                          ListGetL2D,             &
                                                                                                          ListGetCX2D
  procedure, private                                                  ::    ListGetR2D
  procedure, private                                                  ::    ListGetI2D
  procedure, private                                                  ::    ListGetC2D
  procedure, private                                                  ::    ListGetL2D
  procedure, private                                                  ::    ListGetCX2D
  generic, public                                                     ::    GetPointer              =>    ListGetR2DPointer,      &
                                                                                                          ListGetI2DPointer,      &
                                                                                                          ListGetC2DPointer,      &
                                                                                                          ListGetL2DPointer,      &
                                                                                                          ListGetCX2DPointer
  procedure, private                                                  ::    ListGetR2DPointer
  procedure, private                                                  ::    ListGetI2DPointer
  procedure, private                                                  ::    ListGetC2DPointer
  procedure, private                                                  ::    ListGetL2DPointer
  procedure, private                                                  ::    ListGetCX2DPointer
  procedure, public                                                   ::    GetLength               =>    ListLength
  procedure, public                                                   ::    GetNode                 =>    ListGetNode2D
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
  subroutine ListAppend2D( This, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    class(*), dimension(:,:), intent(in)                              ::    Values

    character(*), parameter                                           ::    ProcName='ListAppend2D'
    type(ListNode2D_Type), pointer                                    ::    TempNext
    integer                                                           ::    StatLoc 

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListAppend3D( This, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    class(*), dimension(:,:,:), intent(in)                            ::    Values

    character(*), parameter                                           ::    ProcName='ListAppend3D'
    integer                                                           ::    i
    integer                                                           ::    Length
    integer                                                           ::    StatLoc=0 

    Length = int(size(Values,3),8)

    i = 1
    do i = 1, Length
      call This%Append( Values=Values(:,:,i) )
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR2D( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetR2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc
    integer                                                           ::    StatLoc=0 

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (real(rkp))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarR2D', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI2D( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetI2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc
    integer                                                           ::    StatLoc=0 

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (integer)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarI2D', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC2D( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    character(:), dimension(:,:), allocatable, intent(out)            ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetC2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc
    integer                                                           ::    StatLoc=0 

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (character(*))
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarC2D', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL2D( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    logical, dimension(:,:), allocatable, intent(out)                 ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetL2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc
    integer                                                           ::    StatLoc=0 

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (logical)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarL2D', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX2D( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    complex, dimension(:,:), allocatable, intent(out)                 ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetCX2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc
    integer                                                           ::    StatLoc=0 

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (complex)
        allocate( Values, source=Value, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarCX2D', stat=StatLoc)
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR2DPointer( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    real(rkp), dimension(:,:), pointer, intent(inout)                 ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetR2DPointer'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0 

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (real(rkp))
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI2DPointer( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, dimension(:,:), pointer, intent(inout)                   ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetI2DPointer'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0 

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (integer)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC2DPointer( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    character(:), dimension(:,:), pointer, intent(inout)              ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetC2DPointer'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0 

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (character(*))
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL2DPointer( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    logical, dimension(:,:), pointer, intent(inout)                   ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetL2DPointer'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0 

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (logical)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX2DPointer( This, Node, Values )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    complex, dimension(:,:), pointer, intent(inout)                   ::    Values
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetCX2DPointer'
    type(ListNode2D_Type), pointer                                    ::    NodePointerLoc=>null()
    integer                                                           ::    StatLoc=0 

    if ( associated(Values) ) call Error%Raise( "Passed down pointer already associated with another target" )

    call This%GetNode( Node, NodePointerLoc )

    select type (Value => NodePointerLoc%Values)
      type is (complex)
        Values => Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetNode2D( This, Node, NodePointer )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node
    type(ListNode2D_Type), pointer, intent(inout)                     ::    NodePointer

    character(*), parameter                                           ::    ProcName='ListGetNode2D' 
    
    if ( Node < 1 ) call Error%Raise("Requested node index is below 0")
    if ( Node > This%NbNodes ) call Error%Raise("Requested node index is above the maximum")
    
    call This%MoveBrowser( Node )

    NodePointer => This%Browser

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine MoveBrowser( This, Node )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='MoveBrowser'
    integer                                                           ::    i, i_Max 

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ListLength( This )

    integer                                                           ::    ListLength
    class(LinkedList2D_Type), intent(in)                              ::    This

    character(*), parameter                                           ::    ProcName='ListLength' 

    ListLength = This%NbNodes

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine PurgeList( This )

    class(LinkedList2D_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode2D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode2D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc 

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNode( This, Node )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode2D_Type), pointer                                    ::    TempNext => null()
    type(ListNode2D_Type), pointer                                    ::    TempBack => null()
    type(ListNode2D_Type), pointer                                    ::    TempCurrent => null()
    integer                                                           ::    StatLoc=0

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
          nullify(TempCurrent%Next)
          nullify(TempNext%Back)
          nullify(TempNext)
      elseif ( Node == This%NbNodes ) then
          TempBack => TempCurrent%Back
          This%Tail => TempBack
          nullify(TempCurrent%Back)
          nullify(TempBack%Next)
          nullify(TempBack)
          This%BrowserLoc = This%NbNodes - 1
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
    end if

    This%NbNodes = This%NbNodes - 1

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNodeRange( This, NodeMin, NodeMax )

    class(LinkedList2D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NodeMin
    integer, intent(in)                                               ::    NodeMax

    character(*), parameter                                           ::    ProcName='RemoveNodeRange'
    integer                                                           ::    i

    if ( NodeMax > This%NbNodes .or. NodeMin < 1 ) call Error%Raise( Line='Invalid node number removal spec', ProcName=ProcName )

    if ( (NodeMax - NodeMin + 1) == This%NbNodes ) then
      call This%Purge()
    else
      i = 1
      do i = NodeMin, NodeMax
        call This%Remove( Node=i )
      end do
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(LinkedList2D_Type), intent(out)                             ::    LHS
    class(LinkedList2D_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    type(ListNode2D_Type), pointer                                    ::    NodePointer=>null()

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine FinalizerList( This )

    type(LinkedList2D_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    type(ListNode2D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode2D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc
    integer                                                           ::    StatLoc=0

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
