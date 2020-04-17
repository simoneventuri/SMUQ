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

module LinkedList0D_Class

use Parameters_Library
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    LinkedList0D_Type

type                                                                  ::    ListNode0D_Type
  class(*), allocatable                                               ::    Value
  type(ListNode0D_Type), pointer                                      ::    Back => null()
  type(ListNode0D_Type), pointer                                      ::    Next => null()
end type

type                                                                  ::    LinkedList0D_Type
  integer                                                             ::    BrowserLoc=0
  integer                                                             ::    NbNodes=0
  type(ListNode0D_Type), pointer                                      ::    Head => null()
  type(ListNode0D_Type), pointer                                      ::    Tail => null()
  type(ListNode0D_Type), pointer                                      ::    Browser => null()
contains
  generic, public                                                     ::    Append                  =>    ListAppend0D,           &
                                                                                                          ListAppend1D
  procedure, private                                                  ::    ListAppend0D
  procedure, private                                                  ::    ListAppend1D
  generic, public                                                     ::    Get                     =>    ListGetR0D,             &
                                                                                                          ListGetI0D,             &
                                                                                                          ListGetC0D,             &
                                                                                                          ListGetL0D,             &
                                                                                                          ListGetCX0D,            &
                                                                                                          ListGetR1D,             &
                                                                                                          ListGetI1D,             &
                                                                                                          ListGetC1D,             &
                                                                                                          ListGetL1D,             &
                                                                                                          ListGetCX1D
  procedure, private                                                  ::    ListGetR0D
  procedure, private                                                  ::    ListGetI0D
  procedure, private                                                  ::    ListGetC0D
  procedure, private                                                  ::    ListGetL0D
  procedure, private                                                  ::    ListGetCX0D
  procedure, private                                                  ::    ListGetR1D
  procedure, private                                                  ::    ListGetI1D
  procedure, private                                                  ::    ListGetC1D
  procedure, private                                                  ::    ListGetL1D
  procedure, private                                                  ::    ListGetCX1D
  generic, public                                                     ::    GetPointer              =>    ListGetR0DPointer,      &
                                                                                                          ListGetI0DPointer,      &
                                                                                                          ListGetC0DPointer,      &
                                                                                                          ListGetL0DPointer,      &
                                                                                                          ListGetCX0DPointer
  procedure, private                                                  ::    ListGetR0DPointer
  procedure, private                                                  ::    ListGetI0DPointer
  procedure, private                                                  ::    ListGetC0DPointer
  procedure, private                                                  ::    ListGetL0DPointer
  procedure, private                                                  ::    ListGetCX0DPointer
  procedure, public                                                   ::    GetLength               =>    ListLength
  procedure, public                                                   ::    GetNode                 =>    ListGetNode0D
  procedure, private                                                  ::    MoveBrowser
  procedure, public                                                   ::    Purge                   =>    PurgeList
  generic, public                                                     ::    Remove                  =>    RemoveNode,             &
                                                                                                          RemoveNodeRange
  procedure, private                                                  ::    RemoveNode
  procedure, private                                                  ::    RemoveNodeRange
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListAppend0D(This, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    class(*), intent(in)                                              ::    Value



    character(*), parameter                                           ::    ProcName='ListAppend0D'
    type(ListNode0D_Type), pointer                                    ::    TempNext
    integer                                                           ::    StatLoc



    if (associated(This%Tail)) then
      allocate(This%Tail%Next)
      TempNext => This%Tail%Next
      TempNext%Back => This%Tail
      This%Tail => TempNext
      nullify(TempNext)
    else
      allocate(This%Head)
      This%Tail => This%Head
      This%Browser => This%Head
      This%BrowserLoc = 1
    end if

    allocate(This%Tail%Value, source=Value, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Tail%Value', stat=StatLoc)

    This%NbNodes = This%NbNodes + 1



  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListAppend1D(This, Values)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    class(*), dimension(:), intent(in)                                ::    Values
    character(*), parameter                                           ::    ProcName='ListAppend1D'
    integer                                                           ::    i
    integer                                                           ::    Length
    integer                                                           ::    StatLoc
    Length = int(size(Values,1),8)

    i = 1
    do i = 1, Length
      call This%Append(Value=Values(i))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR0D(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    real(rkp), intent(out)                                            ::    Value
    integer, intent(in)                                               ::    Node
    character(*), parameter                                           ::    ProcName='ListGetR0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    call This%GetNode(Node, NodePointerLoc)

    select type (ValueLoc => NodePointerLoc%Value)
      type is (real(rkp))
        Value = ValueLoc
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI0D(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, intent(out)                                              ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetI0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    call This%GetNode(Node, NodePointerLoc)

    select type (ValueLoc => NodePointerLoc%Value)
      type is (integer)
        Value = ValueLoc
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC0D(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    character(:), allocatable, intent(out)                            ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetC0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    call This%GetNode(Node, NodePointerLoc)

    select type (ValueLoc => NodePointerLoc%Value)
      type is (character(*))
        Value = ValueLoc
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL0D(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    logical, intent(out)                                              ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetL0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    call This%GetNode(Node, NodePointerLoc)

    select type (ValueLoc => NodePointerLoc%Value)
      type is (logical)
        Value = ValueLoc
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX0D(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    complex, intent(out)                                              ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetCX0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    call This%GetNode(Node, NodePointerLoc)

    select type (ValueLoc => NodePointerLoc%Value)
      type is (complex)
        Value = ValueLoc
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)



  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR1D(This, Values, NodeMin, NodeMax)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax

    character(*), parameter                                           ::    ProcName='ListGetR1D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    integer                                                           ::    StatLoc

    NodeMinLoc = 1
    if (present(NodeMin)) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if (present(NodeMax)) NodeMaxLoc = NodeMax

    allocate(Values(NodeMaxLoc-NodeMinLoc+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI1D(This, Values, NodeMin, NodeMax)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, allocatable, dimension(:), intent(out)                   ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax

    character(*), parameter                                           ::    ProcName='ListGetI1D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    integer                                                           ::    StatLoc

    NodeMinLoc = 1
    if (present(NodeMin)) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if (present(NodeMax)) NodeMaxLoc = NodeMax

    allocate(Values(NodeMaxLoc-NodeMinLoc+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC1D(This, Values, NodeMin, NodeMax)

    use String_Library

    class(LinkedList0D_Type), intent(inout)                           ::    This
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax

    character(*), parameter                                           ::    ProcName='ListGetC1D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc

    NodeMinLoc = 1
    if (present(NodeMin)) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if (present(NodeMax)) NodeMaxLoc = NodeMax

    allocate(Values(NodeMaxLoc-NodeMinLoc+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get(Node=i, Value=VarC0D)
      call Values(i)%Initialize(Value=VarC0D)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL1D(This, Values, NodeMin, NodeMax)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    logical, allocatable, dimension(:), intent(out)                   ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax

    character(*), parameter                                           ::    ProcName='ListGetL1D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    integer                                                           ::    StatLoc

    NodeMinLoc = 1
    if (present(NodeMin)) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if (present(NodeMax)) NodeMaxLoc = NodeMax

    allocate(Values(NodeMaxLoc-NodeMinLoc+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX1D(This, Values, NodeMin, NodeMax)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    complex, allocatable, dimension(:), intent(out)                   ::    Values
    integer, optional, intent(in)                                     ::    NodeMin
    integer, optional, intent(in)                                     ::    NodeMax

    character(*), parameter                                           ::    ProcName='ListGetCX1D'
    integer                                                           ::    i, NodeMinLoc, NodeMaxLoc
    integer                                                           ::    StatLoc

    NodeMinLoc = 1
    if (present(NodeMin)) NodeMinLoc = NodeMin

    NodeMaxLoc = This%GetLength()
    if (present(NodeMax)) NodeMaxLoc = NodeMax

    allocate(Values(NodeMaxLoc-NodeMinLoc+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

    i = NodeMinLoc
    do i = NodeMinLoc, NodeMaxLoc
      call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetR0DPointer(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    real(rkp), pointer, intent(inout)                                 ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetR0DPointer'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    if (associated(Value)) call Error%Raise("Passed down pointer already associated with another target")

    call This%GetNode(Node, NodePointerLoc)

    select type (NodeValue => NodePointerLoc%Value)
      type is (real(rkp))
        Value => NodeValue
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetI0DPointer(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, pointer, intent(inout)                                   ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetI0DPointer'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    if (associated(Value)) call Error%Raise("Passed down pointer already associated with another target")

    call This%GetNode(Node, NodePointerLoc)

    select type (NodeValue => NodePointerLoc%Value)
      type is (integer)
        Value => NodeValue
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetC0DPointer(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    character(:), pointer, intent(inout)                              ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetC0DPointer'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    if (associated(Value)) call Error%Raise("Passed down pointer already associated with another target")

    call This%GetNode(Node, NodePointerLoc)

    select type (NodeValue => NodePointerLoc%Value)
      type is (character(*))
        Value => NodeValue
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetL0DPointer(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    logical, pointer, intent(inout)                                   ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetL0DPointer'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    if (associated(Value)) call Error%Raise("Passed down pointer already associated with another target")

    call This%GetNode(Node, NodePointerLoc)

    select type (NodeValue => NodePointerLoc%Value)
      type is (logical)
        Value => NodeValue
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetCX0DPointer(This, Node, Value)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    complex, pointer, intent(inout)                                   ::    Value
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='ListGetCX0DPointer'
    type(ListNode0D_Type), pointer                                    ::    NodePointerLoc=>null()

    if (associated(Value)) call Error%Raise("Passed down pointer already associated with another target")

    call This%GetNode(Node, NodePointerLoc)

    select type (NodeValue => NodePointerLoc%Value)
      type is (complex)
        Value => NodeValue
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    nullify(NodePointerLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ListGetNode0D(This, Node, NodePointer)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node
    type(ListNode0D_Type), pointer, intent(inout)                     ::    NodePointer

    character(*), parameter                                           ::    ProcName='ListGetNode0D'

    if (Node < 1) call Error%Raise("Requested node index is below 0")
    if (Node > This%NbNodes) call Error%Raise("Requested node index is above the maximum")
    
    call This%MoveBrowser(Node)

    NodePointer => This%Browser

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine MoveBrowser(This, Node)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='MoveBrowser'
    integer                                                           ::    i, i_Max

    if (Node > This%BrowserLoc) then
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
  function ListLength(This)

    integer                                                           ::    ListLength
    class(LinkedList0D_Type), intent(in)                              ::    This

    character(*), parameter                                           ::    ProcName='ListLength'

    ListLength = This%NbNodes

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine PurgeList(This)

    class(LinkedList0D_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode0D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode0D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc

    if (.not. associated(This%Head)) return

    TempCurrent => This%Head
    nullify(This%Head)
    nullify(This%Tail)
    nullify(This%Browser)

    NbNodesLoc = This%NbNodes

    i = 1
    do i = 1, NbNodesLoc
      if (associated(TempCurrent%Next)) TempNext => TempCurrent%Next
      deallocate(TempCurrent%Value)
      if (associated(TempCurrent%Back)) nullify(TempCurrent%Back)
      if (associated(TempCurrent%Next)) nullify(TempCurrent%Next)
      deallocate(TempCurrent)
      if (associated(TempNext)) TempCurrent => Tempnext
    end do

    This%BrowserLoc=0
    This%NbNodes=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNode(This, Node)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='PurgeList'
    type(ListNode0D_Type), pointer                                    ::    TempNext => null()
    type(ListNode0D_Type), pointer                                    ::    TempBack => null()
    type(ListNode0D_Type), pointer                                    ::    TempCurrent => null()
    integer                                                           ::    StatLoc=0

    if (This%NbNodes == 0) call Error%Raise(Line='Linked list contains 0 nodes', ProcName=ProcName)

    if (Node > This%NbNodes .or. Node < 1) call Error%Raise(Line='Invalid node number removal spec', ProcName=ProcName)

    if (This%NbNodes == 1) then
      call This%Purge()
    else
      call This%GetNode(Node=Node, NodePointer=TempCurrent)
      deallocate(TempCurrent%Value, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Node%Values', ProcName=ProcName, stat=StatLoc)
      if (Node == 1) then    
          TempNext => TempCurrent%Next
          This%Head => TempNext
          nullify(TempCurrent%Next)
          nullify(TempNext%Back)
          nullify(TempNext)
      elseif (Node == This%NbNodes) then
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
      if (StatLoc /= 0) call Error%Deallocate(Name='TempCurrent', ProcName=ProcName, stat=StatLoc)
    end if

    This%NbNodes = This%NbNodes - 1

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveNodeRange(This, NodeMin, NodeMax)

    class(LinkedList0D_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NodeMin
    integer, intent(in)                                               ::    NodeMax

    character(*), parameter                                           ::    ProcName='RemoveNodeRange'
    integer                                                           ::    i

    if (NodeMax > This%NbNodes .or. NodeMin < 1) call Error%Raise(Line='Invalid node number removal spec', ProcName=ProcName)

    if ((NodeMax - NodeMin + 1) == This%NbNodes) then
      call This%Purge()
    else
      i = 1
      do i = NodeMin, NodeMax
        call This%Remove(Node=i)
      end do
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(LinkedList0D_Type), intent(out)                             ::    LHS
    class(LinkedList0D_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    type(ListNode0D_Type), pointer                                    ::    NodePointer=>null()

    call LHS%Purge()

    if (RHS%NbNodes > 0) then
      NodePointer => RHS%Head

      i = 1
      do i = 1, RHS%NbNodes-1
        call LHS%Append(Value=NodePointer%Value)
        nullify(NodePointer)
        NodePointer => NodePointer%Next
      end do

      call LHS%Append(Value=NodePointer%Value)
      nullify(NodePointer)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine FinalizerList(This)

    type(LinkedList0D_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'

    type(ListNode0D_Type), pointer                                    ::    TempCurrent => null()
    type(ListNode0D_Type), pointer                                    ::    TempNext => null()
    integer                                                           ::    i
    integer                                                           ::    NbNodesLoc
    integer                                                           ::    StatLoc=0

    if (.not. associated(This%Head)) return

    TempCurrent => This%Head
    nullify(This%Head)
    nullify(This%Tail)
    nullify(This%Browser)

    NbNodesLoc = This%NbNodes

    i = 1
    do i = 1, NbNodesLoc
      if (associated(TempCurrent%Next)) TempNext => TempCurrent%Next
      deallocate(TempCurrent%Value, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='TempCurrent%Values', ProcName=ProcName, stat=StatLoc)
      if (associated(TempCurrent%Back)) nullify(TempCurrent%Back)
      if (associated(TempCurrent%Next)) nullify(TempCurrent%Next)
      deallocate(TempCurrent, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='TempCurrent', ProcName=ProcName, stat=StatLoc)
      if (associated(TempNext)) TempCurrent => Tempnext
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
