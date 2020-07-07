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
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

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
  generic, public                                                     ::    Get                     =>    ListGetR40D,            &
                                                                                                          ListGetR80D,            & 
                                                                                                          ListGetI40D,            &
                                                                                                          ListGetI80D,            &
                                                                                                          ListGetC0D_Char,        &
                                                                                                          ListGetC0D_String,      &
                                                                                                          ListGetL0D,             &
                                                                                                          ListGetCX0D,            &
                                                                                                          ListGetR41D,            &
                                                                                                          ListGetR81D,            &
                                                                                                          ListGetI41D,            &
                                                                                                          ListGetI81D,            &
                                                                                                          ListGetC1D,             &
                                                                                                          ListGetL1D,             &
                                                                                                          ListGetCX1D
  procedure, private                                                  ::    ListGetR40D
  procedure, private                                                  ::    ListGetR80D
  procedure, private                                                  ::    ListGetI40D
  procedure, private                                                  ::    ListGetI80D
  procedure, private                                                  ::    ListGetC0D_Char
  procedure, private                                                  ::    ListGetC0D_String
  procedure, private                                                  ::    ListGetL0D
  procedure, private                                                  ::    ListGetCX0D
  procedure, private                                                  ::    ListGetR41D
  procedure, private                                                  ::    ListGetR81D
  procedure, private                                                  ::    ListGetI41D
  procedure, private                                                  ::    ListGetI81D
  procedure, private                                                  ::    ListGetC1D
  procedure, private                                                  ::    ListGetL1D
  procedure, private                                                  ::    ListGetCX1D
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

  class(LinkedList0D_Type), intent(inout)                             ::    This
  class(*), intent(in)                                                ::    Value

  character(*), parameter                                             ::    ProcName='ListAppend0D'
  type(ListNode0D_Type), pointer                                      ::    TempNext
  integer                                                             ::    StatLoc

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

  class(LinkedList0D_Type), intent(inout)                             ::    This
  class(*), dimension(:), intent(in)                                  ::    Values
  character(*), parameter                                             ::    ProcName='ListAppend1D'
  integer                                                             ::    i
  integer                                                             ::    Length
  integer                                                             ::    StatLoc

  Length = size(Values,1)

  i = 1
  do i = 1, Length
    call This%Append(Value=Values(i))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR40D(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  real(4), intent(out)                                                ::    Value
  integer, intent(in)                                                 ::    Node
  character(*), parameter                                             ::    ProcName='ListGetR40D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (real(4))
      Value = ValueLoc
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR80D(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  real(8), intent(out)                                                ::    Value
  integer, intent(in)                                                 ::    Node
  character(*), parameter                                             ::    ProcName='ListGetR80D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (real(8))
      Value = ValueLoc
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI40D(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer(4), intent(out)                                             ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI40D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (integer(4))
      Value = ValueLoc
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI80D(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer(8), intent(out)                                             ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI80D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (integer(8))
      Value = ValueLoc
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC0D_Char(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  character(:), allocatable, intent(out)                              ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetC0D_Char'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (character(*))
      Value = ValueLoc
    type is (SMUQString_Type)
      Value = ValueLoc%Get()
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC0D_String(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  type(SMUQString_Type), allocatable, intent(out)                     ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetC0D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

  call This%GetNode(Node, NodePointerLoc)

  select type (ValueLoc => NodePointerLoc%Value)
    type is (character(*))
      Value = ValueLoc
    type is (SMUQString_Type)
      Value = ValueLoc
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetL0D(This, Node, Value)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  logical, intent(out)                                                ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetL0D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

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

  class(LinkedList0D_Type), intent(inout)                             ::    This
  complex, intent(out)                                                ::    Value
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetCX0D'
  type(ListNode0D_Type), pointer                                      ::    NodePointerLoc=>null()

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
subroutine ListGetR41D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  real(4), allocatable, dimension(:), intent(inout)                   ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetR41D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR81D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  real(8), allocatable, dimension(:), intent(inout)                   ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetR81D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI41D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetI41D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI81D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer(8), allocatable, dimension(:), intent(inout)                ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetI81D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC1D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetC1D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=VarC0D)
    Values(i) = VarC0D
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetL1D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  logical, allocatable, dimension(:), intent(inout)                   ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetL1D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetCX1D(This, Values, NodeMin, NodeMax)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  complex, allocatable, dimension(:), intent(inout)                   ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetCX1D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call EnsureArraySize(Array=Values, Size1=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Value=Values(i-NodeMinLoc+1))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetNode0D(This, Node, NodePointer)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    Node
  type(ListNode0D_Type), pointer, intent(inout)                       ::    NodePointer

  character(*), parameter                                             ::    ProcName='ListGetNode0D'

  if (Node < 1) call Error%Raise("Requested node index is below 0")
  if (Node > This%NbNodes) call Error%Raise("Requested node index is above the maximum")
  
  call This%MoveBrowser(Node)

  NodePointer => This%Browser

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine MoveBrowser(This, Node)

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='MoveBrowser'
  integer                                                             ::    i, i_Max

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

  integer                                                             ::    ListLength
  class(LinkedList0D_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='ListLength'

  ListLength = This%NbNodes

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine PurgeList(This)

  class(LinkedList0D_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='PurgeList'
  type(ListNode0D_Type), pointer                                      ::    TempCurrent => null()
  type(ListNode0D_Type), pointer                                      ::    TempNext => null()
  integer                                                             ::    i
  integer                                                             ::    NbNodesLoc

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

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='PurgeList'
  type(ListNode0D_Type), pointer                                      ::    TempNext => null()
  type(ListNode0D_Type), pointer                                      ::    TempBack => null()
  type(ListNode0D_Type), pointer                                      ::    TempCurrent => null()
  integer                                                             ::    StatLoc=0

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

  class(LinkedList0D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    NodeMin
  integer, intent(in)                                                 ::    NodeMax

  character(*), parameter                                             ::    ProcName='RemoveNodeRange'
  integer                                                             ::    i

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

  class(LinkedList0D_Type), intent(out)                               ::    LHS
  class(LinkedList0D_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  type(ListNode0D_Type), pointer                                      ::    NodePointer=>null()

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

  type(LinkedList0D_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'

  type(ListNode0D_Type), pointer                                      ::    TempCurrent => null()
  type(ListNode0D_Type), pointer                                      ::    TempNext => null()
  integer                                                             ::    i
  integer                                                             ::    NbNodesLoc
  integer                                                             ::    StatLoc=0

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
