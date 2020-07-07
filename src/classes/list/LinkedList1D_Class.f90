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
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

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
  generic, public                                                     ::    Get                     =>    ListGetR41D,            &
                                                                                                          ListGetR81D,            &
                                                                                                          ListGetI41D,            &
                                                                                                          ListGetI81D,            &
                                                                                                          ListGetC1D,             &
                                                                                                          ListGetL1D,             &
                                                                                                          ListGetCX1D,            &
                                                                                                          ListGetR42D,            &
                                                                                                          ListGetR82D,            &
                                                                                                          ListGetI42D,            &
                                                                                                          ListGetI82D,            &
                                                                                                          ListGetC2D,             &
                                                                                                          ListGetL2D,             &
                                                                                                          ListGetCX2D
  procedure, private                                                  ::    ListGetR41D
  procedure, private                                                  ::    ListGetR81D
  procedure, private                                                  ::    ListGetI41D
  procedure, private                                                  ::    ListGetI81D
  procedure, private                                                  ::    ListGetC1D
  procedure, private                                                  ::    ListGetL1D
  procedure, private                                                  ::    ListGetCX1D
  procedure, private                                                  ::    ListGetR42D
  procedure, private                                                  ::    ListGetR82D
  procedure, private                                                  ::    ListGetI42D
  procedure, private                                                  ::    ListGetI82D
  procedure, private                                                  ::    ListGetC2D
  procedure, private                                                  ::    ListGetL2D
  procedure, private                                                  ::    ListGetCX2D
  generic, public                                                     ::    GetPointer              =>    ListGetR41DPointer,     &
                                                                                                          ListGetR81DPointer,     &
                                                                                                          ListGetI41DPointer,     &
                                                                                                          ListGetI81DPointer,     &
                                                                                                          ListGetC1DPointer,      &
                                                                                                          ListGetL1DPointer,      &
                                                                                                          ListGetCX1DPointer
  procedure, private                                                  ::    ListGetR41DPointer
  procedure, private                                                  ::    ListGetR81DPointer
  procedure, private                                                  ::    ListGetI41DPointer
  procedure, private                                                  ::    ListGetI81DPointer
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
subroutine ListAppend1D(This, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  class(*), dimension(:), intent(in)                                  ::    Values

  character(*), parameter                                             ::    ProcName='ListAppend1D'
  type(ListNode1D_Type), pointer                                      ::    TempNext
  integer                                                             ::    StatLoc=0

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

  allocate(This%Tail%Values, source=Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Tail%Values', stat=StatLoc)

  This%NbNodes = This%NbNodes + 1

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListAppend2D(This, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  class(*), dimension(:,:), intent(in)                                ::    Values

  character(*), parameter                                             ::    ProcName='ListAppend2D'
  integer                                                             ::    i
  integer                                                             ::    Length
  integer                                                             ::    StatLoc=0

  Length = size(Values,2)

  i = 1
  do i = 1, Length
    call This%Append(Values=Values(:,i))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR41D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(4), dimension(:), allocatable, intent(inout)                   ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetR41D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (real(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR81D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(8), dimension(:), allocatable, intent(inout)                   ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetR81D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (real(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR42D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(4), allocatable, dimension(:,:), intent(inout)                 ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetR42D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  real(4), allocatable, dimension(:)                                  ::    VarR1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarR1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarR1D,1), Size2=NodeMaxLoc-NodeMinLoc+1 ,DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarR1D) 
    if (size(VarR1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarR1D
    end if
  end do

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR82D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(8), allocatable, dimension(:,:), intent(inout)                 ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetR82D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  real(8), allocatable, dimension(:)                                  ::    VarR1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarR1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarR1D,1), Size2=NodeMaxLoc-NodeMinLoc+1 ,DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarR1D) 
    if (size(VarR1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarR1D
    end if
  end do

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR41DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(4), dimension(:), pointer, intent(inout)                       ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetR41DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (real(4))
      Values => Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetR81DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  real(8), dimension(:), pointer, intent(inout)                       ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetR81DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (real(8))
      Values => Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI41D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(4), dimension(:), allocatable, intent(inout)                ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI41D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (integer(4))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI81D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(8), dimension(:), allocatable, intent(inout)                ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI81D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (integer(8))
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI42D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(4), allocatable, dimension(:,:), intent(inout)              ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetI42D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer(4), allocatable, dimension(:)                               ::    VarI1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarI1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarI1D,1), Size2=NodeMaxLoc-NodeMinLoc+1 ,DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarI1D) 
    if (size(VarI1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarI1D
    end if
  end do

  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI82D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(8), allocatable, dimension(:,:), intent(inout)              ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetI82D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  integer(8), allocatable, dimension(:)                               ::    VarI1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarI1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarI1D,1), Size2=NodeMaxLoc-NodeMinLoc+1 ,DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarI1D) 
    if (size(VarI1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarI1D
    end if
  end do

  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI41DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(4), dimension(:), pointer, intent(inout)                    ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI41DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (integer(4))
      Values => Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetI81DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer(8), dimension(:), pointer, intent(inout)                    ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetI81DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (integer(8))
      Values => Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC1D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  type(SMUQString_Type), dimension(:), allocatable, intent(inout)     ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetC1D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (SMUQString_Type)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC2D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  type(SMUQString_Type), allocatable, dimension(:,:), intent(inout)   ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetC2D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarC1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarC1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarC1D,1), Size2=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarC1D) 
    if (size(VarC1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarC1D
    end if
  end do

  deallocate(VarC1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarC1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetC1DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  type(SMUQString_Type), dimension(:), pointer, intent(inout)         ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetC1DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (SMUQString_Type)
      Values => Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetL1D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  logical, dimension(:), allocatable, intent(inout)                   ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetL1D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (logical)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetL2D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  logical, allocatable, dimension(:,:), intent(inout)                 ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetL2D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  logical, allocatable, dimension(:)                                  ::    VarL1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarL1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarL1D,1), Size2=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarL1D) 
    if (size(VarL1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarL1D
    end if
  end do

  deallocate(VarL1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarL1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetL1DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  logical, dimension(:), pointer, intent(inout)                       ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetL1DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

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
subroutine ListGetCX1D(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  complex, dimension(:), allocatable, intent(inout)                   ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetCX1D'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  call This%GetNode(Node, NodePointerLoc)

  select type (Value => NodePointerLoc%Values)
    type is (complex)
      call EnsureArraySize(Array=Values, Size1=size(Value,1), DefaultValue=.false.)
      Values = Value
    class default
      call Error%Raise("Requested node contains a value that does not match the requested type")
  end select

  nullify(NodePointerLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetCX2D(This, Values, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  complex, allocatable, dimension(:,:), intent(inout)                 ::    Values
  integer, optional, intent(in)                                       ::    NodeMin
  integer, optional, intent(in)                                       ::    NodeMax

  character(*), parameter                                             ::    ProcName='ListGetCX2D'
  integer                                                             ::    i, NodeMinLoc, NodeMaxLoc
  complex, allocatable, dimension(:)                                  ::    VarCX1D
  integer                                                             ::    StatLoc

  NodeMinLoc = 1
  if (present(NodeMin)) NodeMinLoc = NodeMin

  NodeMaxLoc = This%GetLength()
  if (present(NodeMax)) NodeMaxLoc = NodeMax

  call This%Get(Node=NodeMinLoc, Values=VarCX1D) 

  call EnsureArraySize(Array=Values, Size1=size(VarCX1D,1), Size2=NodeMaxLoc-NodeMinLoc+1, DefaultValue=.false.)

  i = NodeMinLoc
  do i = NodeMinLoc, NodeMaxLoc
    call This%Get(Node=i, Values=VarCX1D) 
    if (size(VarCX1D,1) /= size(Values,1)) then
      call Error%Raise(Line='Linked list item not the same length as others', ProcName=ProcName)
    else
      Values(:,i-NodeMinLoc+1) = VarCX1D
    end if
  end do

  deallocate(VarCX1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarCX1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ListGetCX1DPointer(This, Node, Values)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  complex, dimension(:), pointer, intent(inout)                       ::    Values
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='ListGetCX1DPointer'
  type(ListNode1D_Type), pointer                                      ::    NodePointerLoc=>null()
  integer                                                             ::    StatLoc=0

  if (associated(Values)) call Error%Raise("Passed down pointer already associated with another target")

  call This%GetNode(Node, NodePointerLoc)

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
subroutine ListGetNode1D(This, Node, NodePointer)

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    Node
  type(ListNode1D_Type), pointer, intent(inout)                       ::    NodePointer

  character(*), parameter                                             ::    ProcName='ListGetNode1D'

  if (Node < 1) call Error%Raise("Requested node index is below 0")
  if (Node > This%NbNodes) call Error%Raise("Requested node index is above the maximum")
  
  call This%MoveBrowser(Node)

  NodePointer => This%Browser

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine MoveBrowser(This, Node)

  class(LinkedList1D_Type), intent(inout)                             ::    This
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
  class(LinkedList1D_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='ListLength'

  ListLength = This%NbNodes

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine PurgeList(This)

  class(LinkedList1D_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='PurgeList'
  type(ListNode1D_Type), pointer                                      ::    TempCurrent => null()
  type(ListNode1D_Type), pointer                                      ::    TempNext => null()
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
    deallocate(TempCurrent%Values)
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

  class(LinkedList1D_Type), intent(inout)                             ::    This
  integer, intent(in)                                                 ::    Node

  character(*), parameter                                             ::    ProcName='PurgeList'
  type(ListNode1D_Type), pointer                                      ::    TempNext => null()
  type(ListNode1D_Type), pointer                                      ::    TempBack => null()
  type(ListNode1D_Type), pointer                                      ::    TempCurrent => null()
  integer                                                             ::    StatLoc=0

  if (This%NbNodes == 0) call Error%Raise(Line='Linked list contains 0 nodes', ProcName=ProcName)

  if (Node > This%NbNodes .or. Node < 1) call Error%Raise(Line='Invalid node number removal spec', ProcName=ProcName)

  if (This%NbNodes == 1) then
    call This%Purge()
  else
    call This%GetNode(Node=Node, NodePointer=TempCurrent)
    deallocate(TempCurrent%Values, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Node%Values', ProcName=ProcName, stat=StatLoc)
    if (Node == 1) then    
        TempNext => TempCurrent%Next
        This%Head => TempNext
        This%Browser => TempNext
        nullify(TempCurrent%Next)
        nullify(TempNext%Back)
        nullify(TempNext)
    elseif (Node == This%NbNodes) then
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
    if (StatLoc /= 0) call Error%Deallocate(Name='TempCurrent', ProcName=ProcName, stat=StatLoc)
    This%NbNodes = This%NbNodes - 1
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine RemoveNodeRange(This, NodeMin, NodeMax)

  class(LinkedList1D_Type), intent(inout)                             ::    This
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
      call This%Remove(Node=NodeMin)
    end do
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(LinkedList1D_Type), intent(out)                               ::    LHS
  class(LinkedList1D_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  type(ListNode1D_Type), pointer                                      ::    NodePointer=>null()

  call LHS%Purge()

  if (RHS%NbNodes > 0) then
    NodePointer => RHS%Head

    i = 1
    do i = 1, RHS%NbNodes-1
      call LHS%Append(Values=NodePointer%Values)
      nullify(NodePointer)
      NodePointer => NodePointer%Next
    end do

    call LHS%Append(Values=NodePointer%Values)
    nullify(NodePointer)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine FinalizerList(This)

  type(LinkedList1D_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='FinalizerList'
  type(ListNode1D_Type), pointer                                      ::    TempCurrent => null()
  type(ListNode1D_Type), pointer                                      ::    TempNext => null()
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
    deallocate(TempCurrent%Values, stat=StatLoc)
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
