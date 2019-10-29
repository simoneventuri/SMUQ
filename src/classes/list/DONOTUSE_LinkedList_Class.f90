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

module LinkedList_Class

use Parameters_Library
use Logger_Class          ,only:  Logger
use Error_Class           ,only:  Error

implicit none

private

public                                                                ::    LinkedList_Type

type                                                                  ::    ListNode_Type
  type(ListNode1D_Type), pointer                                       ::    Back => null()
  type(ListNode1D_Type), pointer                                       ::    Next => null()
contains
!  Final                                                               ::    FinalizerNode
end type

type, extends(ListNode_Type)                                          ::    ListNode0D_Type
  class(*), allocatable                                               ::    Value
contains
!  Final                                                               ::    FinalizerNode0D
end type

type, extends(ListNode_Type)                                          ::    ListNode1D_Type
  class(*), dimension(:), allocatable                                 ::    Value
contains
  Final                                                               ::    FinalizerNode1D
end type

type, extends(ListNode_Type)                                          ::    ListNode2D_Type
  class(*), dimension(:,:), allocatable                               ::    Value
contains
!  Final                                                               ::    FinalizerNode2D
end type

type                                                                  ::    LinkedList_Type
  integer(ikp)                                                        ::    BrowserLoc=0
  integer(ikp)                                                        ::    NbNodes=0
  class(ListNode1D_Type), pointer                                       ::    Head => null()
  class(ListNode1D_Type), pointer                                       ::    Tail => null()
  class(ListNode1D_Type), pointer                                       ::    Browser => null()
contains
  generic, public                                                     ::    Append                  =>    ListAppend0D,           &
                                                                                                          ListAppend1D,           &
                                                                                                          ListAppend2D
  procedure, private                                                  ::    ListAppend0D
  procedure, private                                                  ::    ListAppend1D
  procedure, private                                                  ::    ListAppend2D
  generic, public                                                     ::    Get                     =>    ListGetR0D,             &
                                                                                                          ListGetR1D,             &
                                                                                                          ListGetR2D,             &
                                                                                                          ListGetI0D,             &
                                                                                                          ListGetI1D,             &
                                                                                                          ListGetI2D,             &
                                                                                                          ListGetC0D,             &
                                                                                                          ListGetC1D,             &
                                                                                                          ListGetC2D,             &
                                                                                                          ListGetL0D,             &
                                                                                                          ListGetL1D,             &
                                                                                                          ListGetL2D,             &
                                                                                                          ListGetCX0D,            &
                                                                                                          ListGetCX1D,            &
                                                                                                          ListGetCX2D
  procedure, private                                                  ::    ListGetR0D
  procedure, private                                                  ::    ListGetR1D
  procedure, private                                                  ::    ListGetR2D
  procedure, private                                                  ::    ListGetI0D
  procedure, private                                                  ::    ListGetI1D
  procedure, private                                                  ::    ListGetI2D
  procedure, private                                                  ::    ListGetC0D
  procedure, private                                                  ::    ListGetC1D
  procedure, private                                                  ::    ListGetC2D
  procedure, private                                                  ::    ListGetL0D
  procedure, private                                                  ::    ListGetL1D
  procedure, private                                                  ::    ListGetL2D
  procedure, private                                                  ::    ListGetCX0D
  procedure, private                                                  ::    ListGetCX1D
  procedure, private                                                  ::    ListGetCX2D
  procedure, public                                                   ::    Length                  =>    ListLength
  procedure, public                                                   ::    Reset                   =>    ListReset
  generic, public                                                     ::    GetNode                 =>    ListGetNode0D,          &
                                                                                                          ListGetNode1D,          &
                                                                                                          ListGetNode2D
  procedure, private                                                  ::    ListGetNode0D
  procedure, private                                                  ::    ListGetNode1D
  procedure, private                                                  ::    ListGetNode2D
  generic, private                                                    ::    DestroyNode             =>    DestroyNode0D,          &
                                                                                                          DestroyNode1D,          &
                                                                                                          DestroyNode2D
  procedure, private                                                  ::    DestroyNode0D
  procedure, private                                                  ::    DestroyNode1D
  procedure, private                                                  ::    DestroyNode2D
  procedure, private                                                  ::    MoveBrowser
!  Final                                                               ::    FinalizerList
end type

logical   ,parameter                                                  ::    i_Debug_Global = .true.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListAppend0D( This, Value, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    class(*), intent(in)                                              ::    Value
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListAppend0D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

!    if ( associated(This%Tail) ) then
!      allocate( ListNode0D_Type :: This%Tail%Next )
!      This%Tail%Next%Back => This%Tail
!      This%Tail => This%Tail%Next
!    else
!      allocate( ListNode0D_Type :: This%Head )
!      This%Tail => This%Head
!      This%Browser => This%Head
!      This%BrowserLoc = 1
!    end if

!    select type (Tail => This%Tail)
!      type is (ListNode0D_Type)
!        allocate( Tail%Value, source=Value )
!      class default
!        call Error%Raise("Something went wrong allocating linked list node value")
!    end select

!    This%NbNodes = This%NbNodes + 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListAppend1D( This, Value, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    class(*), dimension(:), intent(in)                                ::    Value
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListAppend1D'
    integer                                                           ::    IOLoc

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( associated(This%Tail) ) then
      allocate( ListNode1D_Type :: This%Tail%Next )
      This%Tail%Next%Back => This%Tail
      This%Tail => This%Tail%Next
    else
      allocate( ListNode1D_Type :: This%Head )
      This%Tail => This%Head
      This%Browser => This%Head
      This%BrowserLoc = 1
    end if

    select type (Tail => This%Tail)
      type is (ListNode1D_Type)
        allocate( Tail%Value, source=Value, stat=IOLoc )
        if ( IOLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Tail%Value', stat=IOLoc)
      class default
        call Error%Raise('Something went wrong allocating linked list node value')
    end select

    This%NbNodes = This%NbNodes + 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListAppend2D( This, Value, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    class(*), dimension(:,:), intent(in)                              ::    Value
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListAppend2D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

!    if ( associated(This%Tail) ) then
!      allocate( ListNode2D_Type :: This%Tail%Next )
!      This%Tail%Next%Back => This%Tail
!      This%Tail => This%Tail%Next
!    else
!      allocate( ListNode2D_Type :: This%Head )
!      This%Tail => This%Head
!      This%Browser => This%Head
!      This%BrowserLoc = 1
!    end if

!    select type (Tail => This%Tail)
!      type is (ListNode2D_Type)
!        allocate( Tail%Value, source=Value )
!      class default
!        call Error%Raise("Something went wrong allocating linked list node value")
!    end select

!    This%NbNodes = This%NbNodes + 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetR0D( This, iNode, VarR0D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    real(rkp), intent(out)                                            ::    VarR0D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetR0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (real(rkp))
        VarR0D = Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetR1D( This, iNode, VarR1D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    real(rkp), dimension(:), allocatable, intent(out)                 ::    VarR1D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetR1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (real(rkp))
        allocate( VarR1D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetR2D( This, iNode, VarR2D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    VarR2D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetR2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (real(rkp))
        allocate( VarR2D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetI0D( This, iNode, VarI0D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer, intent(out)                                              ::    VarI0D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetI0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (integer)
        VarI0D = Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetI1D( This, iNode, VarI1D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer, dimension(:), allocatable, intent(out)                   ::    VarI1D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetI1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (integer)
        allocate( VarI1D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetI2D( This, iNode, VarI2D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    VarI2D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetI2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (integer)
        allocate( VarI2D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetC0D( This, iNode, VarC0D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    character(:), allocatable, intent(out)                            ::    VarC0D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetC0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (character(*))
        VarC0D = Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetC1D( This, iNode, VarC1D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    character(:), dimension(:), allocatable, intent(out)              ::    VarC1D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetC1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (character(*))
        allocate( VarC1D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetC2D( This, iNode, VarC2D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    character(:), dimension(:,:), allocatable, intent(out)            ::    VarC2D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetC2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (character(*))
        allocate( VarC2D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetL0D( This, iNode, VarL0D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    logical, intent(out)                                              ::    VarL0D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetL0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (logical)
        VarL0D = Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetL1D( This, iNode, VarL1D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    logical, dimension(:), allocatable, intent(out)                   ::    VarL1D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetL1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (logical)
        allocate( VarL1D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetL2D( This, iNode, VarL2D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    logical, dimension(:,:), allocatable, intent(out)                 ::    VarL2D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetL2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (logical)
        allocate( VarL2D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetCX0D( This, iNode, VarCX0D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    complex, intent(out)                                              ::    VarCX0D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetCX0D'
    type(ListNode0D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (complex)
        VarCX0D = Value
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetCX1D( This, iNode, VarCX1D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    complex, dimension(:), allocatable, intent(out)                   ::    VarCX1D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetCX1D'
    type(ListNode1D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (complex)
        allocate( VarCX1D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetCX2D( This, iNode, VarCX2D, RemoveFromList, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    complex, dimension(:,:), allocatable, intent(out)                 ::    VarCX2D
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    RemoveFromList
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetCX2D'
    type(ListNode2D_Type), pointer                                    ::    NodePointer_Loc
    logical                                                           ::    RemoveFromList_Loc=.false.

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( present(RemoveFromList) ) RemoveFromList_Loc = RemoveFromList

    call This%GetNode( iNode, NodePointer_Loc )

    select type (Value => NodePointer_Loc%Value)
      type is (complex)
        allocate( VarCX2D, source=Value )
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (RemoveFromList_Loc) then
      call This%DestroyNode( NodePointer_Loc ) 
    else
      nullify(NodePointer_Loc)
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetNode0D( This, iNode, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer(ikp), intent(in)                                          ::    iNode
    type(ListNode0D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetNode0D'

!    call Logger%Entering( ProcName )
!    i_Debug_Loc = i_Debug_Global
!    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

!    if ( iNode < 1 ) call Error%Raise("Requested node index is below 0")
!    if ( iNode > This%NbNodes ) call Error%Raise("Requested node index is above the maximum")

!    call This%MoveBrowser( iNode )

!    select type (Browser => This%Browser)
!      type is (ListNode0D_Type)
!        NodePointer => Browser
!      class default
!        call Error%Raise("Requested node contains a value that does not match the dimension of the requested value")
!    end select

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetNode1D( This, iNode, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer(ikp), intent(in)                                          ::    iNode
    type(ListNode1D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetNode1D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( iNode < 1 ) call Error%Raise("Requested node index is below 0")
    if ( iNode > This%NbNodes ) call Error%Raise("Requested node index is above the maximum")
    
    call This%MoveBrowser( iNode )

    select type (Browser => This%Browser)
      type is (ListNode1D_Type)
        NodePointer => Browser
      class default
        call Error%Raise("Requested node contains a value that does not match the requested type")
    end select

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListGetNode2D( This, iNode, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer(ikp), intent(in)                                          ::    iNode
    type(ListNode2D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListGetNode2D'

!    call Logger%Entering( ProcName )
!    i_Debug_Loc = i_Debug_Global
!    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

!    if ( iNode < 1 ) call Error%Raise("Requested node index is below 0")
!    if ( iNode > This%NbNodes ) call Error%Raise("Requested node index is above the maximum")
!    
!    call This%MoveBrowser( iNode )

!    select type (Browser => This%Browser)
!      type is (ListNode2D_Type)
!        NodePointer => Browser
!      class default
!        call Error%Raise("Requested node contains a value that does not match the requested type")
!    end select

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine MoveBrowser( This, iNode, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    integer(ikp), intent(in)                                          ::    iNode
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='MoveBrowser'
    integer(ikp)                                                      ::    i, i_Max

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( iNode > This%BrowserLoc ) then
      i = 1
      i_Max = iNode - This%BrowserLoc
      do i = 1, i_Max
        This%Browser => This%Browser%Next
        This%BrowserLoc = This%BrowserLoc + 1
      end do
    else
      i = 1
      i_Max = This%BrowserLoc-iNode
      do i = 1, i_Max
        This%Browser => This%Browser%Back
        This%BrowserLoc = This%BrowserLoc - 1
      end do
    end if

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ListLength( This, i_Debug )

    integer(ikp)                                                      ::    ListLength
    class(LinkedList_Type), intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListLength'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    ListLength = This%NbNodes

    if (i_Debug_Loc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!
  
  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ListReset( This, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='ListReset'
    integer                                                           ::    IOLoc

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if (associated(This%Head)) deallocate(This%Head, stat=IOLoc )
    if ( IOLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='This%Head', stat=IOLoc)
    This%NbNodes = 0
    This%BrowserLoc = 0
    nullify(This%Tail)
    nullify(This%Browser)

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine DestroyNode0D( This, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    type(ListNode0D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='DestroyNode0D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( (.not. associated(NodePointer%Next)) .and. (.not. associated(NodePointer%Back)) ) then
      nullify( This%Head )
      nullify( This%Tail )
      nullify( This%Browser )
      This%BrowserLoc = 0
    elseif ( .not. associated(NodePointer%Back) ) then
      This%Head => NodePointer%Next
      if ( This%BrowserLoc == 1 ) This%Browser => This%Head
      nullify(This%Head%Back)
      nullify(NodePointer%Next)
    elseif ( .not. associated(NodePointer%Next) ) then
      This%Tail => NodePointer%Back
      if ( This%BrowserLoc == This%NbNodes ) This%Browser => This%Tail
      nullify(This%Tail%Next)
      nullify(NodePointer%Back)
    else
      This%Browser => NodePointer%Next
      NodePointer%Back%Next => NodePointer%Next
      NodePointer%Next%Back => NodePointer%Back
      nullify(NodePointer%Next)
      nullify(NodePointer%Back)
    end if

    deallocate(NodePointer)

    This%NbNodes = This%NbNodes - 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine DestroyNode1D( This, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    type(ListNode1D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='DestroyNode1D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( (.not. associated(NodePointer%Next)) .and. (.not. associated(NodePointer%Back)) ) then
      nullify( This%Head )
      nullify( This%Tail )
      nullify( This%Browser )
      This%BrowserLoc = 0
    elseif ( .not. associated(NodePointer%Back) ) then
      This%Head => NodePointer%Next
      if ( This%BrowserLoc == 1 ) This%Browser => This%Head
      nullify(This%Head%Back)
      nullify(NodePointer%Next)
    elseif ( .not. associated(NodePointer%Next) ) then
      This%Tail => NodePointer%Back
      if ( This%BrowserLoc == This%NbNodes ) This%Browser => This%Tail
      nullify(This%Tail%Next)
      nullify(NodePointer%Back)
    else
      This%Browser => NodePointer%Next
      NodePointer%Back%Next => NodePointer%Next
      NodePointer%Next%Back => NodePointer%Back
      nullify(NodePointer%Next)
      nullify(NodePointer%Back)
    end if

    deallocate(NodePointer)

    This%NbNodes = This%NbNodes - 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine DestroyNode2D( This, NodePointer, i_Debug )

    class(LinkedList_Type), intent(inout)                             ::    This
    type(ListNode2D_Type), pointer, intent(inout)                     ::    NodePointer
    logical, optional, intent(in)                                     ::    i_Debug

    logical                                                           ::    i_Debug_Loc
    character(*), parameter                                           ::    ProcName='DestroyNode2D'

    call Logger%Entering( ProcName )
    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug

    if ( (.not. associated(NodePointer%Next)) .and. (.not. associated(NodePointer%Back)) ) then
      nullify( This%Head )
      nullify( This%Tail )
      nullify( This%Browser )
      This%BrowserLoc = 0
    elseif ( .not. associated(NodePointer%Back) ) then
      This%Head => NodePointer%Next
      if ( This%BrowserLoc == 1 ) This%Browser => This%Head
      nullify(This%Head%Back)
      nullify(NodePointer%Next)
    elseif ( .not. associated(NodePointer%Next) ) then
      This%Tail => NodePointer%Back
      if ( This%BrowserLoc == This%NbNodes ) This%Browser => This%Tail
      nullify(This%Tail%Next)
      nullify(NodePointer%Back)
    else
      This%Browser => NodePointer%Next
      NodePointer%Back%Next => NodePointer%Next
      NodePointer%Next%Back => NodePointer%Back
      nullify(NodePointer%Next)
      nullify(NodePointer%Back)
    end if

    deallocate(NodePointer)

    This%NbNodes = This%NbNodes - 1

    if (i_Debug_Loc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

!  !!----------------------------------------------------------------------------------------------------------------------------!!
!  recursive subroutine FinalizerNode( This )

!    type(ListNode_Type), intent(inout)                                ::    This
!!write(*,*) 'ok1'
!    if (associated(This%Next)) then
!      select type ( Next => This%Next )
!      type is ( ListNode0D_Type )
!        deallocate(Next)
!      type is ( ListNode1D_Type )
!        deallocate(Next)
!      type is ( ListNode2D_Type )
!        deallocate(Next)
!      class default
!        call Error%Raise( "Something went wrong selecting type of linked list node to deallocate" )
!      end select
!    end if

!    if (associated(This%Back)) nullify(This%Back)

!  end subroutine
!  !!----------------------------------------------------------------------------------------------------------------------------!!

!  !!----------------------------------------------------------------------------------------------------------------------------!!
!  recursive subroutine FinalizerNode0D( This )

!    type(ListNode0D_Type), intent(inout)                              ::    This

!    if (associated(This%Back)) nullify(This%Back)
!    if (allocated(This%Value)) deallocate(This%Value)
!    if (associated(This%Next)) deallocate(This%Next)

!  end subroutine
!  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  recursive subroutine FinalizerNode1D( This )

    type(ListNode1D_Type), intent(inout)                              ::    This

    if (associated(This%Back)) nullify(This%Back)
    if (allocated(This%Value)) deallocate(This%Value)
    if (associated(This%Next)) deallocate(This%Next)

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

!  !!----------------------------------------------------------------------------------------------------------------------------!!
!  recursive subroutine FinalizerNode2D( This )

!    type(ListNode2D_Type), intent(inout)                              ::    This

!    if (associated(This%Back)) nullify(This%Back)
!    if (allocated(This%Value)) deallocate(This%Value)
!    if (associated(This%Next)) deallocate(This%Next)

!  end subroutine
!  !!----------------------------------------------------------------------------------------------------------------------------!!

!  !!----------------------------------------------------------------------------------------------------------------------------!!
!  subroutine FinalizerList( This )

!    type(LinkedList_Type), intent(inout)                              ::    This

!    if (associated(This%Browser)) nullify(This%Browser)
!    if (associated(This%Head)) deallocate(This%Head)
!    if (associated(This%Tail)) nullify(This%Tail)

!  end subroutine
!  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
