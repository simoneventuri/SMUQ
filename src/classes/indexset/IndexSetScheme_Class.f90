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

module IndexSetScheme_Class

use Input_Library
use Parameters_Library
use String_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    IndexSetScheme_Type

type, abstract                                                        ::    IndexSetScheme_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    Order=1
  integer                                                             ::    MinOrder=1
  integer                                                             ::    MaxOrder=huge(1)
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    GetOrder
  procedure, public                                                   ::    GetMaxOrder
  procedure, public                                                   ::    GetMinOrder
  procedure, nopass, public                                           ::    AlgorithmH
  procedure, nopass, public                                           ::    AlgorithmL
  procedure(Initialize_IndexSetScheme), deferred, public              ::    Initialize
  procedure(Reset_IndexSetScheme), deferred, public                   ::    Reset
  procedure(ConstructInput_IndexSetScheme), deferred, private         ::    ConstructInput
  procedure(GetInput_IndexSetScheme), deferred, public                ::    GetInput
  procedure(GenerateIndices_IndexSetScheme), deferred, public         ::    GenerateIndices
  procedure(SetDefaults_IndexSetScheme), deferred, public             ::    SetDefaults
  procedure(Copy_IndexSetScheme), deferred, public                    ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_IndexSetScheme( This, Debug )
    import                                                            ::    IndexSetScheme_Type
    class(IndexSetScheme_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_IndexSetScheme( This, Debug )
    import                                                            ::    IndexSetScheme_Type
    class(IndexSetScheme_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_IndexSetScheme( This, Input, Prefix, Debug )
    import                                                            ::    IndexSetScheme_Type
    import                                                            ::    InputSection_Type
    class(IndexSetScheme_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_IndexSetScheme( This, Debug )
    import                                                            ::    IndexSetScheme_Type
    class(IndexSetScheme_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_IndexSetScheme( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    IndexSetScheme_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_IndexSetScheme
    class(IndexSetScheme_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices_IndexSetScheme( This, Order, TupleSize, Indices, OrderError, OrderExceeded, Debug )
    import                                                            ::    IndexSetScheme_Type
    class(IndexSetScheme_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    logical, optional, intent(in)                                     ::    OrderError
    logical, optional, intent(out)                                    ::    OrderExceeded
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_IndexSetScheme( LHS, RHS )
    import                                                            ::    IndexSetScheme_Type
    class(IndexSetScheme_Type), intent(out)                           ::    LHS
    class(IndexSetScheme_Type), intent(in)                            ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOrder( This, Debug )

    integer                                                           ::    GetOrder
    class(IndexSetScheme_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOrder'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetOrder = This%Order

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMinOrder( This, Debug )

    integer                                                           ::    GetMinOrder
    class(IndexSetScheme_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMinOrder'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetMinOrder = This%MinOrder

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMaxOrder( This, Debug )

    integer                                                           ::    GetMaxOrder
    class(IndexSetScheme_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMaxOrder'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetMaxOrder = This%MaxOrder

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine algorithmH( M, p, j, Indices, Debug )

    use LinkedList1D_Class                                        ,only:    LinkedList1D_Type

    integer, dimension(:,:), allocatable, intent(inout)               ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    p
    integer, intent(in)                                               ::    j
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='algorithmH'
    logical                                                           ::    DebugLoc
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer(8)                                                        ::    i
    integer(8)                                                        ::    NbIndices
    integer                                                           ::    k, s, x
    integer, dimension(:), allocatable                                ::    al, VarI1D
    integer                                                           ::    StatLoc=0


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(Indices) ) deallocate(Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Indices', stat=StatLoc)

    allocate(IndicesRecord)
    allocate( al(j+1) )
    al(1) = p-j+1
    al(2:j) = 1
    al(j+1) = -1 
    do

      do
        call IndicesRecord%Append( al(1:j) )
        if ( al(2) >= al(1)-1 ) exit
        al(1) = al(1) - 1
        al(2) = al(2) + 1
      end do

      k = 3
      s = al(1) + al(2) - 1
      
      do
        if ( al(k) < (al(1) - 1) ) exit
        s = s + al(k)
        k = k + 1
      end do

      if ( k > j ) exit
      x = al(k) + 1
      al(k) = x
      k = k - 1

      do 
        if ( k <= 1 ) exit
        al(k) = x
        s = s - x
        k = k - 1
        al(1) = s
      end do

    end do

    deallocate( al )

    NbIndices = IndicesRecord%GetLength()
    allocate( Indices( M, NbIndices ), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='Indices', stat=StatLoc)
    Indices = 0

    i = 1
    do i = 1, NbIndices
      call IndicesRecord%Get( i, VarI1D )
      Indices( M - j + 1: M, i ) = VarI1D
    end do

    deallocate(VarI1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarI1D', stat=StatLoc)

    deallocate(IndicesRecord)

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine algorithmL( MTuple, M, Indices, Debug )
    
    use LinkedList1D_Class                                        ,only:    LinkedList1D_Type

    integer, dimension(:,:), allocatable, intent(inout)               ::    Indices
    integer, dimension(:), intent(in)                                 ::    MTuple
    integer, intent(in)                                               ::    M
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='algorithmL'
    logical                                                           ::    DebugLoc
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer, dimension(:), allocatable                                ::    al
    integer(8)                                                        ::    NbIndices
    integer(8)                                                        ::    i
    integer                                                           ::    k, l, r
    integer                                                           ::    VarI0D
    integer, dimension(:), allocatable                                ::    VarI1D
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(Indices) ) deallocate(Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Indices', stat=StatLoc)

    allocate( IndicesRecord, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    allocate( al, source=MTuple, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='al', stat=StatLoc)

    do

      call IndicesRecord%Append(al)
      k = M - 1

      if (  k == 0 ) exit
      do
        if ( al(k) < al(k+1) ) exit
        k = k - 1
        if ( k == 0 ) exit
      end do
      if (  k == 0 ) exit

      l = M
      do
        if ( al(k) < al(l) ) exit
        l = l - 1
      end do

      VarI0D = al(k)
      al(k) = al(l)
      al(l) = VarI0D

      r = k + 1
      l = M

      do
        if ( r >= l) exit
        VarI0D = al(r)
        al(r) = al(l)
        al(l) = VarI0D
        r = r + 1
        l = l - 1
      end do

    end do

    deallocate(al, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    NbIndices = IndicesRecord%GetLength()
    allocate( Indices( M, NbIndices ), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='Indices', stat=StatLoc)
    Indices = 0

    i = 1
    do i = 1, NbIndices
      call IndicesRecord%Get( i, VarI1D )
      Indices(:,i) = VarI1D
    end do

    deallocate(VarI1D, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarI1D', stat=StatLoc)

    deallocate(IndicesRecord, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
