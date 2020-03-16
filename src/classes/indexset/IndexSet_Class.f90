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

module IndexSet_Class

use Input_Library
use Parameters_Library
use String_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    IndexSet_Type

type, abstract                                                        ::    IndexSet_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, nopass, public                                           ::    AlgorithmH
  procedure, nopass, public                                           ::    AlgorithmL
  procedure, public                                                   ::    IsConstructed
  procedure(Initialize_IndexSet), deferred, public                    ::    Initialize
  procedure(Reset_IndexSet), deferred, public                         ::    Reset
  procedure(ConstructInput_IndexSet), deferred, private               ::    ConstructInput
  procedure(GetInput_IndexSet), deferred, public                      ::    GetInput
  procedure(GenerateIndices_IndexSet), deferred, public               ::    GenerateIndices
  procedure(SetDefaults_IndexSet), deferred, public                   ::    SetDefaults
  procedure(Copy_IndexSet), deferred, public                          ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_IndexSet( This )
    import                                                            ::    IndexSet_Type
    class(IndexSet_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_IndexSet( This )
    import                                                            ::    IndexSet_Type
    class(IndexSet_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_IndexSet( This, Input, Prefix )
    import                                                            ::    IndexSet_Type
    import                                                            ::    InputSection_Type
    class(IndexSet_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_IndexSet( This )
    import                                                            ::    IndexSet_Type
    class(IndexSet_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_IndexSet( This, MainSectionName, Prefix, Directory )
    import                                                            ::    IndexSet_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_IndexSet
    class(IndexSet_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices_IndexSet( This, Order, TupleSize, Indices )
    import                                                            ::    IndexSet_Type
    class(IndexSet_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_IndexSet( LHS, RHS )
    import                                                            ::    IndexSet_Type
    class(IndexSet_Type), intent(out)                                 ::    LHS
    class(IndexSet_Type), intent(in)                                  ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine algorithmH( M, p, j, Indices )

    use LinkedList1D_Class                                        ,only:    LinkedList1D_Type

    integer, dimension(:,:), allocatable, intent(inout)               ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    p
    integer, intent(in)                                               ::    j

    character(*), parameter                                           ::    ProcName='algorithmH'
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer                                                           ::    i
    integer                                                           ::    NbIndices
    integer                                                           ::    k, s, x
    integer, dimension(:), allocatable                                ::    al, VarI1D
    integer                                                           ::    StatLoc=0


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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine algorithmL( MTuple, M, Indices )
    
    use LinkedList1D_Class                                        ,only:    LinkedList1D_Type

    integer, dimension(:,:), allocatable, intent(inout)               ::    Indices
    integer, dimension(:), intent(in)                                 ::    MTuple
    integer, intent(in)                                               ::    M

    character(*), parameter                                           ::    ProcName='algorithmL'
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer, dimension(:), allocatable                                ::    al
    integer                                                           ::    NbIndices
    integer                                                           ::    i
    integer                                                           ::    k, l, r
    integer                                                           ::    VarI0D
    integer, dimension(:), allocatable                                ::    VarI1D
    integer                                                           ::    StatLoc=0

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This )

    logical                                                           ::    IsConstructed

    character(*), parameter                                           ::    ProcName='IsConstructed'
    integer                                                           ::    StatLoc=0

    IsConstructed = This%Constructed

  end function
  !!------------------------------------------------------------------------------------------------------------------------------
end module
