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

module IndexStandard_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use IndexSet_Class                                                ,only:    IndexSet_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    IndexStandard_Type

type, extends(IndexSet_Type)                                          ::    IndexStandard_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GenerateIndices
  procedure, public                                                   ::    ComputeIndices
  procedure, public                                                   ::    Copy     
  final                                                               ::    Finalizer     
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(IndexStandard_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name         =   'standard'
      This%Initialized  =   .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(IndexStandard_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(IndexStandard_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(IndexStandard_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This )

    class(IndexStandard_Type), intent(inout)                          ::    This  

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize() 

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(IndexStandard_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices( This, Order, TupleSize, Indices, OrderError, OrderExceeded )

    class(IndexStandard_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices

    character(*), parameter                                           ::    ProcName='GenerateIndices'

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    call This%ComputeIndices( TupleSize, Order, Indices )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeIndices( This, M, Order, Indices )

    class(IndexStandard_Type), intent(in)                             ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    Order

    character(*), parameter                                           ::    ProcName='ComputeIndices'
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer                                                           ::    i, ii
    integer                                                           ::    k, j
    integer                                                           ::    jmax
    integer                                                           ::    NbIndices, NbPermutations
    integer, dimension(:,:), allocatable                              ::    Indices_Loc
    integer, dimension(:), allocatable                                ::    VarI1D
    integer, dimension(:,:), allocatable                              ::    VarI2D
    integer, dimension(:,:), allocatable                              ::    IdentityM
    real(rkp), dimension(:), allocatable                              ::    VarR1D
    integer                                                           ::    StatLoc=0

    allocate( IndicesRecord, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    allocate( VarR1D(M), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='VarR1D', stat=StatLoc)
    VarR1D = Zero

    allocate( IdentityM(M,M), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='IdentityM', stat=StatLoc)
    IdentityM = EyeI( M )

    allocate( VarI1D(M), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='VarI1D', stat=StatLoc)
    VarI1D = 0

    call IndicesRecord%Append( VarI1D )

    do k = 1, Order

      jmax = min(M,k)

      do j = 1, jmax

        if ( j == 1 ) then

          ii = 1
          do ii = 1, M
            call IndicesRecord%Append( k*IdentityM(:,ii) )
          end do

          cycle
        end if 
        
        call This%AlgorithmH( M, k, j, Indices_Loc )

        NbPermutations = size(Indices_Loc,2)

        i = 1
        do i = 1, NbPermutations
          VarR1D = real(Indices_Loc(:,i),rkp)
          call dlasrt( 'I', M, VarR1D, StatLoc )
          if ( StatLoc /= 0 ) call Error%Raise("Something went wrong with lapack routine dlasrt")
          VarI1D = nint(VarR1D)

          call This%AlgorithmL( VarI1D, M, VarI2D )

          NbIndices = size(VarI2D,2)

          ii = 1
          do ii = 1, NbIndices
            call IndicesRecord%Append( VarI2D(:,ii) )
          end do

        end do

      end do

    end do

    StatLoc = 0
    if (allocated(Indices_Loc)) deallocate(Indices_Loc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='Indices_Loc', stat=StatLoc)
    if (allocated(VarI1D)) deallocate(VarI1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarI1D', stat=StatLoc)
    if (allocated(VarI2D)) deallocate(VarI2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarI2D', stat=StatLoc)
    if (allocated(VarR1D)) deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='VarR1D', stat=StatLoc)

    NbIndices = IndicesRecord%GetLength()

    allocate( Indices(M,NbIndices), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='Indices', stat=StatLoc)

    i = 1
    do i = 1, NbIndices
      call IndicesRecord%Get( i, VarI1D )
      Indices(:,i) = VarI1D
    end do

    deallocate(IndicesRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(IndexStandard_Type), intent(out)                            ::    LHS
    class(IndexSet_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (IndexStandard_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then

        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(IndexStandard_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module

