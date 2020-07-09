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

module IndexLowOrder_Class

use Input_Library
use Parameters_Library
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use IndexSet_Class                                                ,only:    IndexSet_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    IndexLowOrder_Type

type, extends(IndexSet_Type)                                          ::    IndexLowOrder_Type
  integer                                                             ::    Norm0=1000
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
  procedure, public                                                   ::    GetNorm0
  procedure, public                                                   ::    Copy 
  final                                                               ::    Finalizer     
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(IndexLowOrder_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name         =   'hyperbolic'
      This%Initialized  =   .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(IndexLowOrder_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(IndexLowOrder_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Norm0 = 1000

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(IndexLowOrder_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = "norm0"
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Norm0 = VarI0D
    if (This%Norm0 < 1) call Error%Raise("Norm0 specification below the minimum of 1")

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Norm0)

    class(IndexLowOrder_Type), intent(inout)                          ::    This
    integer, intent(in)                                               ::    Norm0   

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (Norm0 < 1) call Error%Raise("Norm0 specification below the minimum of 1")
    This%Norm0 = Norm0

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringConversion_Module

    type(InputSection_Type)                                           ::    GetInput
    class(IndexLowOrder_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    call GetInput%AddParameter(Name='norm0', Value=ConvertToString(Value=This%Norm0))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices(This, Order, TupleSize, Indices)

    class(IndexLowOrder_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices

    character(*), parameter                                           ::    ProcName='GenerateIndices'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (TupleSize < This%Norm0) call Error%Raise("Requested MTuples of length TupleSize which is less than the Norm0")
    call This%ComputeIndices(TupleSize, Order, This%Norm0, Indices)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeIndices(This, M, Order, Norm0, Indices)

    class(IndexLowOrder_Type), intent(in)                             ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    Norm0

    character(*), parameter                                           ::    ProcName='ComputeIndices'
    integer, dimension(:,:), allocatable                              ::    Indices_Loc
    integer, dimension(:), allocatable                                ::    VarI1D
    integer, dimension(:,:), allocatable                              ::    VarI2D
    real(rkp), dimension(:), allocatable                              ::    VarR1D
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer                                                           ::    i, ii
    integer                                                           ::    NbPermutations, NbIndices
    integer                                                           ::    StatLoc=0

    allocate(IndicesRecord, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    allocate(VarR1D(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='VarR1D', stat=StatLoc)
    VarR1D = Zero

    allocate(VarI1D(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='VarI1D', stat=StatLoc)
    VarI1D = 0

    call This%AlgorithmH(M, Order, Norm0, Indices_Loc)

    NbPermutations = size(Indices_Loc,2)

    i = 1
    do i = 1, NbPermutations
      VarR1D = real(Indices_Loc(:,i),rkp)
      call dlasrt('I', M, VarR1D, StatLoc)
      if (StatLoc /= 0) call Error%Raise("Something went wrong with lapack routine dlasrt")
      VarI1D = nint(VarR1D)
  
      call This%AlgorithmL(VarI1D, M, VarI2D)

      ii = 1
      NbIndices = size(VarI2D,2)
      do ii = 1, NbIndices
        VarI1D = VarI2D(:,ii)
        call IndicesRecord%Append(VarI1D)
      end do

    end do

    StatLoc = 0
    if (allocated(Indices_Loc)) deallocate(Indices_Loc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='Indices_Loc', stat=StatLoc)
    if (allocated(VarI1D)) deallocate(VarI1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='VarI1D', stat=StatLoc)
    if (allocated(VarI2D)) deallocate(VarI2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='VarI2D', stat=StatLoc)
    if (allocated(VarR1D)) deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='VarR1D', stat=StatLoc)

    NbIndices = IndicesRecord%GetLength()
    allocate(Indices(M, NbIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='Indices', stat=StatLoc)
    i = 1
    do i = 1, NbIndices
      call IndicesRecord%Get(i, VarI1D)
      Indices(:,i) = VarI1D
    end do

    deallocate(IndicesRecord, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='VarR1D', stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNorm0(This)

    integer                                                           ::    GetNorm0
    class(IndexLowOrder_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetNorm0'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetNorm0 = This%Norm0

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(IndexLowOrder_Type), intent(out)                            ::    LHS
    class(IndexSet_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (IndexLowOrder_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%Norm0 = RHS%Norm0
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(IndexLowOrder_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module

