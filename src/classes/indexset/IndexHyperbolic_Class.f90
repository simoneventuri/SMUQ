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

module IndexHyperbolic_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use IndexSet_Class                                                ,only:    IndexSet_Type

implicit none

private

public                                                                ::    IndexHyperbolic_Type

type, extends(IndexSet_Type)                                          ::    IndexHyperbolic_Type
  real(rkp)                                                           ::    NormQ=0.4_rkp
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
  generic, public                                                     ::    ComputeNormQ            =>    ComputeNormQ_I1D
  procedure, nopass, private                                          ::    ComputeNormQ_I1D
  procedure, public                                                   ::    GetNormQ
  procedure, public                                                   ::    Copy  
  final                                                               ::    Finalizer     
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(IndexHyperbolic_Type), intent(inout)                        ::    This

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

    class(IndexHyperbolic_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(IndexHyperbolic_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%NormQ = 0.4_rkp

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(IndexHyperbolic_Type), intent(inout)                        ::    This
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

    ParameterName = "normq"
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%NorMQ = VarR0D
    if (This%NormQ <= Zero) call Error%Raise("NormQ specification at or below 0")
    if (This%NormQ > One) call Error%Raise("NormQ specification above maximum of 1")

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, NormQ)

    class(IndexHyperbolic_Type), intent(inout)                        ::    This
    real(rkp), optional, intent(in)                                   ::    NormQ 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(NormQ)) This%NormQ= NormQ
    if (This%NormQ <= Zero) call Error%Raise("NormQ specification at or below 0")
    if (This%NormQ > One) call Error%Raise("NormQ specification above maximum of 1")

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(IndexHyperbolic_Type), intent(in)                           ::    This
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
    call GetInput%AddParameter(Name='normq', Value=ConvertToString(Value=This%NormQ))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices(This, Order, TupleSize, Indices)

    class(IndexHyperbolic_Type), intent(in)                           ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices

    character(*), parameter                                           ::    ProcName='GenerateIndices'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    call This%ComputeIndices(TupleSize, Order, This%NormQ, Indices)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeIndices(This, M, Order, NormQ, Indices)

    class(IndexHyperbolic_Type), intent(in)                           ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    Order
    real(rkp), intent(in)                                             ::    NormQ

    character(*), parameter                                           ::    ProcName='ComputeIndices'
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer, dimension(:,:), allocatable                              ::    Indices_Loc
    integer, dimension(:), allocatable                                ::    VarI1D
    integer, dimension(:,:), allocatable                              ::    VarI2D
    integer, dimension(:,:), allocatable                              ::    IdentityM
    real(rkp), dimension(:), allocatable                              ::    VarR1D
    integer                                                           ::    i, ii
    integer                                                           ::    k, j
    integer                                                           ::    jmax
    integer                                                           ::    NbPermutations, NbIndices
    logical, dimension(:), allocatable                                ::    LogicMask
    integer                                                           ::    StatLoc=0

    if (M <= 0) call Error%Raise(Line="Specified tuple size at or below 0", ProcName=ProcName)

    allocate(IndicesRecord, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

    allocate(VarR1D(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='VarR1D', stat=StatLoc)
    VarR1D = Zero

    allocate(IdentityM(M,M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='IdentityM', stat=StatLoc)
    IdentityM = EyeI(M)

    allocate(VarI1D(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='VarI1D', stat=StatLoc)
    VarI1D = 0

    call IndicesRecord%Append(VarI1D)

    do k = 1, Order
      jmax = min(M,k)

      do j = 1, jmax

        if (j == 1) then

          ii = 1
          do ii = 1, M
            call IndicesRecord%Append(k*IdentityM(:,ii))
          end do

          cycle
        end if 
        
        call This%AlgorithmH(M, k, j, Indices_Loc)

        NbPermutations = size(Indices_Loc,2)

        StatLoc = 0
        if (allocated(LogicMask)) deallocate(LogicMask, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='LogicMask', stat=StatLoc)
        allocate(LogicMask(NbPermutations), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='LogicMask', stat=StatLoc)
        LogicMask = .true.

        i = 1
        do i = 1, NbPermutations
          if (This%ComputeNormQ(Indices_Loc(:,i), NormQ) > Order) LogicMask(i) = .false.
        end do

        i = 1
        do i = 1, NbPermutations
          if (.not. LogicMask(i)) cycle
          VarR1D = real(Indices_Loc(:,i),rkp)
          call dlasrt('I', M, VarR1D, StatLoc)
          if (StatLoc /= 0) call Error%Raise("Something went wrong with lapack routine dlasrt")
          VarI1D = nint(VarR1D)
          call This%AlgorithmL(VarI1D, M, VarI2D)

          NbIndices = size(VarI2D,2)
          ii = 1
          do ii = 1, NbIndices
            call IndicesRecord%Append(VarI2D(:,ii))
          end do

        end do

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
    if (allocated(LogicMask)) deallocate(LogicMask, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='LogicMask', stat=StatLoc)

    NbIndices = IndicesRecord%GetLength()
    allocate(Indices(M,NbIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(ProcName=ProcName, Name='Indices', stat=StatLoc)

    i = 1
    do i = 1, NbIndices
      call IndicesRecord%Get(i, VarI1D)
      Indices(:,i) = VarI1D
    end do

    deallocate(IndicesRecord, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(ProcName=ProcName, Name='IndicesRecord', stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function  ComputeNormQ_I1D(Vector, NormQ)

    real(rkp)                                                         ::    ComputeNormQ_I1D
    integer, dimension(:), intent(in)                                 ::    Vector
    real(rkp), intent(in)                                             ::    NormQ

    character(*), parameter                                           ::    ProcName='ComputeNormQ_I1D'

    ComputeNormQ_I1D = sum(Vector**NormQ)
    ComputeNormQ_I1D = ComputeNormQ_I1D**(One/NormQ)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNormQ(This)

    real(rkp)                                                         ::    GetNormQ
    class(IndexHyperbolic_Type), intent(in)                           ::    This

    character(*), parameter                                           ::    ProcName='GetNormQ'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetNormQ = This%NormQ

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(IndexHyperbolic_Type), intent(out)                          ::    LHS
    class(IndexSet_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (IndexHyperbolic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%NormQ = RHS%NormQ
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(IndexHyperbolic_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module

