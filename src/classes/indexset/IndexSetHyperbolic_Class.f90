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

module IndexSetHyperbolic_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type

implicit none

private

public                                                                ::    IndexSetHyperbolic_Type

type, extends(IndexSetScheme_Type)                                    ::    IndexSetHyperbolic_Type
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
  subroutine Initialize( This, Debug )

    class(IndexSetHyperbolic_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Write( "Initializing IndexSetHyperbolic object" )

    if ( .not. This%Initialized ) then
      This%Name         =   'hyperbolic'
      This%Initialized  =   .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Write( "Initialization Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(IndexSetHyperbolic_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Write( "Reseting IndexSetHyperbolic object" )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Write( "Reset Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(IndexSetHyperbolic_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Order = 1
    This%MaxOrder = huge(1)
    This%MinOrder = 1
    This%NormQ = 0.4_rkp

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(IndexSetHyperbolic_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = "min_order"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MinOrder = VarI0D

    ParameterName = "max_order"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MaxOrder = VarI0D

    if ( This%MinOrder < 0 ) call Error%Raise( Line='Min order set lower than 0', ProcName=ProcName )
    if ( This%MaxOrder < This%MinOrder ) call Error%Raise( Line='Max Order set lower than min order', ProcName=ProcName )

    ParameterName = "order"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Order = VarI0D
    if ( This%Order > This%MaxOrder .or. This%Order < This%MinOrder ) call Error%Raise(                                           &
                                                               Line='Specified order that exceeds min or max', ProcName=ProcName )  

    ParameterName = "normq"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%NorMQ = VarR0D
    if ( This%NormQ <= Zero ) call Error%Raise( "NormQ specification at or below 0" )
    if ( This%NormQ > One ) call Error%Raise( "NormQ specification above maximum of 1" )

    This%Constructed = .true.
      
    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, NormQ, Order, MinOrder, MaxOrder, Debug )

    class(IndexSetHyperbolic_Type), intent(inout)                     ::    This
    real(rkp), optional, intent(in)                                   ::    NormQ
    integer, optional, intent(in)                                     ::    Order
    integer, optional, intent(in)                                     ::    MinOrder
    integer, optional, intent(in)                                     ::    MaxOrder    
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(NormQ) ) This%NormQ= NormQ
    if ( This%NormQ <= Zero ) call Error%Raise( "NormQ specification at or below 0" )
    if ( This%NormQ > One ) call Error%Raise( "NormQ specification above maximum of 1" )

    if ( present(MinOrder) ) This%MinOrder = MinOrder

    if ( present(MaxOrder) ) This%MaxOrder = MaxOrder

    if ( This%MinOrder < 0 ) call Error%Raise( Line='Min order set lower than 0', ProcName=ProcName )
    if ( This%MaxOrder < This%MinOrder ) call Error%Raise( Line='Max Order set lower than min order', ProcName=ProcName )

    if ( present(Order) ) This%Order = Order
    if ( This%Order > This%MaxOrder .or. This%Order < This%MinOrder ) call Error%Raise(                                           &
                                                               Line='Specified order that exceeds min or max', ProcName=ProcName ) 

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(IndexSetHyperbolic_Type), intent(in)                        ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='order', Value=ConvertToString(Value=This%Order) )
    call GetInput%AddParameter( Name='min_order', Value=ConvertToString(Value=This%MinOrder) )
    call GetInput%AddParameter( Name='max_order', Value=ConvertToString(Value=This%MaxOrder) )
    call GetInput%AddParameter( Name='normq', Value=ConvertToString(Value=This%NormQ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateIndices( This, Order, TupleSize, Indices, OrderError, OrderExceeded, Debug )

    class(IndexSetHyperbolic_Type), intent(in)                        ::    This
    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    TupleSize
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    logical, optional, intent(in)                                     ::    OrderError
    logical, optional, intent(out)                                    ::    OrderExceeded
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GenerateIndices'
    logical                                                           ::    OrderExceededLoc=.false.
    logical                                                           ::    OrderErrorLoc=.true.

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    OrderExceededLoc = .false.
    OrderErrorLoc = .true.
    if ( present(OrderError) ) OrderErrorLoc = OrderError

    if ( Order > This%MaxOrder .or. Order < This%MinOrder ) then
      if ( OrderErrorLoc ) call Error%Raise( Line='Order exceeded', ProcName=ProcName )
      OrderExceededLoc = .true.
    else
      call This%ComputeIndices( TupleSize, Order, This%NormQ, Indices )
    end if

    if ( present(OrderExceeded) ) OrderExceeded = OrderExceededLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeIndices( This, M, Order, NormQ, Indices, Debug  )

    use ArrayRoutines_Module
    use LinkedList1D_Class                                        ,only:  LinkedList1D_Type

    class(IndexSetHyperbolic_Type), intent(in)                        ::    This
    integer, dimension(:,:), allocatable, intent(out)                 ::    Indices
    integer, intent(in)                                               ::    M
    integer, intent(in)                                               ::    Order
    real(rkp), intent(in)                                             ::    NormQ
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeIndices'
    type(LinkedList1D_Type), allocatable                              ::    IndicesRecord
    integer, dimension(:,:), allocatable                              ::    Indices_Loc
    integer, dimension(:), allocatable                                ::    VarI1D
    integer, dimension(:,:), allocatable                              ::    VarI2D
    integer, dimension(:,:), allocatable                              ::    IdentityM
    real(rkp), dimension(:), allocatable                              ::    VarR1D
    integer(8)                                                        ::    i, ii
    integer                                                           ::    k, j
    integer                                                           ::    jmax
    integer(8)                                                        ::    NbPermutations, NbIndices
    logical, dimension(:), allocatable                                ::    LogicMask
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( M <= 0 ) call Error%Raise(Line="Specified tuple size at or below 0", ProcName=ProcName)

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
      jmax = min(M,k,8)

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

        StatLoc = 0
        if ( allocated(LogicMask) ) deallocate(LogicMask, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='LogicMask', stat=StatLoc)
        allocate( LogicMask(NbPermutations), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='LogicMask', stat=StatLoc)
        LogicMask = .true.

        i = 1
        do i = 1, NbPermutations
          if ( This%ComputeNormQ(Indices_Loc(:,i), NormQ) > Order ) LogicMask(i) = .false.
        end do

        i = 1
        do i = 1, NbPermutations
          if ( .not. LogicMask(i) ) cycle
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
    if ( allocated(LogicMask) ) deallocate(LogicMask, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( ProcName=ProcName, Name='LogicMask', stat=StatLoc)

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function  ComputeNormQ_I1D( Vector, NormQ, Debug )

    real(rkp)                                                         ::    ComputeNormQ_I1D
    integer, dimension(:), intent(in)                                 ::    Vector
    real(rkp), intent(in)                                             ::    NormQ
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeNormQ_I1D'
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ComputeNormQ_I1D = sum( Vector**NormQ )
    ComputeNormQ_I1D = ComputeNormQ_I1D**(One/NormQ)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNormQ( This, Debug )

    real(rkp)                                                         ::    GetNormQ
    class(IndexSetHyperbolic_Type), intent(in)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNormQ'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNormQ = This%NormQ

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(IndexSetHyperbolic_Type), intent(out)                       ::    LHS
    class(IndexSetScheme_Type), intent(in)                            ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)

      type is (IndexSetHyperbolic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Order = RHS%Order
          LHS%MinOrder = RHS%MinOrder
          LHS%MaxOrder = RHS%MaxOrder
          LHS%NormQ = RHS%NormQ
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(IndexSetHyperbolic_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module

