module ArrayRoutines_Module

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    Eye
public                                                                ::    EyeI
public                                                                ::    EyeR
public                                                                ::    StrictTriangular
public                                                                ::    StrictTriangularI
public                                                                ::    StrictTriangularR
public                                                                ::    Diagonal
public                                                                ::    IsDiagonal
public                                                                ::    LinSpaceVec

logical, parameter                                                    ::    DebugGlobal = .false.

interface IsDiagonal
  module procedure                                                    ::    IsDiagonal_Real8
end interface

interface LinSpaceVec
  module procedure                                                    ::    LinSpaceVecInput_R1D
  module procedure                                                    ::    LinSpaceVec_R1D
end interface

interface Eye
  module procedure                                                    ::    Eye_R42D
  module procedure                                                    ::    Eye_R82D
  module procedure                                                    ::    Eye_I42D
  module procedure                                                    ::    Eye_I82D
end interface

interface EyeI
  module procedure                                                    ::    EyeI_I42D
  module procedure                                                    ::    EyeI_I82D
end interface

interface EyeR
  module procedure                                                    ::    EyeR_R42D
  module procedure                                                    ::    EyeR_R82D
end interface

interface StrictTriangular
  module procedure                                                    ::    StrictTriangular_I42D
  module procedure                                                    ::    StrictTriangular_I82D
  module procedure                                                    ::    StrictTriangular_R42D
  module procedure                                                    ::    StrictTriangular_R82D
end interface

interface StrictTriangularI
  module procedure                                                    ::    StrictTriangularI_I42D
  module procedure                                                    ::    StrictTriangularI_I82D
end interface

interface StrictTriangularR
  module procedure                                                    ::    StrictTriangularR_R42D
  module procedure                                                    ::    StrictTriangularR_R82D
end interface

interface Diagonal
  module procedure                                                    ::    Diagonal_I42D
  module procedure                                                    ::    Diagonal_I82D
  module procedure                                                    ::    Diagonal_R42D
  module procedure                                                    ::    Diagonal_R82D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsDiagonal_Real8( Array )

    logical                                                           ::    IsDiagonal_Real8

    real(rkp), dimension(:,:), intent(in)                             ::    Array

    character(*), parameter                                           ::    ProcName='IsDiagonal_Real8'
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i, ii

    M = size(Array,1)
    N = size(Array,2)

    IsDiagonal_Real8 = .true.

    if ( M /= N ) then
      IsDiagonal_Real8 = .false.
    else if ( M == 1 ) then
      IsDiagonal_Real8 = .true.
    else
      do i = 2, M
        do ii = 1, i-1
          if ( Array(i,ii) > precision(Array) .or. Array(ii,i) > precision(Array) ) then
            IsDiagonal_Real8 = .false.
            exit
          end if
        end do
       if ( .not. IsDiagonal_Real8 ) exit
      end do     
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSpaceVecInput_R1D( Input )

    real(rkp), allocatable, dimension(:)                              ::    LinSpaceVecInput_R1D

    class(InputSection_Type), intent(inout)                           ::    Input

    character(*), parameter                                           ::    ProcName='LinSpaceVecInput_R1D'
    real(rkp), dimension(:,:), allocatable                            ::    InterExtremes
    integer, dimension(:), allocatable                                ::    InterNbNodes
    character(20), dimension(:), allocatable                          ::    InterGenFun
    integer                                                           ::    NbInter
    integer                                                           ::    i
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    integer                                                           ::    StatLoc=0

    NbInter = Input%GetNumberOfSubSections()

    allocate( InterExtremes(NbInter,2), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterExtremes', ProcName=ProcName, stat=StatLoc )
    allocate( InterNbNodes(NbInter), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterNbNodes', ProcName=ProcName, stat=StatLoc )
    allocate( InterGenFun(NbInter), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterGenFun', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbInter
      SubSectionName = 'interval' // ConvertToString(Value=i)

      ParameterName = 'min'
      call Input%GetValue( ParameterName=ParameterName, Value=VarR0D, SectionName=SubSectionName, Mandatory=.true. )
      InterExtremes(i,1) = VarR0D

      ParameterName = 'max'
      call Input%GetValue( ParameterName=ParameterName, Value=VarR0D, SectionName=SubSectionName, Mandatory=.true. )
      InterExtremes(i,2) = VarR0D

      ParameterName = 'nb_nodes'
      call Input%GetValue( ParameterName=ParameterName, Value=VarI0D, SectionName=SubSectionName, Mandatory=.true. )
      InterNbNodes(i) = VarI0D

      ParameterName = 'spacing'
      InterGenFun(i) = 'linear'
      call Input%GetValue( ParameterName=ParameterName, Value=VarC0D, SectionName=SubSectionName, Mandatory=.false., Found=Found )      
      if ( Found ) InterGenFun(i) = VarC0D
    end do

    LinSpaceVecInput_R1D = LinSpaceVec( NbInter, InterExtremes, InterNbNodes, InterGenFun )

    deallocate( InterExtremes, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterExtremes', ProcName=ProcName, stat=StatLoc )
    deallocate( InterNbNodes, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterNbNodes', ProcName=ProcName, stat=StatLoc )
    deallocate( InterGenFun, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterGenFun', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSpaceVec_R1D( NbInter, InterExtremes, InterNbNodes, InterGenFun )

    use ComputingRoutines_Module

    real(rkp), dimension(:), allocatable                              ::    LinSpaceVec_R1D

    integer, intent(in)                                               ::    NbInter
    real(rkp), dimension(:,:), intent(in)                             ::    InterExtremes
    integer, dimension(:), intent(in)                                 ::    InterNbNodes
    character(*), dimension(:), optional, intent(in)                  ::    InterGenFun


    character(*), parameter                                           ::    ProcName='LinSpaceVec_R1D'
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbNodes
    integer                                                           ::    i
    integer                                                           ::    i_start
    integer                                                           ::    i_end
    integer                                                           ::    ii
    integer                                                           ::    iim1
    integer                                                           ::    StatLoc=0

    if ( NbInter <= 0 ) call Error%Raise( 'Specified number of intervals is invalid ( 0 or lower)' )
    if ( NbInter /= size(InterExtremes,1) ) call Error%Raise( 'Number of intervals does not match size of InterExtremes' )
    if ( NbInter /= size(InterNbNodes,1) ) call Error%Raise( 'Number of intervals does not match size of InternbNodes' )
    if ( .not. any(InterNbNodes > 0) ) call Error%Raise( Line='Zero nodes specified for all intervals', ProcName=ProcName )
    if ( present(InterGenFun) ) then
      if ( NbInter /= size(InterGenFun,1) ) call Error%Raise( 'Number of intervals does not match size of InterGenFun' )
    end if

    NbNodes = sum(InterNbNodes)

    allocate( LinSpaceVec_R1D(NbNodes), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='LinSpaceVec_R1D', stat=StatLoc )
    LinSpaceVec_R1D = Zero

    ii = 0
    iim1 = 0
    i = 1
    do i = 1, NbInter
      if( InterNbNodes(i) <= 0 ) cycle

      iim1 = ii
      ii = ii + InterNbNodes(i)

      if ( .not. present(InterGenFun) ) then
        VarC0D = 'linear'
      else
        VarC0D = trim(adjustl(InterGenFun(i)))
      end if

      select case ( VarC0D )
        case ( 'linear' )
          LinSpaceVec_R1D(iim1+1:ii) = LinSpace( InterMin=InterExtremes(i,1), InterMax=InterExtremes(i,2),                        &
                                                                                                         NbNodes=InterNbNodes(i) )  
        case ( 'log10' )
          if ( InterExtremes(i,1) <= 0 ) call Error%Raise( "Minimum interval bound below zero for the log10 spacing" )
          if ( InterExtremes(i,2) <= 0 ) call Error%Raise( "Maximum interval bound below zero for the log10 spacing" )

          LinSpaceVec_R1D(iim1+1:ii) = Log10Space( InterMin=dlog10(InterExtremes(i,1)), InterMax=dlog10(InterExtremes(i,2)),      &
                                                                                                         NbNodes=InterNbNodes(i) )
        case default
          call Error%Raise( Line='Unrecognized spacing scheme: ' // VarC0D, ProcName=ProcName )
      end select

    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  function StrictTriangularI_I42D( M, N, UL )

    integer(4), dimension(:,:), allocatable                           ::    StrictTriangularI_I42D

    integer(4), intent(in)                                            ::    M
    integer(4), optional, intent(in)                                  ::    N
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangularI_I42D'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i
    integer(4)                                                        ::    imax
    integer(4)                                                        ::    NLoc

    NLoc = M
    if ( present(N) ) NLoc = N

    allocate(StrictTriangularI_I42D(M,NLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='StrictTriangularI_I42D', ProcName=ProcName, stat=StatLoc )

    StrictTriangularI_I42D = 0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, NLoc
        if ( i > M ) then
          StrictTriangularI_I42D(:,i:) = 1
          exit
        end if
        StrictTriangularI_I42D(1:i-1,i) = 1
      end do
    elseif ( UL == 'L' ) then
      if ( M >= NLoc ) then
        imax = NLoc
      else
        imax = NLoc - 1
      end if
      i = 1
      do i = 1, imax
        StrictTriangularI_I42D(i+1:,i) = 1
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function StrictTriangularI_I82D( M, N, UL )

    integer(8), dimension(:,:), allocatable                           ::    StrictTriangularI_I82D

    integer(8), intent(in)                                            ::    M
    integer(8), optional, intent(in)                                  ::    N
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangularI_I82D'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i
    integer(8)                                                        ::    imax
    integer(8)                                                        ::    NLoc

    NLoc = M
    if ( present(N) ) NLoc = N

    allocate(StrictTriangularI_I82D(M,NLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='StrictTriangularI_I82D', ProcName=ProcName, stat=StatLoc )

    StrictTriangularI_I82D = 0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, NLoc
        if ( i > M ) then
          StrictTriangularI_I82D(:,i:) = 1
          exit
        end if
        StrictTriangularI_I82D(1:i-1,i) = 1
      end do
    elseif ( UL == 'L' ) then
      if ( M >= NLoc ) then
        imax = NLoc
      else
        imax = NLoc - 1
      end if
      i = 1
      do i = 1, imax
        StrictTriangularI_I82D(i+1:,i) = 1
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function StrictTriangularR_R42D( M, N, UL )

   real(4), dimension(:,:), allocatable                               ::    StrictTriangularR_R42D

    integer(4), intent(in)                                            ::    M
    integer(4), optional, intent(in)                                  ::    N
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangularR_R42D'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i
    integer(4)                                                        ::    imax
    integer(4)                                                        ::    NLoc

    NLoc = M
    if ( present(N) ) NLoc = N

    allocate(StrictTriangularR_R42D(M,NLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='StrictTriangularR_R42D', ProcName=ProcName, stat=StatLoc )

    StrictTriangularR_R42D = 0.0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, NLoc
        if ( i > M ) then
          StrictTriangularR_R42D(:,i:) = 1.0
          exit
        end if
        StrictTriangularR_R42D(1:i-1,i) = 1.0
      end do
    elseif ( UL == 'L' ) then
      if ( M >= NLoc ) then
        imax = NLoc
      else
        imax = NLoc - 1
      end if
      i = 1
      do i = 1, imax
        StrictTriangularR_R42D(i+1:,i) = 1.0
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function StrictTriangularR_R82D( M, N, UL )

    real(8), dimension(:,:), allocatable                              ::    StrictTriangularR_R82D

    integer(8), intent(in)                                            ::    M
    integer(8), optional, intent(in)                                  ::    N
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangularR_R82D'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i
    integer(8)                                                        ::    imax
    integer(8)                                                        ::    NLoc

    NLoc = M
    if ( present(N) ) NLoc = N

    allocate(StrictTriangularR_R82D(M,NLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='StrictTriangularR_R82D', ProcName=ProcName, stat=StatLoc )

    StrictTriangularR_R82D = 0.0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, NLoc
        if ( i > M ) then
          StrictTriangularR_R82D(:,i:) = 1.0
          exit
        end if
        StrictTriangularR_R82D(1:i-1,i) = 1.0
      end do
    elseif ( UL == 'L' ) then
      if ( M >= NLoc ) then
        imax = NLoc
      else
        imax = NLoc - 1
      end if
      i = 1
      do i = 1, imax
        StrictTriangularR_R82D(i+1:,i) = 1.0
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StrictTriangular_I42D( Array, UL )

    integer(4), dimension(:,:), intent(inout)                         ::    Array
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangular_I42D'
    integer                                                           ::    i
    integer                                                           ::    M
    integer                                                           ::    N
    integer(4)                                                        ::    imax

    M = size(Array,1)
    N = size(Array,2)

    Array = 0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, N
        if ( i > M ) then
          Array(:,i:) = 1
          exit
        end if
        Array(1:i-1,i) = 1
      end do
    elseif ( UL == 'L' ) then
      if ( M >= N ) then
        imax = N 
      else
        imax = N - 1
      end if
      i = 1
      do i = 1, imax
        Array(i+1:,i) = 1
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StrictTriangular_I82D( Array, UL )

    integer(8), dimension(:,:), intent(inout)                         ::    Array
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangular_I82D'
    integer                                                           ::    i
    integer                                                           ::    M
    integer                                                           ::    N
    integer(4)                                                        ::    imax

    M = size(Array,1)
    N = size(Array,2)

    Array = 0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, N
        if ( i > M ) then
          Array(:,i:) = 1
          exit
        end if
        Array(1:i-1,i) = 1
      end do
    elseif ( UL == 'L' ) then
      if ( M >= N ) then
        imax = N 
      else
        imax = N - 1
      end if
      i = 1
      do i = 1, imax
        Array(i+1:,i) = 1
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StrictTriangular_R42D( Array, UL )

    real(4), dimension(:,:), intent(inout)                            ::    Array
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangular_R42D'
    integer                                                           ::    i
    integer                                                           ::    M
    integer                                                           ::    N
    integer(4)                                                        ::    imax

    M = size(Array,1)
    N = size(Array,2)

    Array = 0.0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, N
        if ( i > M ) then
          Array(:,i:) = 1.0
          exit
        end if
        Array(1:i-1,i) = 1.0
      end do
    elseif ( UL == 'L' ) then
      if ( M >= N ) then
        imax = N 
      else
        imax = N - 1
      end if
      i = 1
      do i = 1, imax
        Array(i+1:,i) = 1.0
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StrictTriangular_R82D( Array, UL )

    real(8), dimension(:,:), intent(inout)                            ::    Array
    character(*), intent(in)                                          ::    UL

    character(*), parameter                                           ::    ProcName='StrictTriangular_R82D'
    integer                                                           ::    i
    integer                                                           ::    M
    integer                                                           ::    N
    integer(4)                                                        ::    imax

    M = size(Array,1)
    N = size(Array,2)

    Array = 0.0

    if ( UL == 'U' ) then
      i = 2
      do i = 2, N
        if ( i > M ) then
          Array(:,i:) = 1.0
          exit
        end if
        Array(1:i-1,i) = 1.0
      end do
    elseif ( UL == 'L' ) then
      if ( M >= N ) then
        imax = N 
      else
        imax = N - 1
      end if
      i = 1
      do i = 1, imax
        Array(i+1:,i) = 1.0
      end do
    else
      call Error%Raise( 'Upper or Lower option not recognized', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Diagonal_I42D( Diagonal )

    integer(4), dimension(:,:), allocatable                           ::    Diagonal_I42D
    integer(4), dimension(:), intent(in)                              ::    Diagonal

    character(*), parameter                                           ::    ProcName='Diagonal_I42D'
    integer                                                           ::    i
    integer                                                           ::    N
    integer                                                           ::    StatLoc

    N = size(Diagonal,1)

    allocate(Diagonal_I42D(N,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Diagonal_I42D', ProcName=ProcName, stat=StatLoc )
    Diagonal_I42D = 0

    i = 1
    do i = 1, N
      Diagonal_I42D(i,i) = Diagonal(i)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Diagonal_I82D( Diagonal )

    integer(8), dimension(:,:), allocatable                           ::    Diagonal_I82D
    integer(8), dimension(:), intent(in)                              ::    Diagonal

    character(*), parameter                                           ::    ProcName='Diagonal_I82D'
    integer                                                           ::    i
    integer                                                           ::    N
    integer                                                           ::    StatLoc

    N = size(Diagonal,1)

    allocate(Diagonal_I82D(N,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Diagonal_I82D', ProcName=ProcName, stat=StatLoc )
    Diagonal_I82D = 0

    i = 1
    do i = 1, N
      Diagonal_I82D(i,i) = Diagonal(i)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Diagonal_R42D( Diagonal )

    real(4), dimension(:,:), allocatable                              ::    Diagonal_R42D
    real(4), dimension(:), intent(in)                                 ::    Diagonal

    character(*), parameter                                           ::    ProcName='Diagonal_R42D'
    integer                                                           ::    i
    integer                                                           ::    N
    integer                                                           ::    StatLoc

    N = size(Diagonal,1)

    allocate(Diagonal_R42D(N,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Diagonal_R42D', ProcName=ProcName, stat=StatLoc )
    Diagonal_R42D = 0.0

    i = 1
    do i = 1, N
      Diagonal_R42D(i,i) = Diagonal(i)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Diagonal_R82D( Diagonal )

    real(8), dimension(:,:), allocatable                              ::    Diagonal_R82D
    real(8), dimension(:), intent(in)                                 ::    Diagonal

    character(*), parameter                                           ::    ProcName='Diagonal_R82D'
    integer                                                           ::    i
    integer                                                           ::    N
    integer                                                           ::    StatLoc

    N = size(Diagonal,1)

    allocate(Diagonal_R82D(N,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Diagonal_R82D', ProcName=ProcName, stat=StatLoc )
    Diagonal_R82D = 0

    i = 1
    do i = 1, N
      Diagonal_R82D(i,i) = Diagonal(i)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeI_I42D( N )

    integer(4), dimension(:,:), allocatable                           ::    EyeI_I42D
    integer(4), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='EyeI_I42D'
    integer(4)                                                        ::    i
    integer                                                           ::    StatLoc

    allocate( EyeI_I42D(N,N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeI_I42D', stat=StatLoc)

    EyeI_I42D = 0
    do i = 1,N 
      EyeI_I42D(i,i) = 1
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeI_I82D( N )

    integer(8), dimension(:,:), allocatable                           ::    EyeI_I82D
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='EyeI_I82D'
    integer(8)                                                        ::    i
    integer                                                           ::    StatLoc

    allocate( EyeI_I82D(N,N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeI_I82D', stat=StatLoc)

    EyeI_I82D = 0
    do i = 1,N 
      EyeI_I82D(i,i) = 1
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeR_R42D( N )

    real(4), dimension(:,:), allocatable                              ::    EyeR_R42D
    integer(4), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='EyeR_R42D'
    integer(4)                                                        ::    i
    integer                                                           ::    StatLoc

    allocate( EyeR_R42D(N,N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeR_R42D', stat=StatLoc)

    EyeR_R42D = Zero
    do i = 1,N 
      EyeR_R42D(i,i) = One
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeR_R82D( N )

    real(8), dimension(:,:), allocatable                              ::    EyeR_R82D
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='EyeR_R82D'
    integer(8)                                                        ::    i
    integer                                                           ::    StatLoc

    allocate( EyeR_R82D(N,N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeR_R82D', stat=StatLoc)

    EyeR_R82D = Zero
    do i = 1,N 
      EyeR_R82D(i,i) = One
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_R42D( Array )

    real(4), dimension(:,:)                                           ::    Array

    character(*), parameter                                           ::    ProcName='Eye_R42D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = Zero
    do i = 1, N 
      Array(i,i) = One
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_R82D( Array )

    real(8), dimension(:,:)                                           ::    Array

    character(*), parameter                                           ::    ProcName='Eye_R82D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = Zero
    do i = 1, N 
      Array(i,i) = One
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_I42D( Array )

    integer(4), dimension(:,:)                                        ::    Array

    character(*), parameter                                           ::    ProcName='Eye_I42D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = 0
    do i = 1, N 
      Array(i,i) = 1
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_I82D( Array )

    integer(8), dimension(:,:)                                        ::    Array

    character(*), parameter                                           ::    ProcName='Eye_I82D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = 0
    do i = 1, N 
      Array(i,i) = 1
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------



end module
