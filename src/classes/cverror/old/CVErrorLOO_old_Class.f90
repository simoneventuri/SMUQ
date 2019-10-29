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

module CVErrorLOO_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type

implicit none

private

public                                                                ::    CVErrorLOO_Type

type, extends(CVErrorMethod_Type)                                     ::    CVErrorLOO_Type
  logical                                                             ::    Corrected=.false.
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    ComputeError            =>    ComputeErrorCase1Q
  procedure, public                                                   ::    ComputeErrorDefault     =>    ComputeErrorDefaultQ
  procedure, public                                                   ::    ComputeErrorCase1Q
  procedure, nopass, public                                           ::    ComputeHatDiagQ
  procedure, public                                                   ::    ComputeCorrFactorQ
  generic, public                                                     ::    ComputeErrorChl         =>    ComputeErrorCase1Chl,   &
                                                                                                          ComputeErrorDefaultChl
  procedure, public                                                   ::    ComputeErrorDefaultChl
  procedure, public                                                   ::    ComputeErrorCase1Chl
  procedure, nopass, public                                           ::    ComputeHatDiagChl
  procedure, public                                                   ::    ComputeCorrFactorChl
  procedure, public                                                   ::    IsCorrected
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( .not. This%Initialized ) then
      This%Name = 'loo'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    This%Corrected=.false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    ParameterName = 'corrected'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if( Found ) This%Corrected=VarL0D

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, Corrected, Debug )

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Corrected
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if( present(Corrected) ) This%Corrected=Corrected

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(CVErrorLOO_Type), intent(in)                                ::    This
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

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='corrected', Value=ConvertToString( Value=This%Corrected ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeErrorDefaultQ( This, Coefficients, DesignSpace, Goal, Debug )

    use StatisticsRoutines_Module

    real(rkp)                                                         ::    ComputeErrorDefaultQ

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    real(rkp), dimension(:,:), intent(in)                             ::    DesignSpace
    real(rkp), dimension(:), intent(in)                               ::    Goal
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeErrorDefaultQ'
    real(rkp), allocatable, dimension(:,:)                            ::    Q
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp)                                                         ::    CorrFactor=One
    real(rkp)                                                         ::    Prediction
    real(rkp)                                                         ::    GoalVariance
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    M = size(DesignSpace,1)
    N = size(DesignSpace,2)

    allocate(Q, source=DesignSpace, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q', ProcName=ProcName, stat=StatLoc )

    allocate( TAU(min(M,N)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TAU', ProcName=ProcName, stat=StatLoc )

    if ( M >= N ) then

      call DGEQRF( M, N, Q, M, TAU, WORKSIZE, -1, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

      LWORK = nint(WORKSIZE(1))

      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

      call DGEQRF( M, N, Q, M, TAU, WORK, LWORK, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

    else

      call DGEQLF( M, N, Q, M, TAU, WORKSIZE, -1, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQLF", ProcName=ProcName )

      LWORK = nint(WORKSIZE(1))

      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

      call DGEQLF( M, N, Q, M, TAU, WORK, LWORK, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQLF", ProcName=ProcName )

    end if

    deallocate(WORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

    call This%ComputeHatDiagQ( Q=Q, TAU=TAU, HatDiag=HatDiag )

    ComputeErrorDefaultQ = Zero
    i = 1
    do i = 1, M
      Prediction = 0
      ii = 1
      do ii = 1, N
        Prediction = Prediction + Coefficients(ii)*DesignSpace(i,ii)
      end do
      ComputeErrorDefaultQ = ComputeErrorDefaultQ + ( ( Goal(i) - Prediction ) / ( One - HatDiag(i) ) )**2
    end do

    GoalVariance = ComputeSampleVar(Goal)
    if ( GoalVariance <= Zero ) GoalVariance = One

    ComputeErrorDefaultQ = ComputeErrorDefaultQ / ( real(M,rkp) * GoalVariance )

    if ( This%Corrected ) ComputeErrorDefaultQ = ComputeErrorDefaultQ * This%ComputeCorrFactorQ( Q )

    deallocate(Q, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Q', ProcName=ProcName, stat=StatLoc )

    deallocate(HatDiag, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeErrorCase1Q( This, Coefficients, DesignSpace, Goal, HatDiag, CorrFactor, Debug )

    use StatisticsRoutines_Module

    real(rkp)                                                         ::    ComputeErrorCase1Q

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    real(rkp), dimension(:,:), intent(in)                             ::    DesignSpace
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), dimension(:), intent(in)                               ::    HatDiag
    real(rkp), intent(in)                                             ::    CorrFactor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeErrorCase1Q'
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp)                                                         ::    Prediction
    real(rkp)                                                         ::    GoalVariance
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    M = size(DesignSpace,1)
    N = size(DesignSpace,2)

    ComputeErrorCase1Q = Zero
    i = 1
    do i = 1, M
      Prediction = 0
      ii = 1
      do ii = 1, N
        Prediction = Prediction + Coefficients(ii)*DesignSpace(i,ii)
      end do
      ComputeErrorCase1Q = ComputeErrorCase1Q + ( ( Goal(i) - Prediction ) / ( One - HatDiag(i) ) )**2
    end do

    GoalVariance = ComputeSampleVar(Goal)
    if ( GoalVariance <= Zero ) GoalVariance = One

    ComputeErrorCase1Q = ComputeErrorCase1Q / ( real(M,rkp) * GoalVariance ) * CorrFactor
 
    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ComputeHatDiagQ( Q, TAU, HatDiag, Debug )

    real(rkp), dimension(:,:), intent(in)                             ::    Q
    real(rkp), dimension(:), intent(in)                               ::    TAU
    real(rkp), allocatable, dimension(:), intent(out)                 ::    HatDiag
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeHatDiagQ'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    M = size(Q,1)
    N = size(Q,2)

    allocate( Q1, source=Q, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q1', ProcName=ProcName, stat=StatLoc )    

    if ( M >= N ) then

      call DORGQR( M, N, N, Q1, M, TAU, WORKSIZE, -1, StatLoc )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORGQR", ProcName=ProcName )

      LWORK = nint(WORKSIZE(1))

      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

      call DORGQR( M, N, N, Q1, M, TAU, WORK, LWORK, StatLoc )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORGQR", ProcName=ProcName )

    else

      call DORGQL( M, N, N, Q1, M, TAU, WORKSIZE, -1, StatLoc )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORGQL", ProcName=ProcName )

      LWORK = nint(WORKSIZE(1))

      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

      call DORGQL( M, N, N, Q1, M, TAU, WORK, LWORK, StatLoc )
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORGQL", ProcName=ProcName )

    end if

    deallocate(WORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

    allocate( HatDiag(M), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    Q1 = Q1*Q1
    HatDiag = sum(Q1,2)

    deallocate(Q1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Q1', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeCorrFactorQ( This, Q, Debug )

    real(rkp)                                                         ::    ComputeCorrFactorQ

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Q
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCorrFactorQ'
    integer                                                           ::    StatLoc = 0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    N
    integer                                                           ::    i, ii
    integer                                                           ::    iim1

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    N = size(Q,2)

    allocate( VarR1D(N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    ComputeCorrFactorQ = Zero

    do i = 1, N
      VarR1D = Zero
      VarR1D(i) = One / Q(i,i)

      ii = i+1
      do ii = i+1, N
        iim1 = ii - 1
        VarR1D(ii) = ( - dot_product( Q(i:iim1,ii), VarR1D(i:iim1) ) ) / Q(ii,ii)
      end do

      VarR1D(i:) = VarR1D(i:)*VarR1D(i:)
      ComputeCorrFactorQ = ComputeCorrFactorQ + sum( VarR1D(i:) )
    end do

    deallocate( VarR1D, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeErrorDefaultChl( This, Coefficients, DesignSpace, Goal, Debug )

    use StatisticsRoutines_Module

    real(rkp)                                                         ::    ComputeErrorDefaultChl

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    real(rkp), dimension(:,:), intent(in)                             ::    DesignSpace
    real(rkp), dimension(:), intent(in)                               ::    Goal
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeErrorDefaultChl'
    real(rkp), allocatable, dimension(:,:)                            ::    InfoLChol
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp)                                                         ::    Prediction
    real(rkp)                                                         ::    GoalVariance
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    M = size(DesignSpace,1)
    N = size(DesignSpace,2)

    allocate( InfoLChol(N,N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InfoLChol', ProcName=ProcName, stat=StatLoc )   
                                       
    call DGEMM( 'T', 'N', N, N, M, real(1,8), DesignSpace, M, DesignSpace, M, 0.0d+0, InfoLChol, N )

    call DPOTRF( 'L', N, InfoLChol, N, StatLoc )
    if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DPOTRF", ProcName=ProcName )

    call This%ComputeHatDiagChl( DesignSpace=DesignSpace, InfoLChol=InfoLChol, HatDiag=HatDiag )

    ComputeErrorDefaultChl = Zero
    i = 1
    do i = 1, M
      Prediction = 0
      ii = 1
      do ii = 1, N
        Prediction = Prediction + Coefficients(ii)*DesignSpace(i,ii)
      end do
      ComputeErrorDefaultChl = ComputeErrorDefaultChl + ( ( Goal(i) - Prediction ) / ( One - HatDiag(i) ) )**2
    end do

    GoalVariance = ComputeSampleVar(Goal)
    if ( GoalVariance <= Zero ) GoalVariance = One

    ComputeErrorDefaultChl = ComputeErrorDefaultChl / ( real(M,rkp) * GoalVariance )

    if ( This%Corrected ) ComputeErrorDefaultChl = ComputeErrorDefaultChl * This%ComputeCorrFactorChl( InfoLChol=InfoLChol,       &
                                                                                                                        NbData=M )

    deallocate(HatDiag, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeErrorCase1Chl( This, Coefficients, DesignSpace, Goal, HatDiag, CorrFactor, Debug )

    use StatisticsRoutines_Module
    
    real(rkp)                                                         ::    ComputeErrorCase1Chl

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    real(rkp), dimension(:,:), target, intent(in)                     ::    DesignSpace
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), dimension(:), intent(in)                               ::    HatDiag
    real(rkp), intent(in)                                             ::    CorrFactor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeErrorCase1Chl'
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp)                                                         ::    Prediction
    real(rkp)                                                         ::    GoalVariance
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    M = size(DesignSpace,1)
    N = size(DesignSpace,2)

    ComputeErrorCase1Chl = Zero
    i = 1
    do i = 1, M
      Prediction = 0
      ii = 1
      do ii = 1, N
        Prediction = Prediction + Coefficients(ii)*DesignSpace(i,ii)
      end do
      ComputeErrorCase1Chl = ComputeErrorCase1Chl + ( ( Goal(i) - Prediction ) / ( One - HatDiag(i) ) )**2
    end do

    GoalVariance = ComputeSampleVar(Goal)
    if ( GoalVariance <= Zero ) GoalVariance = One

    ComputeErrorCase1Chl = ComputeErrorCase1Chl / ( real(M,rkp) * GoalVariance ) * CorrFactor

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ComputeHatDiagChl( DesignSpace, InfoLChol, HatDiag, Debug )

    real(rkp), dimension(:,:), intent(in)                             ::    DesignSpace
    real(rkp), dimension(:,:), intent(in)                             ::    InfoLChol
    real(rkp), allocatable, dimension(:), intent(out)                 ::    HatDiag
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeHatDiagChl'
    real(rkp), allocatable, dimension(:,:)                            ::    BLoc
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i
    integer                                                           ::    INFO
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    M = size(DesignSpace,1)
    N = size(DesignSpace,2)

    allocate( HatDiag(M), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    allocate(BLoc(N,1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='BLoc', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, M
      BLoc(:,1) = DesignSpace(i,:)
      call DTRTRS( 'L', 'N', 'N', N, 1, InfoLChol, N, BLoc, N, StatLoc )  ! solve LZ=X'
      if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DTRTRS", ProcName=ProcName )
      HatDiag( i ) = sum(BLoc**2)
    end do

    deallocate(BLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='BLoc', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeCorrFactorChl( This, InfoLChol, NbData, Debug )

    real(rkp)                                                         ::    ComputeCorrFactorChl

    class(CVErrorLOO_Type), intent(in)                                ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    InfoLChol
    integer, intent(in)                                               ::    NbData
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCorrFactorChl'
    real(rkp), allocatable, dimension(:,:)                            ::    BLoc
    real(rkp)                                                         ::    trcInfoinv
    integer                                                           ::    i
    integer                                                           ::    N
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    N = size(InfoLChol,1)

    if ( This%Corrected ) then
      if ( N > 1 ) then
        allocate(BLoc(N,1), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='BLoc', ProcName=ProcName, stat=StatLoc )
        BLoc = Zero
        TrcInfoInv=Zero
        i = 1
        do i = 1, N
          BLoc(i:N,1) = Zero
          BLoc(i,1) = One
          call DTRTRS( 'L', 'N', 'N', N, 1, InfoLChol, N, BLoc(:,1), N, StatLoc )
          TrcInfoInv = TrcInfoInv + sum( BLoc(i:N,1)**2 )
          deallocate(BLoc, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='BLoc', ProcName=ProcName, stat=StatLoc )
        end do
      else
        TrcInfoInv = One/InfoLChol(1,1)
      end if
      ComputeCorrFactorChl =  ( real(NbData,rkp) / ( real(NbData,rkp) - real(N,rkp) ) ) * ( One + TrcInfoInv )
    else
      ComputeCorrFactorChl = One
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function IsCorrected( This, Debug )

    logical                                                           ::    IsCorrected

    class(CVErrorLOO_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsCorrected'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsCorrected = This%Corrected

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(CVErrorLOO_Type), intent(out)                               ::    LHS
    class(CVErrorMethod_Type), intent(in)                             ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (CVErrorLOO_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) LHS%Corrected = RHS%Corrected

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
