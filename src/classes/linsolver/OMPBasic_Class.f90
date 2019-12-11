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

module OMPBasic_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use ComputingRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use OMPMethod_Class                                               ,only:    OMPMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type

implicit none

private

public                                                                ::    OMPBasic_Type

type, extends(OMPMethod_Type)                                         ::    OMPBasic_Type
  class(CVErrorMethod_Type), allocatable                              ::    CVError
  integer                                                             ::    MaxNbFeatures=huge(1)
  real(rkp)                                                           ::    Tolerance=Zero
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    SolveSparse
  procedure, public                                                   ::    SolveFull
  procedure, public                                                   ::    BuildModel
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(OMPBasic_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'OMPBasic'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(OMPBasic_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%CVError) ) deallocate(This%CVError, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CVError', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(OMPBasic_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MaxNbFeatures = huge(This%MaxNbFeatures)
    This%Tolerance = Zero

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library

    class(OMPBasic_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    Found

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'max_nb_features'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MaxNbFeatures = VarI0D

    ParameterName = 'tolerance'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Tolerance = VarR0D

    SectionName = 'cross_validation'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found )
    if ( Found ) then
      call CVErrorMethod_Factory%Construct( Object=This%CVError, Input=InputSection, Prefix=PrefixLoc )
    else
      allocate( CVErrorLOO_Type :: This%CVError )
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          call CVErrorMethod%Construct( Corrected=.true. )
        class default
          call Error%Raise( Line='Something went wrong', ProcName=ProcName )
      end select
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, MaxNbFeatures, Tolerance, CVErrorMethod )

    use String_Library

    class(OMPBasic_Type), intent(inout)                               ::    This
    integer, optional, intent(in)                                     ::    MaxNbFeatures
    real(rkp), optional, intent(in)                                   ::    Tolerance
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(MaxNbFeatures) ) This%MaxNbFeatures = MaxNbFeatures

    if ( present(Tolerance) ) This%Tolerance = Tolerance

    if ( present(CVErrorMethod) ) then
      allocate(This%CVError, source=CVErrorMethod, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CVErrorMethod', ProcName=ProcName, stat=StatLoc )
    else
      allocate( CVErrorLOO_Type :: This%CVError )
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          call CVErrorMethod%Construct( Corrected=.true. )
        class default
          call Error%Raise( Line='Something went wrong', ProcName=ProcName )
      end select
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(OMPBasic_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='max_nb_features', Value=ConvertToString(This%MaxNbFeatures) )
    call GetInput%AddParameter( Name='tolerance', Value=ConvertToString(This%Tolerance) )

    SectionName = 'cross_validation'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cross_validation'
    call GetInput%AddSection( Section=CVErrorMethod_Factory%GetObjectInput( Object=This%CVError, MainSectionName=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectoryLoc ) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse( This, System, Goal, ModelSet, CoefficientsSet, CVError )

    class(OMPBasic_Type), intent(in)                                  ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    CVErrorLOOFlag
    type(LinSolverOLS_Type)                                           ::    OLS
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsLoc
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    M = size(Goal,1)
    N = size(System,2)
    GoalMean = ComputeMean( Values=Goal )
    GoalVariance = ComputeSampleVar( Values=Goal )

    if ( dsqrt(abs((GoalVariance*real(M-1,rkp))/real(M,rkp)))/abs(GoalMean) < 1e-10 ) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if ( abs(dsqrt(VarianceLoc)/MeanLoc) < 1e-10 ) then
          allocate(ModelSet(1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
          ModelSet = i
          allocate(CoefficientsSet(1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
          CoefficientsSet = GoalMean / MeanLoc
          if ( present(CVError) ) CVError = Zero
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

    if ( size(System,1) >= size(System,2) .and. This%MaxNbFeatures >= size(System,2) ) then
      call OLS%Construct(CVErrorMethod=This%CVError)
      if ( present(CVError) ) then
        call OLS%SolveSystem( System=System, Goal=Goal, Coefficients=CoefficientsSet, CVError=CVError )
      else
        call OLS%SolveSystem( System=System, Goal=Goal, Coefficients=CoefficientsSet )
      end if

      ModelSet = LinSequence( SeqStart=1, SeqEnd=size(System,2) )

    else
      if ( present(CVError) ) then

        select type (Object => This%CVError)
          type is ( CVErrorLOO_Type ) 
            CVErrorLOOFlag = .true.
          class default
            CVErrorLOOFlag = .false.
        end select

        if ( CVErrorLOOFlag ) then
          call This%BuildModel( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,                     &
                                                     CVError=CVError, MaxNbFeatures=This%MaxNbFeatures, Tolerance=This%Tolerance )
        else
          call This%BuildModel( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,                     &
                                                                      MaxNbFeatures=This%MaxNbFeatures, Tolerance=This%Tolerance )

          allocate(CoefficientsLoc(size(System,2)), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )
          CoefficientsLoc = Zero

          CoefficientsLoc(ModelSet) = CoefficientsSet
          CVError = This%CVError%ComputeError( Solver=This, System=System, Goal=Goal, Coefficients=CoefficientsLoc )

          deallocate(CoefficientsLoc, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )
        end if

      else
        call This%BuildModel( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,                       &
                                                                                                MaxNbFeatures=This%MaxNbFeatures )
      end if

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull( This, System, Goal, Coefficients, CVError )

    class(OMPBasic_Type), intent(in)                                  ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveFull'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    ModelSet
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsSet

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( present(CVError) ) then
      call This%Solve( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet, CVError=CVError )
    else
      call This%Solve( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet )
    end if

    allocate(Coefficients(size(System,2)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
    Coefficients = Zero

    Coefficients(ModelSet) = CoefficientsSet

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Comparison of orthogonal maching pursuit implementations, Sturm and Christensen 2012
  subroutine BuildModel( This, System, Goal, ModelSet, CoefficientsSet, CVError, MaxNbFeatures, Tolerance )

    class(OMPBasic_Type), intent(in)                                  ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
    integer, optional, intent(in)                                     ::    MaxNbFeatures
    real(rkp), optional, intent(in)                                   ::    Tolerance

    character(*), parameter                                           ::    ProcName='SolveFull'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    MaxNbFeaturesLoc
    real(rkp)                                                         ::    ToleranceLoc
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    P
    integer                                                           ::    PM1
    real(rkp), allocatable, dimension(:)                              ::    Norm
    real(rkp), allocatable, dimension(:)                              ::    ResCorr
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVar
    real(rkp)                                                         ::    STD
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:,:)                            ::    R
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_2
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iiim1
    integer                                                           ::    iiip1
    integer                                                           ::    iim1
    integer                                                           ::    NewIndex
    integer, allocatable, dimension(:)                                ::    ActiveSet
    real(rkp)                                                         ::    uqk
    real(rkp)                                                         ::    CorrFactor
    real(rkp)                                                         ::    WNorm
    real(rkp)                                                         ::    VarR0D

    M = size(System,1)
    N = size(System,2)
    P = min(M,N)

    MaxNbFeaturesLoc = N
    if ( present(MaxNbFeatures) ) MaxNbFeaturesLoc = MaxNbFeatures
    if ( MaxNbFeaturesLoc > P ) MaxNbFeaturesLoc = P

    ToleranceLoc = Zero
    if ( present(Tolerance) ) ToleranceLoc = Tolerance

    allocate(Norm(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Norm', ProcName=ProcName, stat=StatLoc )
    Norm = Zero

    allocate(ResCorr(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Corr', ProcName=ProcName, stat=StatLoc )
    ResCorr = Zero

    allocate(Q1(M,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q1', ProcName=ProcName, stat=StatLoc )
    Q1 = Zero
  
    allocate(R(P,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='R', ProcName=ProcName, stat=StatLoc )
    R = Zero

    allocate(ActiveSet(MaxNbFeaturesLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ActiveSet', ProcName=ProcName, stat=StatLoc )
    ActiveSet = 0

    allocate(VarR1D_2(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D_2', ProcName=ProcName, stat=StatLoc )
    VarR1D_2 = Zero

    allocate(VarR1D_1(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D_1', ProcName=ProcName, stat=StatLoc )
    VarR1D_1 = Zero

    GoalMean = ComputeMean( Values=Goal )
    GoalVar = ComputeSampleVar(Values=Goal, Mean=GoalMean)

    i = 1
    do i = 1, N
      Norm(i) = ComputeNorm( Vector=System(:,i), Norm=2 )
      ResCorr(i) = dot_product( Goal, System(:,i) ) / Norm(i)
    end do

    P = 0
    do i = 1, MaxNbFeaturesLoc

      PM1 = P      
      P = P + 1
      
      NewIndex = maxloc(abs(ResCorr),1)

      ActiveSet(i) = NewIndex

      ! updating QR
      if ( i > 1 ) then
        call DGEMV( 'T', M, PM1, 1.d0, Q1(:,1:PM1), M, System(:,NewIndex), 1, 0.d0, VarR1D_2(1:PM1), 1 )

        WNorm = ComputeNorm(Vector=VarR1D_2(1:PM1), Norm=2)
        VarR0D = dsqrt(ComputeNorm(Vector=System(:,NewIndex), Norm=2)**2 - WNorm**2)

        ii = 1
        do ii = 1, M
          VarR1D_1(1:PM1) = Q1(ii,1:PM1)
          Q1(ii,i) = (System(ii,NewIndex) - dot_product(VarR1D_1(1:PM1),VarR1D_2(1:PM1))) / VarR0D
        end do

        R(1:PM1,i) = VarR1D_2(1:PM1)
        R (i,i) = VarR0D
      else
        R(1,1) = ComputeNorm(Vector=System(:,NewIndex), Norm=2)
        Q1(:,1) = System(:,NewIndex) / R(1,1)
      end if

      uqk = dot_product(Goal, Q1(:,i))

      if ( i == MaxNbFeaturesLoc ) exit
 !     if ( abs(uqk)**2 / GoalVar < ToleranceLoc ) exit

      ResCorr = ResCorr - uqk * dot_product(Q1(:,i),System(:,NewIndex)) / Norm(i)
    end do

    allocate(CoefficientsSet(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
    CoefficientsSet = Zero

    allocate(ModelSet(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )  
    ModelSet = 0

    ! solving system
    call DGEMV( 'T', M, P, 1.d0, Q1(:,1:P), M, Goal, 1, 0.d0, VarR1D_2(1:M), 1 )

    VarR1D_2(P) = VarR1D_2(P) / R(P,P)
    ii = 2
    do ii = 2, P
      iim1 = ii - 1
      iii = P - iim1
      iiip1 = iii + 1
      VarR1D_2(iii) = ( VarR1D_2(iii) - dot_product( R(iii,iiip1:P), VarR1D_2(iiip1:P) ) ) / R(iii,iii)
    end do

    ModelSet = ActiveSet(1:P)
    CoefficientsSet = VarR1D_2(1:P)

    if ( present(CVError) ) then

      allocate(HatDiag(M), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )
      HatDiag = Zero

      ! computing hat diagonals
      HatDiag = sum(Q1(:,1:P)**2,2)

      ! calculating error
      CVError = Zero
      ii = 1
      do ii = 1, M
        iii = 1
        VarR1D_1(1:P) = System(ii, ActiveSet(1:P))
        CVError = CVError + ( (Goal(ii)-dot_product(VarR1D_2(1:P), VarR1D_1(1:P))) / (One-HatDiag(ii)) )**2
      end do
      CVError = CVError / real(M,rkp)
  
      if ( This%CVError%IsNormalized() ) CVError = CVError / GoalVar

      if( This%CVError%IsCorrected() ) then
        ! computing correction factor
        CorrFactor = Zero

        do ii = 1, P
          VarR1D_1(1:P) = Zero
          VarR1D_1(ii) = One / R(ii,ii)

          iii = ii+1
          do iii = ii+1, P
            iiim1 = iii - 1
            VarR1D_1(iii) = ( - dot_product( R(ii:iiim1,iii), VarR1D_1(ii:iiim1) ) ) / R(iii,iii)
          end do

          VarR1D_1(ii:P) = VarR1D_1(ii:i)*VarR1D_1(ii:P)
          CorrFactor = CorrFactor + sum( VarR1D_1(ii:P) )
        end do

        CorrFactor = ( real(M,rkp) / ( real(M,rkp) - real(P,rkp) ) ) * ( One + CorrFactor )
        CVError = CVError * CorrFactor
      end if

      deallocate(HatDiag, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    end if

    deallocate(VarR1D_1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_1', ProcName=ProcName, stat=StatLoc )

    deallocate(VarR1D_2, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_2', ProcName=ProcName, stat=StatLoc )

    deallocate(Q1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Q1', ProcName=ProcName, stat=StatLoc )

    deallocate(R, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='R', ProcName=ProcName, stat=StatLoc )

    deallocate(ResCorr, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ResCorr', ProcName=ProcName, stat=StatLoc )

    deallocate(Norm, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Norm', ProcName=ProcName, stat=StatLoc )

    deallocate(ActiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ActiveSet', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(OMPBasic_Type), intent(out)                                 ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (OMPBasic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%MaxNbFeatures = RHS%MaxNbFeatures
          LHS%Tolerance = RHS%Tolerance
          allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CVError', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Mismatching object types', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
