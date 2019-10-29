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

module LASSOBasic_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use LASSOCD_Module
use String_Library
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use LASSOMethod_Class                                             ,only:    LASSOMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type

implicit none

private

public                                                                ::    LASSOBasic_Type

type, extends(LASSOMethod_Type)                                       ::    LASSOBasic_Type
  logical                                                             ::    Hybrid=.false.
  class(CVErrorMethod_Type), allocatable                              ::    CVError
  character(:), allocatable                                           ::    Algorithm
  real(rkp)                                                           ::    InnerTolerance=1e-7
  real(rkp)                                                           ::    OuterTolerance=1e-5
  integer                                                             ::    NbLambda=100
  real(rkp)                                                           ::    LambdaRatio=0.0001
  real(rkp), allocatable, dimension(:)                                ::    LambdaSet
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
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LASSOBasic_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LASSOBasic'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LASSOBasic_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%CVError) ) deallocate(This%CVError, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CVError', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%LambdaSet) ) deallocate(This%LambdaSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LambdaSet', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(LASSOBasic_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%LambdaRatio = 0.0001
    This%NbLambda = 100
    This%InnerTolerance = 1e-7
    This%OuterTolerance = 1e-5
    This%Hybrid = .false.
    This%Algorithm = '<undefined>'
    This%RestrictNbFeatures = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(LASSOBasic_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    logical                                                           ::    Found
    integer                                                           ::    NbLambdaLoc
    real(rkp), allocatable, dimension(:)                              ::    LambdaSetLoc
    
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'restrict_nb_features'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%RestrictNbFeatures = VarL0D

    ParameterName = 'algorithm'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Algorithm = VarC0D

    ParameterName = 'hybrid'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Hybrid = VarL0D

    ParameterName = 'lambda_ratio'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%LambdaRatio = VarR0D

    ParameterName = 'nb_lambda'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%NbLambda = VarI0D

    ParameterName = 'inner_tolerance'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%InnerTolerance = VarR0D

    ParameterName = 'outer_tolerance'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%OuterTolerance = VarR0D

    ParameterName = 'lambda_vec'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      allocate(LambdaSetLoc, source=ConvertToReal8s(String=VarC0D), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LambdaSetLoc', ProcName=ProcName, stat=StatLoc )
      This%NbLambda = size(LambdaSetLoc,1)
      NbLambdaLoc = This%NbLambda
      call DLAORD('D', NbLambdaLoc, LambdaSetLoc, 1)
      call move_alloc(LambdaSetLoc, This%LambdaSet)
    end if

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, CVErrorMethod, Hybrid, LambdaRatio, NbLambda, LambdaSet, InnerTolerance, OuterTolerance,       &
                                                                                            Algorithm, RestrictNbFeatures, Debug )

    use String_Library

    class(LASSOBasic_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Hybrid
    real(rkp), optional, intent(in)                                   ::    LambdaRatio
    integer, optional, intent(in)                                     ::    NbLambda
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod
    real(rkp), dimension(:), optional, intent(in)                     ::    LambdaSet
    real(rkp), optional, intent(in)                                   ::    InnerTolerance
    real(rkp), optional, intent(in)                                   ::    OuterTolerance
    character(*), optional, intent(in)                                ::    Algorithm
    logical, optional, intent(in)                                     ::    RestrictNbFeatures
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbLambdaLoc
    real(rkp), allocatable, dimension(:)                              ::    LambdaSetLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(RestrictNbFeatures) ) This%RestrictNbFeatures = RestrictNbFeatures

    if ( present(Algorithm) ) This%Algorithm = trim(adjustl(Algorithm))

    if ( present(Hybrid) ) This%Hybrid = Hybrid

    if ( present(NbLambda) ) This%NbLambda = NbLambda

    if ( present(LambdaRatio) ) This%LambdaRatio = LambdaRatio

    if ( present(InnerTolerance) ) This%InnerTolerance = InnerTolerance

    if ( present(OuterTolerance) ) This%OuterTolerance = OuterTolerance

    if ( present(LambdaSet) ) then
      allocate(LambdaSetLoc, source=LambdaSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LambdaSetLoc', ProcName=ProcName, stat=StatLoc )
      This%NbLambda = size(LambdaSetLoc,1)
      NbLambdaLoc = This%NbLambda
      call DLAORD('D', NbLambdaLoc, LambdaSetLoc, 1)
      call move_alloc(LambdaSetLoc, This%LambdaSet)
    end if

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(LASSOBasic_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

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

    call GetInput%AddParameter( Name='restrict_nb_features', Value=ConvertToString(This%RestrictNbFeatures) )
    call GetInput%AddParameter( Name='algorithm', Value=ConvertToString(This%Algorithm) )
    call GetInput%AddParameter( Name='hybrid', Value=ConvertToString(This%Hybrid) )
    call GetInput%AddParameter( Name='ratio', Value=ConvertToString(This%LambdaRatio) )
    call GetInput%AddParameter( Name='nb_lambda', Value=ConvertToString(This%NbLambda) )
    call GetInput%AddParameter( Name='inner_tolerance', Value=ConvertToString(This%InnerTolerance) )
    call GetInput%AddParameter( Name='outer_tolerance', Value=ConvertToString(This%OuterTolerance) )
    if ( allocated(This%LambdaSet) ) then
      call GetInput%AddParameter( Name='lambda_vec', Value=ConvertToString(This%LambdaSet) )
    end if

    SectionName = 'cross_validation'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cross_validation'
    call GetInput%AddSection( Section=CVErrorMethod_Factory%GetObjectInput( Object=This%CVError, MainSectionName=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectoryLoc ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse( This, System, Goal, ModelSet, CoefficientsSet, CVError, Debug )

    class(LASSOBasic_Type), intent(in)                                ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    CoefficientsSetLoc
    integer, allocatable, dimension(:)                                ::    ActiveSet
    integer, allocatable, dimension(:)                                ::    NbActiveSet
    integer                                                           ::    NbLambdaLoc
    real(rkp), allocatable, dimension(:)                              ::    LambdaSetLoc
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    type(LinSolverOLS_Type)                                           ::    OLS
    type(LASSOBasic_Type)                                             ::    LASSOSolver
    integer                                                           ::    MaxNbFeatures
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    M = size(Goal,1)
    N = size(System,2)
    GoalMean = ComputeMean( Values=Goal )
    GoalVariance = ComputeSampleVar( Values=Goal )

    if ( abs((GoalVariance*real(M-1,rkp))/real(M,rkp)) < 1e-10 ) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if ( abs(VarianceLoc/MeanLoc) < 1e-10 ) then
          allocate(ModelSet(1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
          ModelSet = i
          allocate(CoefficientsSet(1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
          CoefficientsSet = GoalMean / MeanLoc
          if ( present(CVError) ) CVError = Zero
          if (DebugLoc) call Logger%Exiting()
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

    if ( This%RestrictNbFeatures ) then
      MaxNbFeatures = min(size(System,1),size(System,2))
    else
      MaxNbFeatures = size(System,2)
    end if

    NbLambdaLoc=This%NbLambda

    if ( allocated(This%LambdaSet) ) then
      allocate(LambdaSetLoc, source=This%LambdaSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LambdaSetLoc', ProcName=ProcName, stat=StatLoc )
    end if

    if ( This%Algorithm /= '<undefined>' ) then
      call LASSOCD( System=System, Goal=Goal, CoefficientsSet=CoefficientsSetLoc, ActiveSet=ActiveSet, NbActiveSet=NbActiveSet,   &
                    NbLambda=NbLambdaLoc, LambdaRatio=This%LambdaRatio, LambdaSet=LambdaSetLoc,                                   &
                    InnerTolerance=This%InnerTolerance, OuterTolerance=This%OuterTolerance, Algorithm=This%Algorithm,             &
                    MaxNbFeatures=MaxNbFeatures )
    else
      call LASSOCD( System=System, Goal=Goal, CoefficientsSet=CoefficientsSetLoc, ActiveSet=ActiveSet, NbActiveSet=NbActiveSet,   &
                    NbLambda=NbLambdaLoc, LambdaRatio=This%LambdaRatio, LambdaSet=LambdaSetLoc,                                   &
                    InnerTolerance=This%InnerTolerance, OuterTolerance=This%OuterTolerance, MaxNbFeatures=MaxNbFeatures )
    end if

    allocate(ModelSet(NbActiveSet(NbLambdaLoc)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
    ModelSet = ActiveSet(1:NbActiveSet(NbLambdaLoc))

    allocate(CoefficientsSet, source=CoefficientsSetLoc(ActiveSet(1:NbActiveSet(NbLambdaLoc)),NbLambdaLoc), stat=StatLoc)

    if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )

    if ( This%Hybrid ) then
      allocate(SystemLoc(size(System,1),NbActiveSet(NbLambdaLoc)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )

      SystemLoc = System(:,ModelSet)

      call OLS%Construct(CVErrorMethod=This%CVError)
      call OLS%SolveSystem( System=SystemLoc, Goal=Goal, Coefficients=CoefficientsSet )

      deallocate(SystemLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )
    end if

    if ( present(CVError) ) then
      call LASSOSolver%Construct( CVErrorMethod=This%CVError, Hybrid=This%Hybrid, LambdaRatio=This%LambdaRatio,                   &
                                  NbLambda=This%NbLambda, LambdaSet=LambdaSetLoc, InnerTolerance=This%InnerTolerance,             &
                                  OuterTolerance=This%OuterTolerance )
      CVError = This%CVError%ComputeError( Solver=LASSOSolver, System=System, Goal=Goal,                                          &
                                           Coefficients=CoefficientsSetLoc(:,NbLambdaLoc) )
    end if

    deallocate(ActiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ActiveSet', ProcName=ProcName, stat=StatLoc )

    deallocate(NbActiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NbActiveSet', ProcName=ProcName, stat=StatLoc )

    deallocate(CoefficientsSetLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LambdaSetLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LambdaSetLoc', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull( This, System, Goal, Coefficients, CVError, Debug )

    class(LASSOBasic_Type), intent(in)                                ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveFull'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    ModelSet
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsSet

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( present(CVError) ) then
      call This%Solve( System, Goal, ModelSet, CoefficientsSet, CVError )
    else
      call This%Solve( System, Goal, ModelSet, CoefficientsSet )
    end if

    allocate(Coefficients(size(System,2)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
    Coefficients = Zero

    Coefficients(ModelSet) = CoefficientsSet

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LASSOBasic_Type), intent(out)                               ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (LASSOBasic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%LambdaRatio = RHS%LambdaRatio
          LHS%NbLambda = RHS%NbLambda
          LHS%InnerTolerance = RHS%InnerTolerance
          LHS%OuterTolerance = RHS%OuterTolerance
          LHS%Hybrid = RHS%Hybrid
          LHS%Algorithm = RHS%Algorithm
          LHS%RestrictNbFeatures = RHS%RestrictNbFeatures
          if ( allocated(RHS%LambdaSet) ) then
            allocate(LHS%LambdaSet, source=RHS%LambdaSet, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%LambdaSet', ProcName=ProcName, stat=StatLoc )
          end if
          allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CVError', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Mismatching object types', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(LASSOBasic_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%LambdaSet) ) deallocate(This%LambdaSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LambdaSet', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
