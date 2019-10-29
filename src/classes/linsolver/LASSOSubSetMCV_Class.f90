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

module LASSOSubSetMCV_Class

use Input_Library
use Parameters_Library
use StatisticsRoutines_Module
use ComputingRoutines_Module
use StringRoutines_Module
use StatisticsRoutines_Module
use LASSOCD_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use LASSOMethod_Class                                             ,only:    LASSOMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type
use CVErrorKFold_Class                                            ,only:    CVErrorKFold_Type
use List1D_Class                                                  ,only:    List1D_Type

implicit none

private

public                                                                ::    LASSOSubSetMCV_Type

type, extends(LASSOMethod_Type)                                       ::    LASSOSubSetMCV_Type
  class(CVErrorMethod_Type), allocatable                              ::    CVError
  character(:), allocatable                                           ::    Algorithm
  real(rkp)                                                           ::    InnerTolerance=1e-7
  real(rkp)                                                           ::    OuterTolerance=1e-5
  integer                                                             ::    NbLambda=100
  real(rkp)                                                           ::    LambdaRatio=0.0001
  real(rkp), allocatable, dimension(:)                                ::    LambdaSet
  real(rkp)                                                           ::    CVStopRatio
  integer                                                             ::    CVStopMinIter
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
  procedure, public                                                   ::    SelectModelCVLOO_OD
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LASSOSubSetMCV_Type), intent(inout)                         ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LASSOSubSetMCV'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LASSOSubSetMCV_Type), intent(inout)                         ::    This
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

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(LASSOSubSetMCV_Type), intent(inout)                         ::    This
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
    This%Algorithm = '<undefined>'
    This%RestrictNbFeatures = .true.
    This%CVStopRatio = 0.1
    This%CVStopMinIter = 3

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use String_Library

    class(LASSOSubSetMCV_Type), intent(inout)                         ::    This
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

    ParameterName = 'cv_stop_ratio'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CVStopRatio = VarR0D

    ParameterName = 'cv_stop_min_iter'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CVStopMinIter = VarI0D

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
  subroutine ConstructCase1( This, CVErrorMethod, LambdaRatio, NbLambda, LambdaSet, InnerTolerance, OuterTolerance,               &
                                                                Algorithm, RestrictNbFeatures, CVStopRatio, CVStopMinIter, Debug )

    use String_Library

    class(LASSOSubSetMCV_Type), intent(inout)                         ::    This
    real(rkp), optional, intent(in)                                   ::    LambdaRatio
    integer, optional, intent(in)                                     ::    NbLambda
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod
    real(rkp), dimension(:), optional, intent(in)                     ::    LambdaSet
    real(rkp), optional, intent(in)                                   ::    InnerTolerance
    real(rkp), optional, intent(in)                                   ::    OuterTolerance
    character(*), optional, intent(in)                                ::    Algorithm
    logical, optional, intent(in)                                     ::    RestrictNbFeatures
    real(rkp), optional, intent(in)                                   ::    CVStopRatio
    integer, optional, intent(in)                                     ::    CVStopMinIter
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

    if ( present(NbLambda) ) This%NbLambda = NbLambda

    if ( present(LambdaRatio) ) This%LambdaRatio = LambdaRatio

    if ( present(InnerTolerance) ) This%InnerTolerance = InnerTolerance

    if ( present(OuterTolerance) ) This%OuterTolerance = OuterTolerance

    if ( present(CVStopRatio) ) This%CVStopRatio = CVStopRatio

    if ( present(CVStopMinIter) ) This%CVStopMinIter = CVStopMinIter

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

    class(LASSOSubSetMCV_Type), intent(in)                            ::    This
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
    call GetInput%AddParameter( Name='ratio', Value=ConvertToString(This%LambdaRatio) )
    call GetInput%AddParameter( Name='nb_lambda', Value=ConvertToString(This%NbLambda) )
    call GetInput%AddParameter( Name='inner_tolerance', Value=ConvertToString(This%InnerTolerance) )
    call GetInput%AddParameter( Name='outer_tolerance', Value=ConvertToString(This%OuterTolerance) )
    call GetInput%AddParameter( Name='cv_stop_ratio', Value=ConvertToString(This%CVStopRatio) )
    call GetInput%AddParameter( Name='cv_stop_min_iter', Value=ConvertToString(This%CVStopMinIter) )

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

    class(LASSOSubSetMCV_Type), intent(in)                            ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:), target                    ::    SystemLoc
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp)                                                         ::    CVErrorLoc
    type(LinSolverOLS_Type)                                           ::    OLS
    integer, allocatable, dimension(:)                                ::    ActiveSet
    integer, allocatable, dimension(:)                                ::    NbActiveSet
    integer                                                           ::    NbLambdaLoc
    real(rkp), allocatable, dimension(:)                              ::    LambdaSetLoc
    integer                                                           ::    NbCandidates
    type(List1D_Type), allocatable, dimension(:)                      ::    InactiveSet
    integer, allocatable, dimension(:)                                ::    NbInactiveSet
    integer, dimension(:), pointer                                    ::    InactiveSetPtr=>null()
    integer, dimension(:), pointer                                    ::    InactiveSetPtrM1=>null()
    integer, dimension(:), allocatable                                ::    IndicesSet
    integer                                                           ::    NbIndices
    integer                                                           ::    FinalNbCoefficients
    real(rkp), allocatable, dimension(:)                              ::    FinalCoefficientsSet
    integer, dimension(:), allocatable                                ::    FinalIndicesSet
    integer                                                           ::    NbActive
    integer                                                           ::    MaxNbFeatures
    logical                                                           ::    LOOCVFlag
    integer                                                           ::    OFCounter
    integer                                                           ::    OFTrip
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    M

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( This%RestrictNbFeatures ) then
      MaxNbFeatures = min(size(System,1),size(System,2))
    else
      MaxNbFeatures = size(System,2)
    end if

    M = size(Goal,1)
    GoalMean = ComputeMean( Values=Goal )
    GoalVariance = ComputeSampleVar( Values=Goal )

    if ( abs((GoalVariance*real(M-1,rkp))/real(M,rkp)) < 1e-10 ) then
      i = 1
      do i = 1, M
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

    NbLambdaLoc=This%NbLambda

    if ( allocated(This%LambdaSet) ) then
      allocate(LambdaSetLoc, source=This%LambdaSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LambdaSetLoc', ProcName=ProcName, stat=StatLoc )
    end if

    if ( This%Algorithm /= '<undefined>' ) then
      call LASSOCD( System=System, Goal=Goal, CoefficientsSet=VarR2D, ActiveSet=ActiveSet, NbActiveSet=NbActiveSet,               &
                    NbLambda=NbLambdaLoc, LambdaRatio=This%LambdaRatio, LambdaSet=LambdaSetLoc,                                   &
                    InnerTolerance=This%InnerTolerance, OuterTolerance=This%OuterTolerance, Algorithm=This%Algorithm,             &
                    MaxNbFeatures=MaxNbFeatures )
    else
      call LASSOCD( System=System, Goal=Goal, CoefficientsSet=VarR2D, ActiveSet=ActiveSet, NbActiveSet=NbActiveSet,               &
                    NbLambda=NbLambdaLoc, LambdaRatio=This%LambdaRatio, LambdaSet=LambdaSetLoc,                                   &
                    InnerTolerance=This%InnerTolerance, OuterTolerance=This%OuterTolerance, MaxNbFeatures=MaxNbFeatures )
    end if

    allocate(NbInactiveSet(NbLambdaLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='NbInactiveSet', ProcName=ProcName, stat=StatLoc )
    NbInactiveSet = 0

    allocate(InactiveSet(NbLambdaLoc), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InactiveList', ProcName=ProcName, stat=StatLoc )

    allocate(VarI1D(maxval(NbActiveSet)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
    VarI1D = 0

    i = 1
    do i = 1, NbLambdaLoc
      ii = 1
      if ( i > 1 ) then
        if( NbInactiveSet(i-1) > 0 ) VarI1D(1:NbInactiveSet(i-1)) = 0
      end if

      do ii = 1, NbActiveSet(i)
        if ( VarR2D(ActiveSet(ii),i) == Zero ) then
          NbInactiveSet(i) = NbInactiveSet(i) + 1
          VarI1D(NbInactiveSet(i)) = ii
        end if
      end do
      if ( NbInactiveSet(i) > 0 ) then
        call InactiveSet(i)%Set( Values=VarI1D(1:NbInactiveSet(i)) )
      else
        call InactiveSet(i)%Set( Values=VarI1D(1:1) )
      end if
    end do
    
    deallocate(VarI1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

    select type (CVErrorMethod => This%CVError)
      type is (CVErrorLOO_Type)
        LOOCVFlag = .true.
      class default
        LOOCVFlag = .false.
    end select

    if ( This%RestrictNbFeatures .and. LOOCVFlag ) then
      call This%SelectModelCVLOO_OD( System=System, Goal=Goal,ActiveSet=ActiveSet, NbActiveSet=NbActiveSet,                       &
                                     InactiveSet=InactiveSet, NbInactiveSet=NbInactiveSet, ModelSet=ModelSet,                     &
                                     CoefficientsSet=CoefficientsSet, CVError=CVErrorLoc )
    else

      CVErrorLoc = huge(One)

      call OLS%Construct( CVErrorMethod=This%CVError )

      NbCandidates = size(NbActiveSet,1)

      OFCounter = 0
      OFTrip = max(This%CVStopMinIter, ceiling(This%CVStopRatio * NbCandidates))

      allocate(SystemLoc(size(System,1),maxval(NbActiveSet-NbInactiveSet)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )
      SystemLoc = Zero

      allocate(FinalCoefficientsSet(size(SystemLoc,2)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='FinalCoefficientsSet', ProcName=ProcName, stat=StatLoc )
      FinalCoefficientsSet = Zero

      allocate(IndicesSet(size(SystemLoc,2)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='IndicesSet', ProcName=ProcName, stat=StatLoc )

      allocate(FinalIndicesSet(size(SystemLoc,2)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='IndicesSet', ProcName=ProcName, stat=StatLoc )

      FinalNbCoefficients = 0

      i = 1
      do i = 1, NbCandidates

        if ( associated(InactiveSetPtr) ) nullify(InactiveSetPtr)
        call InactiveSet(i)%GetPointer( Values=InactiveSetPtr )



        if ( i > 1 ) then
          if ( NbActiveSet(i) == NbActiveSet(i-1) .and. NbInactiveSet(i) == NbInactiveSet(i-1) )then
            if ( all(ActiveSet(1:NbActiveSet(i)) == ActiveSet(1:NbActiveSet(i-1))) .and.                                          & 
                                                                                   all(InactiveSetPtr == InactiveSetPtrM1) ) then
              OFCounter = OFCounter + 1
              if ( OFCounter > OFTrip ) exit
              cycle
            end if
          end if
        end if
        NbActive = NbActiveSet(i) - NbInactiveSet(i)

        if ( NbInactiveSet(i) > 0 ) then
          ii = 1
          iii = 0
          do ii = 1, NbActiveSet(i)
            if (any( ii == InactiveSetPtr ) ) cycle
            iii = iii + 1
            IndicesSet(iii) = ActiveSet(ii)
          end do
        else
          IndicesSet(1:NbActive) = ActiveSet(1:NbActive)
        end if

        SystemLoc(:,1:NbActive) = System(:,IndicesSet(1:NbActive))

        call OLS%SolveSystem( System=SystemLoc(:,1:NbActive), Goal=Goal, Coefficients=VarR1D, CVError=VarR0D )

        if ( VarR0D < CVErrorLoc ) then
          CVErrorLoc = VarR0D
          FinalCoefficientsSet(1:NbActive) = VarR1D
          FinalNbCoefficients = NbActive
          FinalIndicesSet(1:NbActive) = IndicesSet(1:NbActive)
          OFCounter = 0
        else
          OFCounter = OFCounter + 1
          if ( OFCounter > OFTrip ) exit
        end if

        InactiveSetPtrM1 => InactiveSetPtr

      end do

      deallocate(SystemLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )

      allocate(CoefficientsSet(FinalNbCoefficients), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
      CoefficientsSet = FinalCoefficientsSet(1:FinalNbCoefficients)
      
      allocate(ModelSet(FinalNbCoefficients), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
      ModelSet = FinalIndicesSet(1:FinalNbCoefficients)        

      deallocate(IndicesSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='IndicesSet', ProcName=ProcName, stat=StatLoc )

      deallocate(FinalIndicesSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='FinalIndicesSet', ProcName=ProcName, stat=StatLoc )

      deallocate(FinalCoefficientsSet, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='FinalCoefficientsSet', ProcName=ProcName, stat=StatLoc )

    end if

    deallocate(InactiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InactiveSet', ProcName=ProcName, stat=StatLoc )

    deallocate(NbInactiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NbInactiveSet', ProcName=ProcName, stat=StatLoc )

    if ( present(CVError) ) CVError = CVErrorLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull( This, System, Goal, Coefficients, CVError, Debug )

    class(LASSOSubSetMCV_Type), intent(in)                            ::    This
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
  subroutine SelectModelCVLOO_OD( This, System, Goal, ActiveSet, NbActiveSet, InactiveSet, NbInactiveSet, ModelSet,               &
                                                                                                 CoefficientsSet, CVError, Debug )

    class(LASSOSubSetMCV_Type), intent(in)                            ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, dimension(:), intent(in)                                 ::    ActiveSet
    integer, dimension(:), intent(in)                                 ::    NbActiveSet
    type(List1D_Type), dimension(:), intent(in)                       ::    InactiveSet
    integer, dimension(:), intent(in)                                 ::    NbInactiveSet
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), intent(out)                                            ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SelectModelCVLOO_OD'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    BestModel=0
    real(rkp), allocatable, dimension(:)                              ::    BestCoeffs
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:,:)                            ::    R
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp)                                                         ::    CorrFactor=Zero
    real(rkp)                                                         ::    CVErrorLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_2
    integer                                                           ::    N
    integer                                                           ::    M
    integer                                                           ::    P
    integer                                                           ::    PT
    integer                                                           ::    i, ii, iii, iv, v
    integer                                                           ::    im1, iim1, iiim1, iiip1
    real(rkp)                                                         ::    GoalVariance
    integer                                                           ::    NbCandidates
    integer, dimension(:), pointer                                    ::    InactiveSetPtr=>null()
    integer, dimension(:), pointer                                    ::    InactiveSetPtrM1=>null()
    logical                                                           ::    Flag1
    logical                                                           ::    Flag2
    integer, allocatable, dimension(:)                                ::    IndicesSet
    integer, allocatable, dimension(:)                                ::    BestIndices
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    WNorm
    integer                                                           ::    OFCounter
    integer                                                           ::    OFTrip

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    M = size(System,1)
    N = size(System,2)

    if ( N < 1 ) call Error%Raise( Line='Passed an empty system', ProcName=ProcName )

    NbCandidates = size(NbActiveSet,1)
    P = maxval(NbActiveSet,1)

    OFCounter = 0
    OFTrip = max(This%CVStopMinIter, ceiling(This%CVStopRatio * NbCandidates))

    GoalVariance = ComputeSampleVar(Values=Goal)

    if ( GoalVariance <= Zero ) GoalVariance = tiny(One)

    allocate(IndicesSet(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='IndicesSet', ProcName=ProcName, stat=StatLoc )
    IndicesSet = 0

    allocate(BestIndices(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='BestIndices', ProcName=ProcName, stat=StatLoc )
    BestIndices = 0

    allocate(VarR1D_1(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D_1', ProcName=ProcName, stat=StatLoc )
    VarR1D_1 = Zero

    allocate(VarR1D_2(max(M,P)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D_2', ProcName=ProcName, stat=StatLoc )
    VarR1D_2 = Zero

    allocate(BestCoeffs(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='BestCoeffs', ProcName=ProcName, stat=StatLoc )
    BestCoeffs = Zero

    allocate(HatDiag(M), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )
    HatDiag = Zero

    allocate(Q1(M,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q1', ProcName=ProcName, stat=StatLoc )
    Q1 = Zero

    allocate(R(P,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q1', ProcName=ProcName, stat=StatLoc )
    R = Zero

    Q1 = Zero

    CVError = huge(CVError)
    BestModel = 0

    P = 0
    i = 1
    do i = 1, NbCandidates

      nullify( InactiveSetPtr )

      call InactiveSet(i)%GetPointer( Values=InactiveSetPtr )

      if ( i > 1 ) then
        if ( NbActiveSet(i) == NbActiveSet(i-1) .and. NbInactiveSet(i) == NbInactiveSet(i-1) )then
          if ( all(ActiveSet(1:NbActiveSet(i)) == ActiveSet(1:NbActiveSet(i-1))) .and. all(InactiveSetPtr == InactiveSetPtrM1) )  &
                                                                                                                              then
            OFCounter = OFCounter + 1
            if ( OFCounter > OFTrip ) exit
            cycle
          end if
        end if
      end if

      ! updating q and r
      if ( i > 1 ) then
        ii = 1
        iii = 0
        do ii = 1, NbActiveSet(i)
          iii = iii + 1
          Flag1 = any(ii == InactiveSetPtr)
          if ( ii <= NbActiveSet(i-1) ) then
            Flag2 = any(ii == InactiveSetPtrM1 )
            if ( Flag1 .and. Flag2 ) then
              iii = iii - 1
              cycle
            elseif ( Flag1 ) then
              PT = P - 1
              call dqrdec( M, P, P, Q1(:,1:P), M, R(1:P,1:P), P, iii, VarR1D_2(1:max(1,P-iii)) )
              P = PT
              iii = iii - 1
            elseif ( Flag2 ) then
              PT = P + 1
              call dqrinc( M, P, P, Q1(:,1:PT), M, R(1:PT,1:PT), PT, iii, System(:,ActiveSet(ii)), VarR1D_2(1:P))
              P = PT
              IndicesSet(iii) = ActiveSet(ii)
            else
              IndicesSet(iii) = ActiveSet(ii)
            end if
          else
            if ( Flag1 ) then
              iii = iii - 1
              cycle
            end if
            PT = P + 1
            call DGEMV( 'T', M, P, 1.d0, Q1(:,1:PT), M, System(:,ActiveSet(ii)), 1, 0.d0, VarR1D_2(1:P), 1 )
            WNorm = ComputeNorm(Vector=VarR1D_2(1:P), Norm=2)
            VarR0D = dsqrt(ComputeNorm(Vector=System(:,ActiveSet(ii)), Norm=2)**2 - WNorm**2)
            iv = 1
            do iv = 1, M
              VarR1D_1(1:P) = Q1(iv,1:P)
              Q1(iv,PT) = (System(iv,ActiveSet(ii)) - dot_product(VarR1D_1(1:P),VarR1D_2(1:P))) / VarR0D
            end do
            R(1:P,PT) = VarR1D_2(1:P)
            R (PT,PT) = VarR0D
            P = PT
            IndicesSet(iii) = ActiveSet(ii)
          end if
        end do
      else
        ii = 1
        do ii = 1, NbActiveSet(i)
          if ( any(ii == InactiveSetPtr) ) cycle
          PT = P + 1
          if ( ii > 1 ) then
            call DGEMV( 'T', M, P, 1.d0, Q1(:,1:PT), M, System(:,ActiveSet(ii)), 1, 0.d0, VarR1D_2(1:P), 1 )
            WNorm = ComputeNorm(Vector=VarR1D_2(1:P), Norm=2)
            VarR0D = dsqrt(ComputeNorm(Vector=System(:,ActiveSet(ii)), Norm=2)**2 - WNorm**2)
            iv = 1
            do iv = 1, M
              VarR1D_1(1:P) = Q1(iv,1:P)
              Q1(iv,PT) = (System(iv,ActiveSet(ii)) - dot_product(VarR1D_1(1:P),VarR1D_2(1:P))) / VarR0D
            end do
            R(1:P,PT) = VarR1D_2(1:P)
            R (PT,PT) = VarR0D
          else
            R(1,1) = ComputeNorm( Vector=System(:,ActiveSet(ii)), Norm=2 )
            Q1(:,1) = System(:,ActiveSet(ii)) / R(1,1)
          end if
          P = PT
        end do
        IndicesSet(1:NbActiveSet(1)) = ActiveSet(1:NbActiveSet(1))
      end if

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

      ! computing hat diagonals
      HatDiag = sum(Q1(:,1:P)**2,2)

      ! calculating error
      CVErrorLoc = Zero
      ii = 1
      do ii = 1, M
        iii = 1
        VarR1D_1(1:P) = System(ii, IndicesSet(1:P))
        CVErrorLoc = CVErrorLoc + ( (Goal(ii)-dot_product(VarR1D_2(1:P), VarR1D_1(1:P))) / (One-HatDiag(ii)) )**2
      end do
      CVErrorLoc = CVErrorLoc / real(M,rkp)

      if ( This%CVError%IsNormalized() ) CVErrorLoc = CVErrorLoc / GoalVariance

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

          VarR1D_1(ii:P) = VarR1D_1(ii:P)*VarR1D_1(ii:P)
          CorrFactor = CorrFactor + sum( VarR1D_1(ii:P) )
        end do

        CorrFactor = ( real(M,rkp) / ( real(M,rkp) - real(P,rkp) ) ) * ( One + CorrFactor )
        CVErrorLoc = CVErrorLoc * CorrFactor
      end if

      ! checking if the current metamodel has lower cv error
      if ( CVErrorLoc < CVError ) then
        CVError = CVErrorLoc
        BestModel = i
        BestIndices = IndicesSet
        BestCoeffs(1:P) = VarR1D_2(1:P)
        OFCounter = 0
      else
        OFCounter = OFCounter + 1
        if ( OFCounter > OFTrip ) exit
      end if

      InactiveSetPtrM1 => InactiveSetPtr
      IndicesSet(1:P) = 0

    end do

    nullify( InactiveSetPtr )

    deallocate(Q1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Q1', ProcName=ProcName, stat=StatLoc )

    deallocate(R, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='R', ProcName=ProcName, stat=StatLoc )

    deallocate(HatDiag, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='HatDiag', ProcName=ProcName, stat=StatLoc )

    deallocate(VarR1D_1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_1', ProcName=ProcName, stat=StatLoc )

    deallocate(VarR1D_2, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_2', ProcName=ProcName, stat=StatLoc )

    allocate(ModelSet(NbActiveSet(BestModel)-NbInactiveSet(BestModel)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )

    allocate(CoefficientsSet(NbActiveSet(BestModel)-NbInactiveSet(BestModel)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )

    call InactiveSet(BestModel)%GetPointer( Values=InactiveSetPtr )

    ModelSet(1:(NbActiveSet(BestModel)-NbInactiveSet(BestModel)))=BestIndices(1:(NbActiveSet(BestModel)-NbInactiveSet(BestModel)))

    CoefficientsSet = BestCoeffs(1:size(CoefficientsSet,1))

    deallocate(BestIndices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='BestIndices', ProcName=ProcName, stat=StatLoc )

    deallocate(BestCoeffs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='BestCoefficients', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LASSOSubSetMCV_Type), intent(out)                           ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()

    select type (RHS)
      type is (LASSOSubSetMCV_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%LambdaRatio = RHS%LambdaRatio
          LHS%NbLambda = RHS%NbLambda
          LHS%InnerTolerance = RHS%InnerTolerance
          LHS%OuterTolerance = RHS%OuterTolerance
          LHS%Algorithm = RHS%Algorithm
          LHS%RestrictNbFeatures = RHS%RestrictNbFeatures
          LHS%CVStopRatio = RHS%CVStopRatio
          LHS%CVStopMinIter = RHS%CVStopMinIter
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

end module
