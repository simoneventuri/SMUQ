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

module LinSolverLAR_Class

use Input_Library
use Parameters_Library
use StatisticsRoutines_Module
use ComputingRoutines_Module
use QRUpdate_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use CVMethod_Factory_Class                                        ,only:    CVMethod_Factory
use CVMethod_Class                                                ,only:    CVMethod_Type, CVFitTarget
use CVLOO_Class                                                   ,only:    CVLOO_Type

implicit none

private

public                                                                ::    LinSolverLAR_Type

type, extends(LinSolverMethod_Type)                                   ::    LinSolverLAR_Type
  real(rkp)                                                           ::    MinAbsCorr
  logical                                                             ::    Hybrid
  logical                                                             ::    ModifiedCV
  logical                                                             ::    GetBest
  logical                                                             ::    StopEarly
  class(CVMethod_Type), allocatable                                   ::    CVError
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Solve
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(LinSolverLAR_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'LinSolverLAR'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(LinSolverLAR_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Initialized = .false.
  This%Constructed = .false.

  if (allocated(This%CVError)) deallocate(This%CVError, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%LARMethod', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(LinSolverLAR_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%MinAbsCorr = epsilon(This%MinAbsCorr)*100.0_rkp
  This%Hybrid = .true.
  This%ModifiedCV = .true.
  This%GetBest = .true.
  This%StopEarly = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(LinSolverLAR_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    VarL0D
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    Found

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'minimum_correlation'
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%MinAbsCorr = VarR0D

  Parametername = 'hybrid'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Hybrid = VarL0D

  ParameterName = 'modified_cross_validation'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%ModifiedCV = VarL0D

  ParameterName = 'get_best'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%GetBest = VarL0D

  ParameterName = 'stop_early'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%StopEarly = VarL0D

  SectionName = 'cross_validation'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call CVErrorMethod_Factory%Construct(Object=This%CVError, Input=InputSection, Prefix=PrefixLoc)
  else
    allocate(CVLOO_Type :: This%CVError)
    select type (CVMethod => This%CVError)
      type is (CVLOO_Type)
        call CVMethod%Construct(Normalized=.true.)
      class default
        call Error%Raise(Line='Something went wrong', ProcName=ProcName)
    end select
  end if

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, CVMethod, Hybrid, MinAbsCorr, GetBest, StopEarly, ModifiedCV)

  class(LinSolverLAR_Type), intent(inout)                             ::    This
  class(CVMethod_Type), optional, intent(in)                          ::    CVMethod
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  logical, optional, intent(in)                                       ::    StopEarly
  logical, optional, intent(in)                                       ::    ModifiedCV
  real(rkp), optional, intent(in)                                     ::    MinAbsCorr

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(Hybrid)) This%Hybrid = Hybrid

  if (present(MinAbsCorr)) This%MinAbsCorr = MinAbsCorr

  if (present(GetBest)) This%GetBest = GetBest

  if (present(StopEarly)) This%StopEarly = StopEarly

  if (present(ModifiedCV)) This%ModifiedCV = ModifiedCV

  if (present(CVMethod)) then
    allocate(This%CVError, source=CVMethod, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%CVErrorMethod', ProcName=ProcName, stat=StatLoc)
  else
    allocate(CVLOO_Type :: This%CVError)
    select type (CVError => This%CVError)
      type is (CVLOO_Type)
        call CVError%Construct(Corrected=.true.)
      class default
        call Error%Raise(Line='Something went wrong', ProcName=ProcName)
    end select
  end if

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(LinSolverLAR_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%AddValue(Name='hybrid', Value=ConvertToString(Value=This%Hybrid))
  call GetInput%AddValue(Name='stop_early', Value=ConvertToString(Value=This%StopEarly))
  call GetInput%AddValue(Name='get_best', Value=ConvertToString(Value=This%GetBest))
  call GetInput%AddValue(Name='modified_cross_validation', Value=ConvertToString(Value=This%ModifiedCV))
  call GetInput%AddValue(Name='minimum_correlation', Value=ConvertToString(Value=This%MinAbsCorr))

  SectionName = 'cross_validation'
  GetInput = CVMethod_Factory%GetObjectInput(Object=This%LARMethod, Name=SectionName, Prefix=PrefixLoc, Directory=DirectoryLoc)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Solve(This, System, Goal, Coefficients, CVError)

  class(LinSolverLAR_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='Solve'
  integer                                                             ::    StatLoc=0
  procedure(CVFitTarget), pointer                                     ::    CVFit=>null()
  integer                                                             ::    M
  integer                                                             ::    N

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients and system arrays', ProcName=ProcName)

  if (present(CVError)) then
    select type (CVMethod=>This%CVError)
      type is (CVLOO_Type)
        call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, CVLOO=CVError, Hybrid=This%Hybrid, &
                                   GetBest=This%GetBest, MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, &
                                   ModifiedCVLOO=This%ModifiedCV)
        if (.not. CVMethod%IsNormalized .and. CVError < huge(One)) CVError = CVError * ComputeSampleVar(Values=Goal)
      class default
        CVFit => CVFitLAR
        call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid, GetBest=This%GetBest, &
                                   MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, ModifiedCVLOO=This%ModifiedCV)
        CVError = CVMethod%Calculate(Fit=CVFit, Data=Goal)
        if (This%ModifiedCV) CVError = CVError * ComputeCorrectionFactor(System=System, Coefficients=Coefficients)
        nullify(CVFit)
    end select
  else
    call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid, GetBest=This%GetBest, &
                               MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, ModifiedCVLOO=This%ModifiedCV)
  end if

  contains

  !!----------------------------------------------------------------------------------------------------------------------------
  subroutine CVFitLAR(TrainingSet, TrainingSetIndices, ValidationSet, ValidationSetIndices, Residual)

    real(rkp), dimension(:), intent(inout)                          ::    TrainingSet
    integer, dimension(:), intent(in)                               ::    TrainingSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    ValidationSet
    integer, dimension(:), intent(in)                               ::    ValidationSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    Residual

    character(*), parameter                                         ::    ProcName='CVFitLAR'
    integer                                                         ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                          ::    SystemLoc
    real(rkp), allocatable, dimension(:)                            ::    CoefficientsLoc
    integer                                                         ::    NbTraining
    integer                                                         ::    NbValidation
    integer                                                         ::    iLoc
    integer                                                         ::    iiLoc

    NbTraining = size(TrainingSet,1)
    NbValidation = size(ValidationSet,1)

    if (size(Residual,1) /= NbValidation) call Error%Raise('Incompatible residual and validation arrays', ProcName=ProcName)

    allocate(CoefficientsLoc(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)
    CoefficientsLoc = Zero

    allocate(SystemLoc(NbTraining,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)
    SystemLoc = Zero

    iLoc = 1
    do iLoc = 1, N
      SystemLoc(:,iLoc) = System(TrainingSetIndices,iLoc)
    end do

    call This%Solve(System=SystemLoc, Goal=TrainingSet, Coefficients=CoefficientsLoc)

    Residual = ValidationSet

    iLoc = 1
    do iLoc = 1, N
      if (.not. dabs(CoefficientsLoc(iLoc)) > Zero) cycle
      iiLoc = 1
      do iiLoc = 1, NbValidation
        Residual(iiLoc) = Residual(iiLoc) - CoefficientsLoc(i)*System(ValidationIndices(iiLoc,iLoc))
      end do
    end do
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    allocate(SystemLoc(NbTraining,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)
    SystemLoc = Zero

    iLoc = 1
    do iLoc = 1, N
      SystemLoc(:,iLoc) = System(TrainingSetIndices,iLoc)
    end do

    call This%Solve(System=SystemLoc, Goal=TrainingSet, Coefficients=CoefficientsLoc)

    Residual = ValidationSet

    iLoc = 1
    do iLoc = 1, N
      if (.not. dabs(CoefficientsLoc(iLoc)) > Zero) cycle
      iiLoc = 1
      do iiLoc = 1, NbValidation
        Residual(iiLoc) = Residual(iiLoc) - CoefficientsLoc(i)*System(ValidationIndices(iiLoc,iLoc))
      end do
    end do
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    deallocate(CoefficientsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)
    deallocate(CoefficientsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(LinSolverLAR_Type), intent(out)                               ::    LHS
  class(LinSolverMethod_Type), intent(in)                             ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (LinSolverLAR_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%CVError', ProcName=ProcName, stat=StatLoc)
        LHS%Hybrid = RHS%Hybrid
        LHS%MinAbsCorr = RHS%MinAbsCorr
        LHS%StopEarly = RHS%StopEarly
        LHS%ModifiedCV = RHS%ModifiedCV
        LHS%GetBest = RHS%GetBest
      end if
    class default
      call Error%Raise(Line='Mismatching object types', ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(LinSolverLAR_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%CVError)) deallocate(This%CVError, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CVError', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine BuildMetaModel_Gram_LAR(System, Goal, Coefficients, CVLOO, Hybrid, GetBest, MinAbsCorr, StopEarly, ModifiedCVLOO)

  real(rkp), dimension(:,:), target, intent(inout)                    ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), intent(inout)                                            ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVLOO
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  logical, optional, intent(in)                                       ::    ModifiedCVLOO
  logical, optional, intent(in)                                       ::    StopEarly

  character(*), parameter                                             ::    ProcName='BuildMetaModel_Gram_LAR'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    HybridLoc
  logical                                                             ::    GetBestLoc
  logical                                                             ::    StopEarlyLoc
  real(rkp)                                                           ::    MinAbsCorrLoc
  logical                                                             ::    ModifiedCVLOOLoc
  integer                                                             ::    N
  integer                                                             ::    M
  real(rkp)                                                           ::    Nreal
  real(rkp)                                                           ::    Mreal
  integer                                                             ::    MaxNbRegressors
  integer                                                             ::    MaxNbIterations
  integer                                                             ::    ConstantIndex
  integer                                                             ::    i
  integer                                                             ::    ii
  real(rkp), allocatable, dimension(:)                                ::    Mean
  real(rkp), allocatable, dimension(:)                                ::    StD
  real(rkp)                                                           ::    GoalMean
  real(rkp)                                                           ::    GoalVariance
  integer                                                             ::    iIteration
  logical, allocatable, dimension(:)                                  ::    Active
  logical, allocatable, dimension(:)                                  ::    Skip
  real(rkp), allocatable, dimension(:)                                ::    Corr
  real(rkp)                                                           ::    MaxAbsCorr 
  integer                                                             ::    MaxAbsCorrIndex
  integer, allocatable, dimension(:)                                  ::    ActiveIndices
  integer                                                             ::    NbActiveIndices
  real(rkp), allocatable, dimension(:)                                ::    Residual
  real(rkp), allocatable, dimension(:,:), target                      ::    InvXtX
  real(rkp)                                                           ::    InvXtXScalar
  real(rkp), allocatable, dimension(:)                                ::    u1
  real(rkp), allocatable, dimension(:)                                ::    u2
  real(rkp)                                                           ::    d
  real(rkp), allocatable, dimension(:), target                        ::    VarR1D
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:)                                ::    s
  real(rkp)                                                           ::    c
  real(rkp), allocatable, dimension(:)                                ::    w
  real(rkp), allocatable, dimension(:)                                ::    u
  real(rkp)                                                           ::    aj
  real(rkp)                                                           ::    gamma
  real(rkp), allocatable, dimension(:)                                ::    h
  real(rkp)                                                           ::    CVLOOTempNonN
  real(rkp)                                                           ::    CVLOOTemp
  integer                                                             ::    CVLOOCounter
  integer                                                             ::    CVLOOTrip
  real(rkp)                                                           ::    T
  real(rkp), allocatable, dimension(:)                                ::    BestCoefficients
  integer, allocatable, dimension(:)                                  ::    BestIndices
  integer                                                             ::    BestNbIndices
  real(rkp)                                                           ::    BestCVLOO
  real(rkp)                                                           ::    BestCVLOONonN
  real(rkp), allocatable, dimension(:)                                ::    CoefficientsLoc
  integer                                                             ::    InterceptAdjustment
  real(rkp), allocatable, dimension(:,:)                              ::    Q1
  real(rkp), allocatable, dimension(:,:)                              ::    R
  real(rkp), allocatable, dimension(:)                                ::    XtY
  real(rkp), dimension(:), pointer                                    ::    v=>null()

  HybridLoc = .true.
  if (present(Hybrid)) HybridLoc=Hybrid

  StopEarlyLoc = .true.
  if (present(StopEarly )) StopEarly Loc=StopEarly 

  GetBestLoc = .true.
  if (present(GetBest)) GetBestLoc=GetBest

  ModifiedCVLOOLoc = .true.
  if (present(ModifiedCVLOO)) ModifiedCVLOOLoc=ModifiedCVLOO

  MinAbsCorrLoc = epsilon(MinAbsCorr)*100.0_rkp
  if(present(MinAbsCorr)) MinAbsCorrLoc = MinAbsCorr

  M = size(System,1)
  Mreal = real(M,rkp)
  N = size(System,2)
  Nreal = real(N,rkp) 

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array length', ProcName=ProcName)
  Coefficients = Zero

  GoalMean = ComputeMean(Values=Goal)
  GoalVariance = ComputeSampleVar(Values=Goal, Mean=GoalMean)

  if (N == 1) then
    InvXtXScalar = One / dot_product(System(:,1),System(:,1))
    Coefficients(1) = dot_product(System(:,1),Goal)*InvXtXScalar

    CVLOOTempNonN = Zero
    i = 1
    do i = 1, M 
      CVLOOTempNonN = CVLOOTempNonN + ((Goal(i)-Coefficients(1)*System(i,1))/(One-System(i,1)**2*InvXtXScalar))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(ModifiedCVLOOLoc) then
      if (M > 1) then
        T = (realM / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if

    if (present(CVLOO)) CVLOO = CVLOOTemp
    return
  end if

  allocate(Skip(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Skip', ProcName=ProcName, stat=StatLoc)
  Skip = .false.

  allocate(Mean(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Mean', ProcName=ProcName, stat=StatLoc)
  Mean = Zero

  allocate(StD(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='StD', ProcName=ProcName, stat=StatLoc)
  StD = Zero

  ConstantIndex = 0

  i = 1
  do i = 1, N 
    Skip(i) = IsArrayConstant(Array=System(:,i))
    if (Skip(i) .and. .not. dabs(System(1,i)) > Zero) call Error%Raise('System has a column of zeros', ProcName=ProcName)
    Mean(i) = ComputeMean(Values=System(:,i))
    if (.not. Skip(i)) then
      StD(i) = ComputeSampleVar(Values=System(:,i), Mean=Mean(i))
      System(:,i) = (System(:,i) - Mean(i)) / StD(i)
    else
      if (ConstantIndex == 0) ConstantIndex = i
      StD(i) = Zero
      System(:,i) = Zero
    end if
  end do

  if (ConstantIndex /= 0) Goal = Goal - GoalMean

  T = Zero
  CVLOOCounter = 0

  VarR0D = Zero

  InterceptAdjustment = 0
  if (ConstantIndex /= 0) InterceptAdjustment = 1

  MaxNbRegressors = min(M-InterceptAdjustment,N)

  allocate(Corr(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Corr', ProcName=ProcName, stat=StatLoc)
  Corr = Zero

  allocate(u1(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u1', ProcName=ProcName, stat=StatLoc)
  u1 = Zero

  allocate(u2(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u2', ProcName=ProcName, stat=StatLoc)
  u2 = Zero

  d = Zero

  allocate(ActiveIndices(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ActiveIndices', ProcName=ProcName, stat=StatLoc)
  ActiveIndices = 0
  NbActiveIndices = 0

  allocate(Active(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Inactive', ProcName=ProcName, stat=StatLoc)
  Active = .false.

  allocate(s(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='s', ProcName=ProcName, stat=StatLoc)
  s = Zero

  allocate(w(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='w', ProcName=ProcName, stat=StatLoc)
  w = Zero

  allocate(u(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u', ProcName=ProcName, stat=StatLoc)
  u = Zero

  allocate(h(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)

  allocate(CoefficientsLoc(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)
  CoefficientsLoc = Zero

  allocate(BestCoefficients(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='BestCoefficients', ProcName=ProcName, stat=StatLoc)
  BestCoefficients = Zero

  allocate(BestIndices(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='BestIndices', ProcName=ProcName, stat=StatLoc)
  BestIndices = 0

  allocate(Residual, source=Goal, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

  allocate(InvXtX(MaxNbRegressors,MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='InvXtX', ProcName=ProcName, stat=StatLoc)

  if (HybridLoc) then
    allocate(XtY(MaxnbRegressors), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='XtY', ProcName=ProcName, stat=StatLoc)
    XtY = Zero
  end if

  allocate(VarR1D(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  VarR1D = Zero

  BestCVLOO = huge(One)
  BestCVLOONonN = huge(One)
  BestNbIndices = 0

  MaxNbIterations = MaxNbRegressors
  NbActiveIndices = 0
  iIteration = 0
  CVLOOTemp = huge(One)
  CVLOOTempNonN = huge(One)
  CVLOOCounter = 0
  T = Zero
  CVLOOTrip = max(nint(real(MaxNbIterations,rkp)*0.1),100)

  ! do ols for constant regressor solution if there is a constant regressor
  if (ConstantIndex /= 0) then
    InvXtXScalar = One / (Mreal*Mean(ConstantIndex)**2)

    CVLOOTempNonN = Zero
    i = 1
    do i = 1, M 
      CVLOOTempNonN = CVLOOTempNonN + (Goal(i)/(One-One/Mreal))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(ModifiedCVLOOLoc) then
      if (M > 1) then
        T = (realM / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    BestCVLOO = CVLOOTemp
    BestCVLOONonN = CVLOOTempNonN
  end if

  ! begin loop
  do
    iIteration = iIteration + 1
    if (iIteration > MaxNbIterations) exit

    ! finding most correlated regressor from inactive and non-constant list
    MaxAbsCorr = Zero
    MaxAbsCorrIndex = 0
    i = 1
    do i = 1, N
      if (Active(i)) cycle
      if (Skip(i)) cycle
      Corr(i) = dot_product(System(:,i), Residual)
      if (dabs(Corr(i)) < MaxCorr) cycle
      MaxAbsCorr = dabs(Corr(i))
      MaxAbsCorrIndex = i
    end do

    if (.not. MaxAbsCorr > MinAbsCorrLoc) exit

    ! Updating inverse locally to reduce memory requirements and alloc/dealloc times
    v => System(:,MaxAbsCorrIndex)
    if (NbActiveIndices == 0) then
      InvXtX(1,1) = One / dot_product(v,v)
      if (.not. IsFinite(Value=InvXtX(1,1))) then
        Skip(MaxAbsCorrIndex) = .true.
        write(*,'(A)') 'Warning: Ignoring regressor ' // ConvertToString(Value=MaxAbsCorrIndex) // 'due to instability'
        cycle
      end if
    else
      i = 1
      do i = 1, NbActiveIndices
        u1(i) = dot_product(System(:,ActiveIndices(i)), v)
      end do
  
      i = 1
      do i = 1, NbActiveIndices
        VarR1D(1:NbActivesIndices) = InvXtX(i,1:NbActiveIndices)
        u2(i) = dot_product(VarR1D, u1(1:NbActiveIndices))
      end do
  
      d = One / (dot_product(v,v) - dot_product(u1(1:NbActiveIndices), u2(1:NbActiveIndices)))
  
      VarR0D = dot_product(u2(1:NbActiveIndices), u2(1:NbActiveIndices))
  
      if ((.not. IsFinite(Values=u1)) .or. (.not. IsFinite(Values=u2)) .or. (.not. IsFinite(Value=d)) &
            .or. (.not. IsFinite(Value=VarR0D))) then
        Skip(MaxAbsCorrIndex) = .true.
        write(*,'(A)') 'Warning: Ignoring regressor ' // ConvertToString(Value=MaxAbsCorrIndex) // 'due to instability'
        cycle
      end if

      i = 1
      do i = NbActiveIndices
        InvXtX(1:NbActiveIndices,i) = InvXtX(1:NbActiveIndices,i) + d*VarR0D
      end do
      InvXtX(1:NbActiveIndices,NbActiveIndices+1) = -d*u2(1:NbActiveIndices)
      InvXtX(NbActivesIndices+1,1:NbActiveIndices) = -d*u2(1:NbActiveIndices)
      InvXtX(NbActivesIndices+1,NbActivesIndices+1) = d
    end if
    nullify(v)

    NbActiveIndices = NbActiveIndices + 1
    ActiveIndices(NbActiveIndices) = MaxAbsCorrIndex
    Active(MaxAbsCorrIndex) = .true.

    ! s of active set
    s(NbActiveIndices) = sign(Corr(MaxAbsCorrIndex))

    ! c of active set
    c = ZeroGoal
    i = 1
    do i = 1, NbActiveIndices
      c = c + dot_product(s(1:NbActiveIndices),InvXtX(1:NbActiveIndices,i))*s(i)
    end do
    c = One/dsqrt(c)

    ! w of active set
    w(1:NbActiveIndices) = Zero
    i = 1
    do i = 1, NbActiveIndices
      w(1:NbActiveIndices) = w(1:NbActiveIndices) + invXtX(1:NbActiveIndices,i)*s(i)
    end do
    w(1:NbActiveIndices) = w(1:NbActiveIndices)*c 

    ! u
    u = Zero
    i = 1
    do i = 1, NbActiveIndices
      u = u + System(:,ActiveIndices(i)) * w(i)
    end do

    ! step length
    gamma = huge(One)
    if (iIteration < NbMaxIterations) then
      i  = 1
      do i = 1, N 
        if (Active(i)) cycle
        if (Skip(i)) cycle
        aj = dot_product(System(:,i),u)
        VarR0D = (MaxAbsCorr-Corr(i))/(c-aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
        VarR0D = (MaxAbsCorr+Corr(i))/(c+aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
      end do
    else
      gamma = MaxAbsCorr / c
    end if

    ! residual update
    Residual = Residual - gamma*U

    ! udpate coefficients
    if (.not. HybridLoc) then
      CoefficientsLoc(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices) + gamma*w(1:NbActiveIndices)
    else
      CoefficientsLoc(1:NbActiveIndices) = Zero
      XtY(NbActiveIndices) = dot_product(System(:,ActiveIndices(NbActiveIndices)),Goal)
      i = 1
      do i = 1, NbActiveIndices
        CoefficientsLoc(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices) + XtY(i) * InvXtx(:,i)
      end do
    end if

    ! compute LOO cv error
    ! first get leverage
    h = Zero
    i = 1
    do i = 1, M
      VarR1D(1:NbActiveIndices) = System(i,ActiveIndices(1:NbActiveIndices))
      v => VarR1D(1:NbActiveIndices)
      ii = 1
      do ii = 1:NbActiveIndices
        h(i) = h(i) + dot_product(v,InvXtX(1:NbActiveIndices,ii))*v(ii)
      end do
      nullify(v)
    end do
  
    ! adjust for centralized regressors when intercept is present
    if (ConstantIndex /= 0) h = h + One/Mreal
  
    ! compute cv loo
    CVLOOTempNonN = Zero
    CVLOOTemp = Zero
    if (HybridLoc) then
      VarR1D(1:M) = Zero
      i = 1
      do i = 1, NbActiveIndices
        VarR1D(1:M) = VarR1D(1:M) + CoefficientsLoc(i)*System(:,ActiveIndices(i))
      end do
      i = 1
      do i = 1, M
        CVLOOTempNonN = CVLOOTempNonN + ((Goal(i)-VarR1D(i))/(One-h(i)))**2
      end do
      CVLOOTempNonN = CVLOOTempNonN / Mreal
    else
      VarR1D(1:M) = Residual / (One-h)
      CVLOOTempNonN = dot_product(VarR1D(1:M), VarR1D(1:M)) / Mreal
    end if
  
    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    ! get correction factor if needed
    if (ModifiedCVLOOLoc) then
      if (M > NbActiveIndices + InterceptAdjustment) then
        if (ConstantIndex /= 0) then
          T = Zero
          i = 1
          do i = 1, NbActiveIndices
            T = T + InvXtX(i,i)*One/StD(ActiveIndices(i))**2
          end do
          i = 1
          do i = 1, NbActiveIndices
            ii = ActiveIndices(i)
            VarR1D(i) = sum(System(:,ii)) + Mreal*Mean(ii)/StD(ii)
          end do
          VarR0D = Zero
          i = 1
          do i = 1, NbActiveIndices
            VarR0D = VarR0D + dot_product(VarR1D(1:NbActiveIndices),InvXtX(1:NbActiveIndices,i))*VarR1D(i)
          end do
          T = T + One/(Mean(ConstantIndex)**2*Mreal)  + One/(Mean(ConstantIndex)*Mreal)**2*VarR0D
        else
          T = Zero
          i = 1
          do i = 1, NbActiveIndices
            T = T + InvXtX(i,i)
          end do
        end if
        T = (Mreal/real(M-NbActiveIndices-InterceptAdjustment,rkp))*(One+T)
        CVLOOTemp = CVLOOTemp*T
        ! test for NaN
        if (CVLOOTemp /= CVLOOTemp) CVLOOTemp = huge(One)
      else
        CVLOOTemp = huge(One)
      end if
    end if
  
    if (CVLOOTemp < BestCVLOO) then
      BestCVLOONonN = CVLOOTempNonN
      BestCVLOO = CVLOOTemp
      BestCoefficients(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices)
      BestIndices(1:NbActiveIndices) = ActiveIndices(1:NbActiveIndices)
      BestNbIndices = NbActiveIndices
      CVLOOCounter = 0
    else
      CVLOOCounter = CVLOOCounter + 1
    end if

    if (CVLOOCounter >= CVLOOTrip) exit

  end do

  if (GetBestLoc) then
    if(present(CVLOO)) CVLOO = BestCVLOO
    i = 1
    do i = 1, BestNbIndices
      Coefficients(BestIndices(i)) = BestCoefficients(i) / StD(BestIndices(i))
    end do
  else
    if(present(CVLOO)) CVLOO = CVLOOTemp
    i = 1
    do i = 1, NbActiveIndices
      Coefficients(ActiveIndices(i)) = CoefficientsLoc(i) / StD(ActiveIndices(i))
    end do
  end if

  if (ConstantIndex /= 0) then
    Coefficients(ConstantIndex) = Zero
    Coefficients(ConstantIndex) = (GoalMean - dot_product(Coefficients,Mean)) / Mean(ConstantIndex)
  end if

  i = 1
  do i = 1, N
    System(:,i) = System(:,i)*StD(i)+Mean(i)
  end do
  Goal = Goal + GoalMean

  deallocate(Q1, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

  deallocate(R, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

  deallocate(Mean, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Mean', ProcName=ProcName, stat=StatLoc)

  deallocate(StD, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='StD', ProcName=ProcName, stat=StatLoc)

  deallocate(Active, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Active', ProcName=ProcName, stat=StatLoc)

  deallocate(Skip, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Skip', ProcName=ProcName, stat=StatLoc)

  deallocate(Corr, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Corr', ProcName=ProcName, stat=StatLoc)

  deallocate(ActiveIndices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='ActiveIndices', ProcName=ProcName, stat=StatLoc)

  deallocate(InvXtX, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InvXtX', ProcName=ProcName, stat=StatLoc)

  deallocate(u1, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='u1', ProcName=ProcName, stat=StatLoc)

  deallocate(u2, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='u2', ProcName=ProcName, stat=StatLoc)

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  deallocate(s, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='s', ProcName=ProcName, stat=StatLoc)

  deallocate(w, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='w', ProcName=ProcName, stat=StatLoc)

  deallocate(u, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='u', ProcName=ProcName, stat=StatLoc)

  deallocate(BestCoefficients, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='BestCoefficients', ProcName=ProcName, stat=StatLoc)

  deallocate(BestIndices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='BestIndices', ProcName=ProcName, stat=StatLoc)

  deallocate(CoefficientsLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

  if (allocated(XtY)) deallocate(XtY, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='XtY', ProcName=ProcName, stat=StatLoc)

  ! recompute loo values
  if (ConstantIndex == 0 .and. present(CVLOO)) then
    NbActiveIndices = count(dabs(Coefficients) > Zero)

    allocate(Q1(M,NbActiveIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
    Q1 = Zero

    allocate(R(NbActiveIndices,NbActiveIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)
    R = Zero

    ii = 0
    i = 1
    do i = 1, N
      if (.not. dabs(Coefficients(i)) > Zero) cycle
      ii = ii + 1
      Q1(:,ii) = System(:,i) 
    end do
    if (NbActiveIndices /= ii) call Error%Raise('Something went wrong', ProcName=ProcName)

    call ComputeQR(Q=Q1, R=R, LowerR=.true.)

    ! get leverage from Q1
    h = sum(Q1**2,2)

    ! get cv loo
    Residual = Zero
    i = 1
    do i = 1, N
      if (.not. dabs(Coefficients(i)) > Zero) cycle
      Residual = Residual + Coefficients(i)*System(:,i)
    end do

    CVLOOTempNonN = Zero
    CVLOOTemp = Zero
    i = 1
    do i = 1, M
      CVLOOTempNonN = CVLOOTempNonN + (Residual(i)/(One-h(i)))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal
    
    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if (ModifiedCVLOOLoc) then
      if (M > NbActiveIndices) then
        call DTRTRI('L', 'N', NbActiveIndices, R, NbActiveIndices, StatLoc)
        if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                            ProcName=ProcName)
        T = Zero
        i = 1
        do i = 1, NbActiveIndices
          T = T + dot_product(R(i:NbActiveIndices,i),R(i:NbActiveIndices,i))
        end do
        T = (Mreal/real(M-NbActiveIndices,rkp))*(One+T)
        CVLOOTemp = CVLOOTemp*T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    CVLOO = CVLOOTemp

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
  
    deallocate(R, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)
  end if

  deallocate(h, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='h', ProcName=ProcName, stat=StatLoc)

  deallocate(Residual, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine BuildMetaModel_QR_LAR(System, Goal, Coefficients, CVLOO, Hybrid, GetBest, MinAbsCorr, StopEarly, ModifiedCVLOO)

  real(rkp), dimension(:,:), target, intent(inout)                    ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), intent(inout)                                            ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVLOO
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  logical, optional, intent(in)                                       ::    ModifiedCVLOO
  logical, optional, intent(in)                                       ::    StopEarly

  character(*), parameter                                             ::    ProcName='BuildMetaModel_QR_LAR'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    HybridLoc
  logical                                                             ::    GetBestLoc
  logical                                                             ::    StopEarlyLoc
  real(rkp)                                                           ::    MinAbsCorrLoc
  logical                                                             ::    ModifiedCVLOOLoc
  integer                                                             ::    N
  integer                                                             ::    M
  real(rkp)                                                           ::    Nreal
  real(rkp)                                                           ::    Mreal
  integer                                                             ::    MaxNbRegressors
  real(rkp), allocatable, dimension(:)                                ::    Mean
  real(rkp), allocatable, dimension(:)                                ::    StD
  real(rkp)                                                           ::    GoalMean
  real(rkp)                                                           ::    GoalVariance
  integer                                                             ::    ConstantIndex
  real(rkp)                                                           ::    InvXtXScalar
  real(rkp), allocatable, dimension(:), target                        ::    VarR1D
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:)                                ::    s
  real(rkp)                                                           ::    c
  real(rkp), allocatable, dimension(:)                                ::    w
  real(rkp), allocatable, dimension(:)                                ::    u
  real(rkp)                                                           ::    aj
  real(rkp)                                                           ::    gamma
  real(rkp), allocatable, dimension(:)                                ::    h
  real(rkp)                                                           ::    hAdjustment
  real(rkp)                                                           ::    CVLOOTempNonN
  real(rkp)                                                           ::    CVLOOTemp
  integer                                                             ::    CVLOOCounter
  integer                                                             ::    CVLOOTrip
  real(rkp)                                                           ::    T
  real(rkp)                                                           ::    TSum
  logical, allocatable, dimension(:)                                  ::    Active
  logical, allocatable, dimension(:)                                  ::    Skip
  real(rkp), allocatable, dimension(:)                                ::    Corr
  real(rkp)                                                           ::    MaxAbsCorr 
  integer                                                             ::    MaxAbsCorrIndex
  integer, allocatable, dimension(:)                                  ::    ActiveIndices
  integer                                                             ::    NbActiveIndices
  real(rkp), allocatable, dimension(:)                                ::    Residual
  real(rkp), allocatable, dimension(:)                                ::    InvRInvRtS
  real(rkp)                                                           ::    InvRtS
  integer                                                             ::    MaxNbIterations
  integer                                                             ::    iIteration
  real(rkp), allocatable, dimension(:,:)                              ::    Q1
  real(rkp), allocatable, dimension(:,:)                              ::    R
  real(rkp), allocatable, dimension(:,:)                              ::    InvR
  real(rkp)                                                           ::    WNorm
  integer                                                             ::    i
  integer                                                             ::    ip1
  integer                                                             ::    ii
  integer                                                             ::    InterceptAdjustment

  HybridLoc = .true.
  if (present(Hybrid)) HybridLoc=Hybrid

  StopEarlyLoc = .true.
  if (present(StopEarly )) StopEarly Loc=StopEarly 

  GetBestLoc = .true.
  if (present(GetBest)) GetBestLoc=GetBest

  ModifiedCVLOOLoc = .true.
  if (present(ModifiedCVLOO)) ModifiedCVLOOLoc=ModifiedCVLOO

  MinAbsCorrLoc = epsilon(MinAbsCorr)*100.0_rkp
  if(present(MinAbsCorr)) MinAbsCorrLoc = MinAbsCorr

  M = size(System,1)
  Mreal = real(M,rkp)
  N = size(System,2)
  Nreal = real(N,rkp) 


  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array length', ProcName=ProcName)
  Coefficients = Zero

  GoalMean = ComputeMean(Values=Goal)
  GoalVariance = ComputeSampleVar(Values=Goal, Mean=GoalMean)

  if (N == 1) then
    InvXtXScalar = One / dot_product(System(:,1),System(:,1))
    Coefficients(1) = dot_product(System(:,1),Goal)*InvXtXScalar

    CVLOOTempNonN = Zero
    i = 1
    do i = 1, M 
      CVLOOTempNonN = CVLOOTempNonN + ((Goal(i)-Coefficients(1)*System(i,1))/(One-System(i,1)**2*InvXtXScalar))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(ModifiedCVLOOLoc) then
      if (M > 1) then
        T = (realM / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if

    if (present(CVLOO)) CVLOO = CVLOOTemp
    return
  end if

  allocate(Skip(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Skip', ProcName=ProcName, stat=StatLoc)
  Skip = .false.

  allocate(Mean(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Mean', ProcName=ProcName, stat=StatLoc)
  Mean = Zero

  allocate(StD(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='StD', ProcName=ProcName, stat=StatLoc)
  StD = Zero

  ConstantIndex = 0

  i = 1
  do i = 1, N
    Skip(i) = IsArrayConstant(Array=System(:,i))
    if (Skip(i) .and. .not. dabs(System(1,i)) > Zero) call Error%Raise('System has a column of zeros', ProcName=ProcName)
    Mean(i) = ComputeMean(Values=System(:,i))
    if (.not. Skip(i)) then
      StD(i) = ComputeSampleVar(Values=System(:,i), Mean=Mean(i))
      System(:,i) = (System(:,i) - Mean(i)) / StD(i)
    else
      if (ConstantIndex == 0) ConstantIndex = i
      StD(i) = Zero
      System(:,i) = Zero
    end if
  end do

  if (ConstantIndex /= 0) Goal = Goal - GoalMean

  VarR0D = Zero

  InterceptAdjustment = 0
  if (ConstantIndex /= 0) InterceptAdjustment = 1

  MaxNbRegressors = min(M-InterceptAdjustment,N)

  allocate(Corr(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Corr', ProcName=ProcName, stat=StatLoc)
  Corr = Zero

  allocate(ActiveIndices(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ActiveIndices', ProcName=ProcName, stat=StatLoc)
  ActiveIndices = 0
  NbActiveIndices = 0

  allocate(Active(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Inactive', ProcName=ProcName, stat=StatLoc)
  Active = .false.

  allocate(s(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='s', ProcName=ProcName, stat=StatLoc)
  s = Zero

  allocate(w(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='w', ProcName=ProcName, stat=StatLoc)
  w = Zero

  allocate(u(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u', ProcName=ProcName, stat=StatLoc)
  u = Zero

  allocate(h(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)
  h = Zero
  hAdjustment = Zero
  if (ConstantIndex /= 0) hAdjustment = One/Mreal

  allocate(CoefficientsLoc(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Coefficients'M ProcName=ProcName, stat=StatLoc)
  CoefficientsLoc = Zero

  allocate(BestCoefficients(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='BestCoefficients', ProcName=ProcName, stat=StatLoc)
  BestCoefficients = Zero

  allocate(BestIndices(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='BestIndices', ProcName=ProcName, stat=StatLoc)
  BestIndices = 0

  allocate(Residual, source=Goal, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

  allocate(InvRInvRtS(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='InvRInvRtS', ProcName=ProcName, stat=StatLoc)
  InvRInvRtS = Zero

  allocate(Q1(M,MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
  Q1 = Zero

  allocate(R(MaxNbRegressors,MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)
  R = Zero

  allocate(InvR(MaxNbRegressors,MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='InvR', ProcName=ProcName, stat=StatLoc)
  InvR = Zero

  allocate(VarR1D(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  VarR1D = Zero

  BestCVLOO = huge(One)
  BestCVLOONonN = huge(One)
  BestNbIndices = 0

  MaxNbIterations = MaxNbRegressors
  NbActiveIndices = 0
  iIteration = 0
  CVLOOTemp = huge(One)
  CVLOOTempNonN = huge(One)
  T = Zero
  CVLOOCounter = 0
  TSum = Zero
  c = Zero
  CVLOOTrip = max(nint(real(MaxNbIterations,rkp)*0.1),100)

  ! do ols for constant regressor solution if there is a constant regressor
  if (ConstantIndex /= 0) then
    InvXtXScalar = One / (Mreal*Mean(ConstantIndex)**2)

    CVLOOTempNonN = Zero
    i = 1
    do i = 1, M
      CVLOOTempNonN = CVLOOTempNonN + (Goal(i)/(One-One/Mreal))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(ModifiedCVLOOLoc) then
      if (M > 1) then
        T = (realM / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    BestCVLOO = CVLOOTemp
    BestCVLOONonN = CVLOOTempNonN
  end if

  ! begin loop
  do
    iIteration = iIteration + 1
    if (iIteration > MaxNbIterations) exit

    ! finding most correlated regressor from inactive and non-constant list
    MaxAbsCorr = Zero
    MaxAbsCorrIndex = 0
    i = 1
    do i = 1, N
      if (Active(i)) cycle
      if (Skip(i)) cycle
      Corr(i) = dot_product(System(:,i), Residual)
      if (dabs(Corr(i)) < MaxCorr) cycle
      MaxAbsCorr = dabs(Corr(i))
      MaxAbsCorrIndex = i
    end do

    if (.not. MaxAbsCorr > MinAbsCorrLoc) exit

    ! updating Q, R, and R inverse with the most correlated regressor
    ! taking advantage of inv(R') and Q' only changing last row with each iteration
    if (NbActiveIndices > 0) then

      call DGEMV('T', M, NbActiveIndices, 1.d0, Q1(:,1:NbActiveIndices), M, System(:,MaxAbsCorrIndex), 1, 0.d0, &
                  VarR1D(1:NbActiveIndices), 1)
      WNorm = ComputeNorm(Vector=VarR1D(1:NbActiveIndices), Norm=2)
      VarR0D = dsqrt(One - WNorm**2)

      ip1 = NbActiveIndices + 1
      i = 1
      do i = 1, M
        Q1(i,ip1) = (System(i,MaxAbsCorrIndex) - sum(Q1(i,1:NbActiveIndices)*VarR1D(1:NbActiveIndices))) / VarR0D
      end do

      ! actually updating transpose of R
      R(ip1,ip1) = VarR0D
      R(ip1,1:NbActiveIndices) = VarR1D(1:NbActiveIndices)

      InvR(ip1,ip1) = One / R(ip1,ip1)
      i = 1
      do i = NbActiveIndices, 1, -1
        InvR(i,ip1) = -dot_product(R(i+1:ip1,i),InvR(i+1:ip1,ip1)) / R(i,i)
      end do

    else
      Q1(:,1) = System(:,MaxAbsCorrIndex)
      R(1,1) = One
      InvR(1,1) = One
    end if

    if (.not. IsFinite(Q1(:,ip1))) call Error%Raise('Q1 is Nan or Inf', ProcName=ProcName)
    if (.not. IsFinite(InvR(1:ip1,ip1))) call Error%Raise('Inverse of R is NaN or Inf', ProcName=ProcName)
    
    ! adding the most correlated regressor to the active set
    NbActiveIndices = NbActiveIndices + 1
    ActiveIndices(NbActiveIndices) = MaxAbsCorrIndex
    Active(MaxAbsCorrIndex) = .true.

    ! s of active set
    s(NbActiveIndices) = sign(Corr(MaxAbsCorrIndex))

    ! Inv(R')*s for the newly added coefficient
    ! taking advantage of inv(R') only changing last row with each iteration
    InvRtS = dot_product(InvR(1:NbActiveIndices,NbActiveIndices),s(1:NbActiveIndices))

    ! updating Inv(R)inv(R')*s saves a lot of computations
    InvRInvRtS(1:NbActiveIndices) = InvRInvRtS(1:NbActiveIndices) + InvR(1:NbActiveIndices,NbActiveIndices)*InvRtS

    ! c of active set, c = s'*inv(R'R)*s = s'*inv(R)*inv(R')*s
    ! taking advantage of inv(R') only changing last row with each iteration
    c = c + InvRtS**2

    ! w of active set, w = inv(R)*inv(R')*s*c
    w(1:NbActiveIndices) = InvRInvRtS(1:NbActiveIndices)*c

    ! u of active set, u = System_active*w
    u = Zero
    i = 1
    do i = 1, NbActiveIndices
      u = u + System(:,ActiveIndices(i))*w(i)
    end do

    ! step length
    gamma = huge(One)
    if (iIteration < NbMaxIterations) then
      i  = 1
      do i = 1, N
        if (Active(i)) cycle
        if (Skip(i)) cycle
        aj = dot_product(System(:,i),u)
        VarR0D = (MaxAbsCorr-Corr(i))/(c-aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
        VarR0D = (MaxAbsCorr+Corr(i))/(c+aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
      end do
    else
      gamma = MaxAbsCorr / c
    end if

    ! residual update
    Residual = Residual - gamma*U

    ! udpate coefficients
    if (.not. HybridLoc) then
      CoefficientsLoc(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices) + gamma*w(1:NbActiveIndices)
    else
      ! taking advantage of inv(R') and Q' only changing last row with each iteration
      CoefficientsLoc(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices) &
                                            + InvR(1:NbActiveIndices,NbActiveIndices)*dot_product(Q1(:,NbActiveIndices),Goal)
    end if  

    ! compute LOO cv error
    ! first get leverage
    ! taking advantage of Q' only changing last column each iteration
    h = h + Q1(:,NbActiveIndices)**2
  
    ! compute cv loo
    CVLOOTempNonN = Zero
    CVLOOTemp = Zero
    if (HybridLoc) then
      VarR1D(1:M) = Zero
      i = 1
      do i = 1, NbActiveIndices
        VarR1D(1:M) = VarR1D(1:M) + CoefficientsLoc(i)*System(:,ActiveIndices(i))
      end do
      i = 1
      do i = 1, M
        CVLOOTempNonN = CVLOOTempNonN + ((Goal(i)-VarR1D(i))/(One-(h(i)+hAdjustment)))**2
      end do
      CVLOOTempNonN = CVLOOTempNonN / Mreal
    else
      VarR1D(1:M) = Residual / (One-(h+hAdjustment))
      CVLOOTempNonN = dot_product(VarR1D(1:M), VarR1D(1:M)) / MReal
    end if
  
    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    ! get correction factor if needed
    if (ModifiedCVLOOLoc) then
      if (M > NbActiveIndices + InterceptAdjustment) then
        if (ConstantIndex /= 0) then
          ! when intercept term is present, cv loo can be adjusted to be reflective of non-standardized problem
          T = Zero 
          ! taking advantage of the fact that only last colum/row of inv(R)/inv(R') changes with each loop
          i = 1
          do i = 1, NbActiveIndices 
            TSum = TSum + InvR(i,NbActiveIndices)**2 / StD(ActiveIndices(i))**2
          end do
          i = 1
          do i = 1, NbActiveIndices
            ii = ActiveIndices(i)
            VarR1D(i) = sum(System(:,ii)) + Mreal*Mean(ii)/StD(ii)
          end do
          VarR0D = Zero
          i = 1
          do i = 1, NbActiveIndices
            VarR0D = VarR0D + dot_product(VarR1D(1:i),InvR(1:i,i))**2
          end do
          T = TSum + One/(Mean(ConstantIndex)**2*Mreal)  + One/(Mean(ConstantIndex)*Mreal)**2*VarR0D
        else
          TSum = TSum + dot_product(InvR(1:NbActiveIndices,NbActiveIndices))
          T = TSum
        end if
        T = (Mreal/real(M-NbActiveIndices-InterceptAdjustment,rkp))*(One+T)
        CVLOOTemp = CVLOOTemp*T
        ! test for NaN
        if (CVLOOTemp /= CVLOOTemp) CVLOOTemp = huge(One)
      else
        CVLOOTemp = huge(One)
      end if
    end if

    if (CVLOOTemp < BestCVLOO) then
      BestCVLOONonN = CVLOOTempNonN
      BestCVLOO = CVLOOTemp
      BestCoefficients(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices)
      BestIndices(1:NbActiveIndices) = ActiveIndices(1:NbActiveIndices)
      BestNbIndices = NbActiveIndices
      CVLOOCounter = 0
    else
      CVLOOCounter = CVLOOCounter + 1
    end if

    if (CVLOOCounter >= CVLOOTrip) exit

  end do

  if (GetBestLoc) then
    if (present(CVLOO)) CVLOO = BestCVLOO
    i = 1
    do i = 1, BestNbIndices
      Coefficients(BestIndices(i)) = BestCoefficients(i) / StD(BestIndices(i))
    end do
  else
    if (present(CVLOO)) CVLOO = CVLOOTemp
    i = 1
    do i = 1, NbActiveIndices
      Coefficients(ActiveIndices(i)) = CoefficientsLoc(i) / StD(ActiveIndices(i))
    end do
  end if

  if (ConstantIndex /= 0) then
    Coefficients(ConstantIndex) = Zero
    Coefficients(ConstantIndex) = (GoalMean - dot_product(Coefficients,Mean)) / Mean(ConstantIndex)
  end if

  i = 1
  do i = 1, N 
    System(:,i) = System(:,i)*StD(i)+Mean(i)
  end do
  Goal = Goal + GoalMean

  deallocate(Q1, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

  deallocate(R, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

  deallocate(Mean, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Mean', ProcName=ProcName, stat=StatLoc)

  deallocate(StD, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='StD', ProcName=ProcName, stat=StatLoc)

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  VarR1D
  deallocate(s, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='s', ProcName=ProcName, stat=StatLoc)

  deallocate(w, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='w', ProcName=ProcName, stat=StatLoc)

  deallocate(u, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='u', ProcName=ProcName, stat=StatLoc)

  deallocate(Active, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Active', ProcName=ProcName, stat=StatLoc)

  deallocate(Skip, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Skip', ProcName=ProcName, stat=StatLoc)

  deallocate(Corr, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Corr', ProcName=ProcName, stat=StatLoc)

  deallocate(ActiveIndices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='ActiveIndices', ProcName=ProcName, stat=StatLoc)

  deallocate(InvRInvRtS, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InvRInvRtS', ProcName=ProcName, stat=StatLoc)

  deallocate(InvR, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InvR', ProcName=ProcName, stat=StatLoc)

  ! recompute loo values
  if (ConstantIndex == 0 .and. present(CVLOO)) then
    NbActiveIndices = count(dabs(Coefficients) > Zero)

    allocate(Q1(M,NbActiveIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
    Q1 = Zero

    allocate(R(NbActiveIndices,NbActiveIndices), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)
    R = Zero

    ii = 0
    i = 1
    do i = 1, N
      if (.not. dabs(Coefficients(i)) > Zero) cycle
      ii = ii + 1
      Q1(:,ii) = System(:,i) 
    end do
    if (NbActiveIndices /= ii) call Error%Raise('Something went wrong', ProcName=ProcName)

    call ComputeQR(Q=Q1, R=R, LowerR=.true.)

    ! get leverage from Q1
    h = sum(Q1**2,2)

    ! get cv loo
    Residual = Zero
    i = 1
    do i = 1, N
      if (.not. dabs(Coefficients(i)) > Zero) cycle
      Residual = Residual + Coefficients(i)*System(:,i)
    end do

    CVLOOTempNonN = Zero
    CVLOOTemp = Zero
    i = 1
    do i = 1, M
      CVLOOTempNonN = CVLOOTempNonN + (Residual(i)/(One-h(i)))**2
    end do
    CVLOOTempNonN = CVLOOTempNonN / Mreal
    
    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if (ModifiedCVLOOLoc) then
      if (M > NbActiveIndices) then
        call DTRTRI('L', 'N', NbActiveIndices, R, NbActiveIndices, StatLoc)
        if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                            ProcName=ProcName)
        T = Zero
        i = 1
        do i = 1, NbActiveIndices
          T = T + dot_product(R(i:NbActiveIndices,i),R(i:NbActiveIndices,i))
        end do
        T = (Mreal/real(M-NbActiveIndices,rkp))*(One+T)
        CVLOOTemp = CVLOOTemp*T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    
    CVLOO = CVLOOTemp

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
  
    deallocate(R, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

  end if

  deallocate(h, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='h', ProcName=ProcName, stat=StatLoc)

  deallocate(Residual, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ComputeCorrectionFactor(System, Coefficients)

  real(rkp)                                                           ::    ComputeCorrectionFactor

  character(*), parameter                                             ::    ProcName='BuildMetaModel_QR_LAR'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    Q
  real(rkp), allocatable, dimension(:,:)                              ::    InvRt 
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    N
  integer                                                             ::    M
  integer                                                             ::    NbIndices

  M = size(System,1)
  N = size(System,2)
  NbIndices = count(dabs(Coefficients) > Zero)

  allocate(Q(M,NbIndices), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Q', ProcName=ProcName, stat=StatLoc)
  Q = Zero

  allocate(InvRt(NbIndices,NbIndices), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='InvRt', ProcName=ProcName, stat=StatLoc)
  InvRt = Zero

  ii = 0
  i = 1
  do i = 1, N
    if (.not. dabs(Coefficients(i)) > Zero) cycle
    ii = ii + 1
    Q(:,ii) = System(:,i)
  end do

  call ComputeQInvR(Q=Q, InvR=InvRt, LowerInvR=.true.)

  ComputeCorrectionFactor = Zero
  i = 1
  do i = 1, NbIndices
    ComputeCorrectionFactor = ComputeCorrectionFactor + dot_product(InvRt(i:NbIndices,i),InvRt(i:NbIndices,i))
  end do
  ComputeCorrectionFactor = (Mreal/real(M-NbIndices,rkp))*(One+T)

  deallocate(Q, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Q', ProcName=ProcName, stat=StatLoc)

  deallocate(InvRt, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

end module
