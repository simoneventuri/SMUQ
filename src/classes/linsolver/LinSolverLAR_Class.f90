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
use CommandRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use CVMethod_Factory_Class                                        ,only:    CVMethod_Factory
use CVMethod_Class                                                ,only:    CVMethod_Type, CVFitTarget
use CVLOO_Class                                                   ,only:    CVLOO_Type
use ArrayIORoutines_Module

implicit none

private

public                                                                ::    LinSolverLAR_Type

type, extends(LinSolverMethod_Type)                                   ::    LinSolverLAR_Type
  real(rkp)                                                           ::    MinAbsCorr
  logical                                                             ::    Hybrid
  logical                                                             ::    CorrectedCV
  logical                                                             ::    GetBest
  logical                                                             ::    StopEarly
  integer                                                             ::    NbCVErrorInc
  class(CVMethod_Type), allocatable                                   ::    CVError
  integer                                                             ::    MetaModelMethod
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
  This%CorrectedCV = .true.
  This%GetBest = .true.
  This%StopEarly = .true.
  This%MetaModelMethod = 1
  This%NbCvErrorInc = 6

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
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    VarC0D
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

  ParameterName = 'corrected_cross_validation'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%CorrectedCV = VarL0D

  ParameterName = 'get_best'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%GetBest = VarL0D

  ParameterName = 'stop_early'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%StopEarly = VarL0D

  ParameterName = 'nb_cross_validation_increases'
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%NbCvErrorInc = VarI0D
  if (This%NbCVErrorInc < 2) call Error%Raise('Number of allowable CV error increases must be above 1', ProcName=ProcName)

  ParameterName = 'metamodel_method'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) then
    select case (VarC0D)
      case('qr')
        This%MetaModelMethod = 1
      case('gram')
        This%MetaModelMethod = 2
      case default
        call Error%Raise('Meta model specification not recognized', ProcName=ProcName)
    end select
  end if

  SectionName = 'cross_validation'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call CVMethod_Factory%Construct(Object=This%CVError, Input=InputSection, Prefix=PrefixLoc)
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
subroutine ConstructCase1(This, CVMethod, Hybrid, MinAbsCorr, GetBest, StopEarly, CorrectedCV, MetaModelMethod, NbCVErrorInc)

  class(LinSolverLAR_Type), intent(inout)                             ::    This
  class(CVMethod_Type), optional, intent(in)                          ::    CVMethod
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  logical, optional, intent(in)                                       ::    StopEarly
  logical, optional, intent(in)                                       ::    CorrectedCV
  real(rkp), optional, intent(in)                                     ::    MinAbsCorr
  character(*), optional, intent(in)                                  ::    MetaModelMethod
  integer, optional, intent(in)                                       ::    NbCVErrorInc

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(Hybrid)) This%Hybrid = Hybrid

  if (present(MinAbsCorr)) This%MinAbsCorr = MinAbsCorr

  if (present(GetBest)) This%GetBest = GetBest

  if (present(StopEarly)) This%StopEarly = StopEarly

  if (present(CorrectedCV)) This%CorrectedCV = CorrectedCV

  if (present(NbCVErrorInc)) This%NbCVErrorInc = NbCVErrorInc
  if (This%NbCVErrorInc < 2) call Error%Raise('Number of allowable CV error increases must be above 1', ProcName=ProcName)

  if (present(MetaModelMethod)) then
    select case (MetaModelMethod)
      case('qr')
        This%MetaModelMethod = 1
      case('gram')
        This%MetaModelMethod = 2
      case default
        call Error%Raise('Meta model specification not recognized', ProcName=ProcName)
    end select
  end if

  if (present(CVMethod)) then
    allocate(This%CVError, source=CVMethod, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%CVErrorMethod', ProcName=ProcName, stat=StatLoc)
  else
    allocate(CVLOO_Type :: This%CVError)
    select type (CVError => This%CVError)
      type is (CVLOO_Type)
        call CVError%Construct(Normalized=.true.)
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
  if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='hybrid', Value=ConvertToString(Value=This%Hybrid))
  call GetInput%AddParameter(Name='stop_early', Value=ConvertToString(Value=This%StopEarly))
  call GetInput%AddParameter(Name='get_best', Value=ConvertToString(Value=This%GetBest))
  call GetInput%AddParameter(Name='modified_cross_validation', Value=ConvertToString(Value=This%CorrectedCV))
  call GetInput%AddParameter(Name='minimum_correlation', Value=ConvertToString(Value=This%MinAbsCorr))
  call GetInput%AddParameter(Name='nb_cross_validation_increases', Value=ConvertToString(Value=This%NbCVErrorInc))
  select case (This%MetaModelMethod)
    case(1)
      call GetInput%AddParameter(Name='metamodel_method', Value='qr')

    case(2)
      call GetInput%AddParameter(Name='metamodel_method', Value='gram')
    case default
      call Error%Raise('Something went wrong', ProcName=ProcName)
  end select

  SectionName = 'cross_validation'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/cross_validation'
  call GetInput%AddSection(Section=CVMethod_Factory%GetObjectInput(Object=This%CVError, Name=SectionName, Prefix=PrefixLoc, &
                           Directory=DirectorySub))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Solve(This, System, Goal, Coefficients, CVError)
use SMUQFile_Class ,only: SMUQFile_TYpe
use ArrayIORoutines_Module
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
  character(:), allocatable :: FileName
  type(SMUQFile_TYpe) :: File

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients and system arrays', ProcName=ProcName)

  if (This%MetaModelMethod == 1) then
    if (present(CVError)) then
      select type (CVMethod=>This%CVError)
        type is (CVLOO_Type)
          call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, CVLOO=CVError, Hybrid=This%Hybrid, &
                                     GetBest=This%GetBest, MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, &
                                     CorrectedCV=This%CorrectedCV, NbCVErrorInc=This%NbCVErrorInc)
          if (.not. CVMethod%IsNormalized() .and. CVError < huge(One)) CVError = CVError * ComputeVariance(Values=Goal)
        class default
          CVFit => CVFitLAR
          call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid,  &
                                     GetBest=This%GetBest, MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, &
                                     CorrectedCV=This%CorrectedCV, NbCVErrorInc=This%NbCVErrorInc)
          CVError = CVMethod%Calculate(Fit=CVFit, FitData=Goal)
          if (This%CorrectedCV) CVError = CVError * ComputeCorrectionFactor(System=System, Coefficients=Coefficients)
          nullify(CVFit)
      end select
    else
      call BuildMetaModel_QR_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid, GetBest=This%GetBest, &
                                 MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, CorrectedCV=This%CorrectedCV, &
                                 NbCVErrorInc=This%NbCVErrorInc)
    end if
  elseif ( This%MetaModelMethod == 2) then
    if (present(CVError)) then
      select type (CVMethod=>This%CVError)
        type is (CVLOO_Type)
          call BuildMetaModel_Gram_LAR(System=System, Goal=Goal, Coefficients=Coefficients, CVLOO=CVError, Hybrid=This%Hybrid, &
                                     GetBest=This%GetBest, MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, &
                                     CorrectedCV=This%CorrectedCV, NbCVErrorInc=This%NbCVErrorInc)
          if (.not. CVMethod%IsNormalized() .and. CVError < huge(One)) CVError = CVError * ComputeVariance(Values=Goal)
        class default
          CVFit => CVFitLAR
          call BuildMetaModel_Gram_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid,  &
                                     GetBest=This%GetBest, MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, &
                                     CorrectedCV=This%CorrectedCV, NbCVErrorInc=This%NbCVErrorInc)
          CVError = CVMethod%Calculate(Fit=CVFit, FitData=Goal)
          if (This%CorrectedCV) CVError = CVError * ComputeCorrectionFactor(System=System, Coefficients=Coefficients)
          nullify(CVFit)
      end select
    else
      call BuildMetaModel_Gram_LAR(System=System, Goal=Goal, Coefficients=Coefficients, Hybrid=This%Hybrid, GetBest=This%GetBest, &
                                 MinAbsCorr=This%MinAbsCorr, StopEarly=This%StopEarly, CorrectedCV=This%CorrectedCV, &
                                 NbCVErrorInc=This%NbCVErrorInc)
    end if
  else
    call Error%Raise('Something went wrong', ProcName=ProcName)
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

    NbTraining = size(TrainingSet,1)
    NbValidation = size(ValidationSet,1)

    if (size(Residual,1) /= NbValidation) call Error%Raise('Incompatible residual and validation arrays', ProcName=ProcName)

    allocate(CoefficientsLoc(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)
    CoefficientsLoc = Zero

    allocate(SystemLoc(NbTraining,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)
    SystemLoc = System(TrainingSetIndices,:)

    call This%Solve(System=SystemLoc, Goal=TrainingSet, Coefficients=CoefficientsLoc)

    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    allocate(SystemLoc(NbValidation,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)
    SystemLoc = System(ValidationSetIndices,:)

    Residual = matmul(SystemLoc,CoefficientsLoc)
    Residual = ValidationSet - Residual

    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

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
        LHS%CorrectedCV = RHS%CorrectedCV
        LHS%GetBest = RHS%GetBest
        LHS%MetaModelMethod = RHS%MetaModelMethod
        LHS%NbCVErrorInc = RHS%NbCVErrorInc
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
subroutine BuildMetaModel_Gram_LAR(System, Goal, Coefficients, CVLOO, Hybrid, GetBest, MinAbsCorr, StopEarly, CorrectedCV, &
                                   NbCVErrorInc)

  real(rkp), dimension(:,:), target, intent(inout)                    ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVLOO
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  real(rkp), optional, intent(in)                                     ::    MinAbsCorr
  logical, optional, intent(in)                                       ::    CorrectedCV
  logical, optional, intent(in)                                       ::    StopEarly
  integer, optional, intent(in)                                       ::    NbCVErrorInc

  character(*), parameter                                             ::    ProcName='BuildMetaModel_Gram_LAR'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    HybridLoc
  logical                                                             ::    GetBestLoc
  logical                                                             ::    StopEarlyLoc
  real(rkp)                                                           ::    MinAbsCorrLoc
  logical                                                             ::    CorrectedCVLoc
  integer                                                             ::    N
  integer                                                             ::    M
  real(rkp)                                                           ::    Nreal
  real(rkp)                                                           ::    Mreal
  integer                                                             ::    MaxNbRegressors
  integer                                                             ::    MaxNbIterations
  integer                                                             ::    ConstantIndex
  integer                                                             ::    iIndex
  integer                                                             ::    i
  integer                                                             ::    ip1
  integer                                                             ::    ii
  integer                                                             ::    iii
  real(rkp), allocatable, dimension(:)                                ::    Mean
  real(rkp), allocatable, dimension(:)                                ::    StD
  real(rkp)                                                           ::    GoalMean
  real(rkp)                                                           ::    GoalVariance
  integer                                                             ::    iIteration
  logical, allocatable, dimension(:)                                  ::    InActive
  real(rkp), allocatable, dimension(:)                                ::    Corr
  real(rkp)                                                           ::    MaxAbsCorr 
  integer                                                             ::    MaxAbsCorrIndex
  integer, allocatable, dimension(:)                                  ::    ActiveIndices
  integer                                                             ::    NbActiveIndices
  real(rkp), allocatable, dimension(:)                                ::    Residual
  real(rkp), allocatable, dimension(:,:)                              ::    InvXtX
  real(rkp)                                                           ::    InvXtXScalar
  real(rkp), allocatable, dimension(:)                                ::    u1
  real(rkp), allocatable, dimension(:)                                ::    u2
  real(rkp)                                                           ::    d
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
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
  integer                                                             ::    CVLOOIncCounter
  real(rkp)                                                           ::    CVLOOM1
  integer                                                             ::    NbCVErrorIncLoc
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
  real(rkp), allocatable, dimension(:,:)                              ::    SystemTemp
  real(rkp), allocatable, dimension(:)                                ::    CorrectionMem
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D

  HybridLoc = .true.
  if (present(Hybrid)) HybridLoc=Hybrid

  NbCVErrorIncLoc = 6
  if (present(NbCVErrorInc)) NbCVErrorIncLoc = NbCVErrorInc

  StopEarlyLoc = .true.
  if (present(StopEarly)) StopEarlyLoc=StopEarly 

  GetBestLoc = .true.
  if (present(GetBest)) GetBestLoc=GetBest

  CorrectedCVLoc = .true.
  if (present(CorrectedCV)) CorrectedCVLoc=CorrectedCV

  MinAbsCorrLoc = epsilon(MinAbsCorr)*100.0_rkp
  if(present(MinAbsCorr)) MinAbsCorrLoc = MinAbsCorr

  M = size(System,1)
  Mreal = real(M,rkp)
  N = size(System,2)
  Nreal = real(N,rkp) 

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array length', ProcName=ProcName)
  Coefficients = Zero

  GoalMean = ComputeMean(Values=Goal)
  GoalVariance = ComputeVariance(Values=Goal, Mean=GoalMean)

  if (N == 1) then
    InvXtXScalar = One / dot_product(System(:,1),System(:,1))
    Coefficients(1) = dot_product(System(:,1),Goal)*InvXtXScalar

    CVLOOTempNonN = sum(((Goal-Coefficients(1)*System(:,1))/(One-System(:,1)**2*InvXtXScalar))**2) / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(CorrectedCVLoc) then
      if (M > 1) then
        T = (Mreal / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if

    if (present(CVLOO)) CVLOO = CVLOOTemp
    return
  end if

  allocate(Mean(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Mean', ProcName=ProcName, stat=StatLoc)
  Mean = Zero

  allocate(StD(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='StD', ProcName=ProcName, stat=StatLoc)
  StD = Zero

  allocate(InActive(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Inactive', ProcName=ProcName, stat=StatLoc)
  InActive = .true.

  ConstantIndex = 0

  i = 1
  do i = 1, N 
    InActive(i) = .not. IsArrayConstant(Array=System(:,i))
    if (.not. InActive(i) .and. .not. dabs(System(1,i)) > Zero) call Error%Raise('System has a column of zeros', ProcName=ProcName)
    Mean(i) = ComputeMean(Values=System(:,i))
    if (InActive(i)) then
      StD(i) = dsqrt(ComputeVariance(Values=System(:,i), Mean=Mean(i)))
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

  MaxNbIterations = min(M-InterceptAdjustment,N)
  MaxNbRegressors = MaxNbIterations - count(.not. InActive)

  allocate(SystemTemp(M,MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='SystemTemp', ProcName=ProcName, stat=StatLoc)
  SystemTemp = Zero 

  allocate(CorrectionMem(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='CorrectionMem', ProcName=ProcName, stat=StatLoc)
  CorrectionMem = Zero 

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

  allocate(s(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='s', ProcName=ProcName, stat=StatLoc)
  s = 1

  allocate(w(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='w', ProcName=ProcName, stat=StatLoc)
  w = Zero

  allocate(u(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u', ProcName=ProcName, stat=StatLoc)
  u = Zero

  allocate(h(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)
  h = Zero

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
  InvXtX = Zero

  if (HybridLoc) then
    allocate(XtY(MaxnbRegressors), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='XtY', ProcName=ProcName, stat=StatLoc)
    XtY = Zero
  end if

  allocate(VarR1D(max(M,N)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  VarR1D = Zero

  BestCVLOO = huge(One)
  BestCVLOONonN = huge(One)
  BestNbIndices = 0

  NbActiveIndices = 0
  iIteration = 0
  CVLOOTemp = huge(One)
  CVLOOTempNonN = huge(One)
  CVLOOCounter = 0
  T = Zero
  CVLOOTrip = max(nint(real(MaxNbIterations,rkp)*0.1),100)
  CVLOOM1 = huge(One)
  CVLOOIncCounter = 0

  ! do ols for constant regressor solution if there is a constant regressor
  if (ConstantIndex /= 0) then
    InvXtXScalar = One / (Mreal*Mean(ConstantIndex)**2)

    CVLOOTempNonN = sum((Goal/(One-One/Mreal))**2) / MReal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(CorrectedCVLoc) then
      if (M > 1) then
        T = (Mreal / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    BestCVLOO = CVLOOTemp
    BestCVLOONonN = CVLOOTempNonN

    CVLOOIncCounter = CVLOOIncCounter + 1
    if (CVLOOTemp < CVLOOM1) CVLOOIncCounter = 0
    CVLOOM1 = CVLOOTemp
  end if

  ! begin loop
  do
    iIteration = iIteration + 1
    if (iIteration > MaxNbIterations) exit
    if (NbActiveIndices == MaxNbRegressors) exit

    ! finding most correlated regressor from inactive and non-constant list
    Corr(1:N) = matmul(Residual,System)
    MaxAbsCorrIndex = maxloc(dabs(Corr),1,InActive)
    MaxAbsCorr = dabs(Corr(MaxAbsCorrIndex))

    if (.not. MaxAbsCorr > MinAbsCorrLoc) exit
    InActive(MaxAbsCorrIndex) = .false.

    ! Updating inverse locally to reduce memory requirements and alloc/dealloc times
    v => System(:,MaxAbsCorrIndex)
    if (NbActiveIndices == 0) then
      InvXtX(1,1) = One / sum(v**2) 
      if (.not. IsFinite(Value=InvXtX(1,1))) then
        write(*,'(A)') 'Warning: Ignoring regressor ' // ConvertToString(Value=MaxAbsCorrIndex) // 'due to instability'
        cycle
      end if
    else
      u1(1:NbActiveIndices) = matmul(v, SystemTemp(:,1:NbActiveIndices))
  
      u2(1:NbActiveIndices) = matmul(InvXtX(1:NbActiveIndices,1:NbActiveIndices),u1(1:NbActiveIndices))

      d = One / (sum(v**2) - dot_product(u1(1:NbActiveIndices), u2(1:NbActiveIndices)))
  
      VarR0D = sum(u2(1:NbActiveIndices)**2)
  
      if ((.not. IsFinite(Values=u1)) .or. (.not. IsFinite(Values=u2)) .or. (.not. IsFinite(Value=d)) &
            .or. (.not. IsFinite(Value=VarR0D))) then
        write(*,'(A)') 'Warning: Ignoring regressor ' // ConvertToString(Value=MaxAbsCorrIndex) // 'due to instability'
        cycle
      end if

      i = 1
      do i = 1, NbActiveIndices
        InvXtX(1:NbActiveIndices,i) = InvXtX(1:NbActiveIndices,i) + d*u2(i)*u2(1:NbActiveIndices)
      end do
      InvXtX(1:NbActiveIndices,NbActiveIndices+1) = -d*u2(1:NbActiveIndices)
      InvXtX(NbActiveIndices+1,1:NbActiveIndices) = -d*u2(1:NbActiveIndices)
      InvXtX(NbActiveIndices+1,NbActiveIndices+1) = d
    end if

    NbActiveIndices = NbActiveIndices + 1
    ActiveIndices(NbActiveIndices) = MaxAbsCorrIndex
    SystemTemp(:,NbActiveIndices) = v
    nullify(v)

    ! s of active set
    if (Corr(MaxAbsCorrIndex) <= Zero) s(NbActiveIndices) = -1

    ! w and c of active set
    w(1:NbActiveIndices) = matmul(InvXtx(1:NbActiveIndices,1:NbActiveIndices),s(1:NbActiveIndices))
    c = One / dsqrt(dot_product(w(1:NbActiveIndices),s(1:NbActiveIndices)))
    w(1:NbActiveIndices) = w(1:NbActiveIndices) * c 

    ! u
    u = matmul(SystemTemp(:,1:NbActiveIndices),w(1:NbActiveIndices))

    ! step length
    gamma = huge(One)
    if (NbActiveIndices < MaxNbRegressors) then
      VarR1D(1:N) = matmul(u,System)
      i  = 1
      do i = 1, N 
        if (.not. InActive(i)) cycle
        aj = VarR1D(i)
        VarR0D = (MaxAbsCorr-Corr(i))/(c-aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
        VarR0D = (MaxAbsCorr+Corr(i))/(c+aj)
        if (VarR0D > Zero .and. VarR0D < gamma) gamma = VarR0D
      end do
    else
      gamma = MaxAbsCorr / c
    end if

    ! residual update
    Residual = Residual - gamma*u

    ! udpate coefficients
    if (.not. HybridLoc) then
      CoefficientsLoc(1:NbActiveIndices) = CoefficientsLoc(1:NbActiveIndices) + gamma*w(1:NbActiveIndices)
    else
      XtY(NbActiveIndices) = dot_product(System(:,ActiveIndices(NbActiveIndices)),Goal)
      CoefficientsLoc(1:NbActiveIndices) = matmul(InvXtx(1:NbActiveIndices,1:NbActiveIndices),XtY(1:NbActiveIndices))
    end if

    ! compute LOO cv error
    ! first get leverage
    ! h = Zero
    ! i = 1
    ! do i = 1, NbActiveIndices 
    !   h = h + SystemTemp(:,i)**2*InvXtX(i,i)
    !   ii = i+1
    !   do ii = i+1,NbActiveIndices
    !     h = h + Two*SystemTemp(:,i)*SystemTemp(:,ii)*InvXtX(ii,i)
    !   end do
    ! end do

    VarR2D = matmul(SystemTemp(:,1:NbActiveIndices),InvXtx(1:NbActiveIndices,1:NbActiveIndices))
    h = sum(VarR2D*SystemTemp(:,1:NbActiveIndices),2)
    if (ConstantIndex /= 0) h = h + One/Mreal

    ! compute cv loo
    CVLOOTempNonN = Zero
    CVLOOTemp = Zero
    if (HybridLoc) then
      VarR1D(1:M) = matmul(SystemTemp(:,1:NbActiveIndices),CoefficientsLoc(1:NbActiveIndices))
      CVLOOTempNonN = sum(((Goal-VarR1D(1:M))/(One-h))**2) / MReal
    else
      CVLOOTempNonN = sum((Residual / (One-h))**2) / Mreal
    end if
  
    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    ! get correction factor if needed
    if (CorrectedCVLoc) then
      if (M > NbActiveIndices + InterceptAdjustment) then
        if (ConstantIndex /= 0) then
          iIndex = ActiveIndices(NbActiveIndices)
          CorrectionMem(NbActiveIndices) = sum(System(:,iIndex)) + Mreal*Mean(iIndex)/StD(iIndex)
          T = Zero
          VarR1D(1:NbActiveIndices) = matmul(InvXtX(1:NbActiveIndices,1:NbActiveIndices),CorrectionMem(1:NbActiveIndices))
          VarR0D = dot_product(VarR1D(1:NbActiveIndices),CorrectionMem(1:NbActiveIndices))
          i = 1
          do i = 1, NbActiveIndices
            T = T + InvXtX(i,i)/StD(ActiveIndices(i))**2
          end do
          T = T + One/(Mean(ConstantIndex)**2*Mreal) + One/(Mean(ConstantIndex)*Mreal)**2*VarR0D
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

    if (StopEarlyLoc .and. CVLOOCounter >= CVLOOTrip) exit

    CVLOOIncCounter = CVLOOIncCounter + 1
    if (CVLOOTemp < CVLOOM1) CVLOOIncCounter = 0
    if (StopEarlyLoc .and. CVLOOIncCounter >= NbCVErrorIncLoc) exit
    CVLOOM1 = CVLOOTemp
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

  deallocate(SystemTemp, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='SystemTemp', ProcName=ProcName, stat=StatLoc)

  deallocate(CorrectionMem, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='CorrectionMem', ProcName=ProcName, stat=StatLoc)

  deallocate(Mean, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Mean', ProcName=ProcName, stat=StatLoc)

  deallocate(StD, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='StD', ProcName=ProcName, stat=StatLoc)

  deallocate(InActive, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InActive', ProcName=ProcName, stat=StatLoc)

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

  if (allocated(VarR2D)) deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

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

    if (CorrectedCVLoc) then
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
subroutine BuildMetaModel_QR_LAR(System, Goal, Coefficients, CVLOO, Hybrid, GetBest, MinAbsCorr, StopEarly, CorrectedCV, &
                                 NbCVErrorInc)

  real(rkp), dimension(:,:), target, intent(inout)                    ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVLOO
  logical, optional, intent(in)                                       ::    Hybrid
  logical, optional, intent(in)                                       ::    GetBest
  logical, optional, intent(in)                                       ::    CorrectedCV
  real(rkp), optional, intent(in)                                     ::    MinAbsCorr
  logical, optional, intent(in)                                       ::    StopEarly
  integer, optional, intent(in)                                       ::    NbCVErrorInc

  character(*), parameter                                             ::    ProcName='BuildMetaModel_QR_LAR'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    HybridLoc
  logical                                                             ::    GetBestLoc
  logical                                                             ::    StopEarlyLoc
  real(rkp)                                                           ::    MinAbsCorrLoc
  logical                                                             ::    CorrectedCVLoc
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
  integer                                                             ::    iIndex
  real(rkp)                                                           ::    InvXtXScalar
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:)                                ::    s
  real(rkp)                                                           ::    c
  real(rkp)                                                           ::    cden
  real(rkp), allocatable, dimension(:)                                ::    w
  real(rkp), allocatable, dimension(:)                                ::    u
  real(rkp)                                                           ::    aj
  real(rkp)                                                           ::    gamma
  real(rkp), allocatable, dimension(:)                                ::    h
  real(rkp)                                                           ::    hAdjustment
  real(rkp)                                                           ::    CVLOOTempNonN
  real(rkp)                                                           ::    CVLOOTemp
  integer                                                             ::    CVLOOCounter
  integer                                                             ::    CVLOOIncCounter
  integer                                                             ::    NbCVErrorIncLoc
  real(rkp)                                                           ::    CVLOOM1
  integer                                                             ::    CVLOOTrip
  real(rkp)                                                           ::    T
  real(rkp)                                                           ::    TSum
  logical, allocatable, dimension(:)                                  ::    InActive
  real(rkp), allocatable, dimension(:)                                ::    Corr
  real(rkp), allocatable, dimension(:)                                ::    CoefficientsLoc
  real(rkp), allocatable, dimension(:)                                ::    BestCoefficients
  integer, allocatable, dimension(:)                                  ::    BestIndices
  real(rkp)                                                           ::    BestCVLOO 
  real(rkp)                                                           ::    BestCVLOONonN
  integer                                                             ::    BestNbIndices
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
  integer                                                             ::    i
  integer                                                             ::    ip1
  integer                                                             ::    ii
  integer                                                             ::    InterceptAdjustment
  real(rkp), allocatable, dimension(:)                                ::    CorrectionMem
  real(rkp)                                                           ::    CorrectionSum

  HybridLoc = .true.
  if (present(Hybrid)) HybridLoc=Hybrid

  StopEarlyLoc = .true.
  if (present(StopEarly)) StopEarlyLoc=StopEarly 

  NbCVErrorIncLoc = 6
  if (present(NbCVErrorInc)) NbCVErrorIncLoc = NbCVErrorInc

  GetBestLoc = .true.
  if (present(GetBest)) GetBestLoc=GetBest

  CorrectedCVLoc = .true.
  if (present(CorrectedCV)) CorrectedCVLoc=CorrectedCV

  MinAbsCorrLoc = epsilon(MinAbsCorr)*100.0_rkp
  if(present(MinAbsCorr)) MinAbsCorrLoc = MinAbsCorr

  M = size(System,1)
  Mreal = real(M,rkp)
  N = size(System,2)
  Nreal = real(N,rkp) 

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array length', ProcName=ProcName)
  Coefficients = Zero

  GoalMean = ComputeMean(Values=Goal)
  GoalVariance = ComputeVariance(Values=Goal, Mean=GoalMean)

  if (N == 1) then
    InvXtXScalar = One / dot_product(System(:,1),System(:,1))
    Coefficients(1) = dot_product(System(:,1),Goal)*InvXtXScalar

    CVLOOTempNonN = sum(((Goal-Coefficients(1)*System(:,1))/(One-System(:,1)**2*InvXtXScalar))**2) / Mreal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(CorrectedCVLoc) then
      if (M > 1) then
        T = (Mreal / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if

    if (present(CVLOO)) CVLOO = CVLOOTemp
    return
  end if

  allocate(InActive(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Inactive', ProcName=ProcName, stat=StatLoc)
  InActive = .true.

  allocate(Mean(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Mean', ProcName=ProcName, stat=StatLoc)
  Mean = Zero

  allocate(StD(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='StD', ProcName=ProcName, stat=StatLoc)
  StD = Zero

  ConstantIndex = 0

  i = 1
  do i = 1, N
    InActive(i) = .not. IsArrayConstant(Array=System(:,i))
    if (.not. InActive(i) .and. .not. dabs(System(1,i)) > Zero) call Error%Raise('System has a column of zeros', ProcName=ProcName)
    Mean(i) = ComputeMean(Values=System(:,i))
    if (InActive(i)) then
      StD(i) = dsqrt(ComputeVariance(Values=System(:,i), Mean=Mean(i)))
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

  MaxNbIterations = min(M-InterceptAdjustment,N)
  MaxNbRegressors = MaxNbIterations - count(.not. InActive)
  
  allocate(CorrectionMem(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='CorrectionMem', ProcName=ProcName, stat=StatLoc)
  CorrectionMem = Zero 

  allocate(Corr(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Corr', ProcName=ProcName, stat=StatLoc)
  Corr = Zero

  allocate(ActiveIndices(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ActiveIndices', ProcName=ProcName, stat=StatLoc)
  ActiveIndices = 0
  NbActiveIndices = 0

  allocate(s(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='s', ProcName=ProcName, stat=StatLoc)
  s = 1

  allocate(w(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='w', ProcName=ProcName, stat=StatLoc)
  w = Zero

  allocate(u(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='u', ProcName=ProcName, stat=StatLoc)
  u = Zero

  allocate(h(M), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)
  h = Zero
  hAdjustment = Zero
  if (ConstantIndex /= 0) hAdjustment = One/Mreal

  allocate(CoefficientsLoc(MaxNbRegressors), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
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

  allocate(VarR1D(max(M,N)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  VarR1D = Zero

  BestCVLOO = huge(One)
  BestCVLOONonN = huge(One)
  BestNbIndices = 0

  NbActiveIndices = 0
  iIteration = 0
  CVLOOTemp = huge(One)
  CVLOOTempNonN = huge(One)
  T = Zero
  CVLOOCounter = 0
  TSum = Zero
  c = Zero
  cden = Zero
  CVLOOTrip = max(nint(real(MaxNbIterations,rkp)*0.1),100)
  ip1 = 0
  CVLOOM1 = huge(One)
  CVLOOIncCounter = 0
  CorrectionSum = Zero 

  ! do ols for constant regressor solution if there is a constant regressor
  if (ConstantIndex /= 0) then
    InvXtXScalar = One / (Mreal*Mean(ConstantIndex)**2)

    CVLOOTempNonN = sum((Goal/(One-One/Mreal))**2) / MReal

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    if(CorrectedCVLoc) then
      if (M > 1) then
        T = (Mreal / real(M-1,rkp))*(One+InvXtXScalar)
        CVLOOTemp = CVLOOTemp * T
      else
        CVLOOTemp = huge(One)
      end if
    end if
    BestCVLOO = CVLOOTemp
    BestCVLOONonN = CVLOOTempNonN

    CVLOOIncCounter = CVLOOIncCounter + 1
    if (CVLOOTemp < CVLOOM1) CVLOOIncCounter = 0
    CVLOOM1 = CVLOOTemp

  end if

  ! begin loop
  do
    iIteration = iIteration + 1
    if (iIteration > MaxNbIterations) exit
    if (NbActiveIndices == MaxNbRegressors) exit
    ! finding most correlated regressor from inactive and non-constant list
    Corr(1:N) = matmul(Residual,System)
    MaxAbsCorrIndex = maxloc(dabs(Corr),1,InActive)
    MaxAbsCorr = dabs(Corr(MaxAbsCorrIndex))

    if (.not. MaxAbsCorr > MinAbsCorrLoc) exit
    InActive(MaxAbsCorrIndex) = .false.

    ! updating Q, R, and R inverse with the most correlated regressor
    ! taking advantage of inv(R') and Q' only changing last row with each iteration
    if (NbActiveIndices > 0) then
      ip1 = NbActiveIndices + 1

      R(1:NbActiveIndices,ip1) = matmul(System(:,MaxAbsCorrIndex),Q1(:,1:NbActiveIndices))
      VarR1D(1:M) = matmul(Q1(:,1:NbActiveIndices),R(1:NbActiveIndices,ip1))
      Q1(:,ip1) = System(:,MaxAbsCorrIndex) - VarR1D(1:M)
      R(ip1,ip1) = dsqrt(sum(Q1(:,ip1)**2))
      if (.not. dabs(R(ip1,ip1)) > Zero) call Error%Raise('Zero on the diagonal of R', ProcName=ProcName)
      Q1(:,ip1) = Q1(:,ip1) / R(ip1,ip1)
      VarR1D(1:NbActiveIndices) = R(1:NbActiveIndices,ip1)
      R(ip1,1:NbActiveIndices) = VarR1D(1:NbActiveIndices)

      InvR(ip1,ip1) = One / R(ip1,ip1)
      i = 1
      do i = NbActiveIndices, 1, -1
        InvR(i,ip1) = -dot_product(R(i+1:ip1,i),InvR(i+1:ip1,ip1)) / R(i,i)
      end do
    else
      R(1,1) = dsqrt(sum(System(:,MaxAbsCorrIndex)**2))
      if (.not. dabs(R(1,1)) > Zero) call Error%Raise('Zero on the diagonal of R', ProcName=ProcName)
      Q1(:,1) = System(:,MaxAbsCorrIndex) / R(1,1)
      InvR(1,1) = One / R(1,1)
    end if

    ! adding the most correlated regressor to the active set
    NbActiveIndices = NbActiveIndices + 1
    ActiveIndices(NbActiveIndices) = MaxAbsCorrIndex

    if (.not. IsFinite(Q1(:,NbActiveIndices))) call Error%Raise('Q1 is Nan or Inf', ProcName=ProcName)
    if (.not. IsFinite(InvR(1:NbActiveIndices,NbActiveIndices))) call Error%Raise('Inverse of R is NaN or Inf', ProcName=ProcName)

    ! s of active set
    if (Corr(MaxAbsCorrIndex) <= Zero) s(NbActiveIndices) = -1

    ! Inv(R')*s for the newly added coefficient
    ! taking advantage of inv(R') only changing last row with each iteration
    InvRtS = dot_product(InvR(1:NbActiveIndices,NbActiveIndices),s(1:NbActiveIndices))

    ! updating Inv(R)inv(R')*s saves a lot of computations
    InvRInvRtS(1:NbActiveIndices) = InvRInvRtS(1:NbActiveIndices) + InvR(1:NbActiveIndices,NbActiveIndices)*InvRtS

    ! c of active set, c = 1/sqrt(s'*inv(R'R)*s)
    ! taking advantage of inv(R') only changing last row with each iteration
    cden = cden + InvRtS**2
    c = One / dsqrt(cden)

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

    if (NbActiveIndices < MaxNbRegressors) then
      VarR1D(1:N) = matmul(u,System)
      i  = 1
      do i = 1, N
        if (.not. InActive(i)) cycle
        aj = VarR1D(i)
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
      VarR1D(1:M) = CoefficientsLoc(1)*System(:,ActiveIndices(1))
      i = 2
      do i = 2, NbActiveIndices
        VarR1D(1:M) = VarR1D(1:M) + CoefficientsLoc(i)*System(:,ActiveIndices(i))
      end do
      CVLOOTempNonN = sum(((Goal-VarR1D(1:M))/(One-(h+hAdjustment)))**2) / MReal
    else
      CVLOOTempNonN = sum((Residual / (One-(h+hAdjustment)))**2) / Mreal
    end if

    if (GoalVariance > Zero) then
      CVLOOTemp = CVLOOTempNonN / GoalVariance
    else
      CVLOOTemp = Zero
    end if

    ! get correction factor if needed
    if (CorrectedCVLoc) then
      if (M > NbActiveIndices + InterceptAdjustment) then
        if (ConstantIndex /= 0) then
          ! when intercept term is present, cv loo can be adjusted to be reflective of non-standardized problem
          ! taking advantage of the fact that only last colum/row of inv(R)/inv(R') changes with each loop
          iIndex = ActiveIndices(NbActiveIndices)
          T = Zero 
          TSum = TSum + sum((InvR(1:NbActiveIndices,NbActiveIndices)/StD(ActiveIndices(1:NbActiveIndices)))**2)
          CorrectionMem(NbActiveIndices) = sum(System(:,iIndex)) + Mreal*Mean(iIndex)/StD(iIndex)
          CorrectionSum = CorrectionSum + dot_product(CorrectionMem(1:NbActiveIndices),InvR(1:NbActiveIndices,NbActiveIndices))**2
          T = TSum + One/(Mean(ConstantIndex)**2*Mreal)  + One/(Mean(ConstantIndex)*Mreal)**2*CorrectionSum
        else
          TSum = TSum + sum(InvR(1:NbActiveIndices,NbActiveIndices)**2)
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

    if (StopEarlyLoc .and. CVLOOCounter >= CVLOOTrip) exit

    CVLOOIncCounter = CVLOOIncCounter + 1
    if (CVLOOTemp < CVLOOM1) CVLOOIncCounter = 0
    if (StopEarlyLoc .and. CVLOOIncCounter >= NbCVErrorIncLoc) exit
    CVLOOM1 = CVLOOTemp

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

  deallocate(s, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='s', ProcName=ProcName, stat=StatLoc)

  deallocate(w, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='w', ProcName=ProcName, stat=StatLoc)

  deallocate(u, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='u', ProcName=ProcName, stat=StatLoc)

  deallocate(InActive, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InActive', ProcName=ProcName, stat=StatLoc)

  deallocate(CorrectionMem, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='CorrectionMem', ProcName=ProcName, stat=StatLoc)

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

    if (CorrectedCVLoc) then
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

  real(rkp), dimension(:,:), intent(in)                               ::    System
  real(rkp), dimension(:), intent(in)                                 ::    Coefficients

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

  ComputeCorrectionFactor = (real(M,rkp)/real(M-NbIndices,rkp))*(One+ComputeCorrectionFactor)

  deallocate(Q, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Q', ProcName=ProcName, stat=StatLoc)

  deallocate(InvRt, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

end module
