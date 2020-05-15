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

module LinSolverOLS_Class

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

public                                                                ::    LinSolverOLS_Type

type, extends(LinSolverMethod_Type)                                   ::    LinSolverOLS_Type
  logical                                                             ::    CorrectedCV
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
  procedure, private                                                  ::    SolveUD
  procedure, private                                                  ::    SolveOD
  procedure, public                                                   ::    SolveQR
  procedure, public                                                   ::    SolveQInvR
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(LinSolverOLS_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'linsolverols'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(LinSolverOLS_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Initialized = .false.
  This%Constructed = .false.

  if (allocated(This%CVError)) deallocate(This%CVError, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CVError', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(LinSolverOLS_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%CorrectedCV = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(LinSolverOLS_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    VarC0D
  logical                                                             ::    Found

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'modified_cross_validation'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%CorrectedCV = VarL0D

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
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, CVMethod, CorrectedCV)

  class(LinSolverOLS_Type), intent(inout)                             ::    This
  class(CVMethod_Type), optional, intent(in)                          ::    CVMethod
  logical, optional, intent(in)                                       ::    CorrectedCV

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(CorrectedCV)) This%CorrectedCV = CorrectedCV

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
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(LinSolverOLS_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='modified_cross_validation', Value=ConvertToString(Value=This%CorrectedCV))

  SectionName = 'cross_validation'
  GetInput = CVMethod_Factory%GetObjectInput(Object=This%CVError, Name=SectionName, Prefix=PrefixLoc, Directory=DirectoryLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Solve(This, System, Goal, Coefficients, CVError)

  class(LinSolverOLS_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='Solve'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    M
  integer                                                             ::    N
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (M /= size(Goal,1)) call Error%Raise(Line='Incorrect system and goal sizes', ProcName=ProcName)

  if (M < N) then
    if (present(CVError)) then
      call This%SolveUD(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
    else
      call This%SolveUD(System=System, Goal=Goal, Coefficients=Coefficients)
    end if
  else
    if (present(CVError)) then
      call This%SolveOD(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
    else
      call This%SolveOD(System=System, Goal=Goal, Coefficients=Coefficients)
    end if
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SolveUD(This, System, Goal, Coefficients, CVError)

  class(LinSolverOLS_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='SolveUD'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    SystemLoc
  real(rkp), allocatable, dimension(:,:)                              ::    GoalLoc
  real(rkp), allocatable, dimension(:)                                ::    WORK
  real(rkp), dimension(1)                                             ::    WORKSIZE=0
  integer                                                             ::    LWORK
  integer                                                             ::    M
  integer                                                             ::    N
  real(rkp)                                                           ::    GoalMean
  real(rkp)                                                           ::    GoalVariance
  real(rkp)                                                           ::    MeanLoc
  real(rkp)                                                           ::    VarianceLoc
  integer                                                             ::    i
  procedure(CVFitTarget), pointer                                     ::    CVFit=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array supplied', ProcName=ProcName)

  Coefficients = Zero

  GoalMean = ComputeMean(Values=Goal)
  GoalVariance = ComputeSampleVar(Values=Goal)

  allocate(SystemLoc(M,N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

  SystemLoc = System

  allocate(GoalLoc(max(1,M,N),1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GloalLoc', ProcName=ProcName, stat=StatLoc)
  GoalLoc(1:M,1) = Goal

  call DGELS('N', M, N, 1, SystemLoc, M, GoalLoc, size(GoalLoc,1), WORKSIZE, -1, StatLoc)
  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in dgels when querying for lwork', ProcName=ProcName)

  LWORK = nint(WORKSIZE(1))

  allocate(WORK(max(1,LWORK)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

  call DGELS('N', M, N, 1, SystemLoc, M,  GoalLoc, size(GoalLoc,1), WORK, LWORK, StatLoc)

  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in dgels', ProcName=ProcName)

  Coefficients = GoalLoc(1:N,1)

  deallocate(SystemLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

  deallocate(GoalLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='GoalLoc', ProcName=ProcName, stat=StatLoc)

  deallocate(WORK, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

  if (present(CVError)) then
    CVFit => CVFitOLS_UD
    CVError = This%CVError%Calculate(Fit=CVFit, FitData=Goal)
    nullify(CVFit)
  end if

  contains

  !!----------------------------------------------------------------------------------------------------------------------------
  subroutine CVFitOLS_UD(TrainingSet, TrainingSetIndices, ValidationSet, ValidationSetIndices, Residual)

    real(rkp), dimension(:), intent(inout)                          ::    TrainingSet
    integer, dimension(:), intent(in)                               ::    TrainingSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    ValidationSet
    integer, dimension(:), intent(in)                               ::    ValidationSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    Residual

    character(*), parameter                                         ::    ProcName='CVFitOLS_UD'
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
        Residual(iiLoc) = Residual(iiLoc) - CoefficientsLoc(iLoc)*System(ValidationSetIndices(iiLoc),iLoc)
      end do
    end do
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    deallocate(CoefficientsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SolveOD(This, System, Goal, Coefficients, CVError)

  class(LinSolverOLS_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='SolveOD'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    Q
  real(rkp), allocatable, dimension(:,:)                              ::    R
  integer                                                             ::    N 
  integer                                                             ::    M 

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array supplied', ProcName=ProcName)

  allocate(Q(M,N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Q', ProcName=ProcName, stat=StatLoc)
  Q = Zero

  allocate(R(N,N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)
  R = Zero

  call ComputeQR(Matrix=System, Q=Q, R=R)

  if (present(CVError)) then
    call This%SolveQR(System=System, Goal=Goal, Coefficients=Coefficients, Q=Q, R=R, CVError=CVError)
  else
    call This%SolveQR(System=System, Goal=Goal, Coefficients=Coefficients, Q=Q, R=R)
  end if

  deallocate(Q, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Q', ProcName=ProcName, stat=StatLoc)

  deallocate(R, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SolveQR(This, System, Goal, Coefficients, Q, R, CVError)

  class(LinSolverOLS_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), dimension(:,:), intent(in)                               ::    Q
  real(rkp), dimension(:,:), intent(in)                               ::    R
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='SolveSystemQR'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer                                                             ::    i
  real(rkp), allocatable, dimension(:)                                ::    h
  real(rkp), allocatable, dimension(:)                                ::    Residual
  real(rkp), allocatable, dimension(:,:)                              ::    InvR
  procedure(CVFitTarget), pointer                                     ::    CVFit=>null()
  real(rkp)                                                           ::    GoalVariance
  real(rkp)                                                           ::    T
  integer                                                             ::    M
  integer                                                             ::    N

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array supplied', ProcName=ProcName)

  if (M < N) call Error%Raise('Procedure only meant for tall arrays', ProcName=ProcName)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array', ProcName=ProcName)
  Coefficients = Zero

  allocate(VarR2D(N,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, N
    VarR2D(i,1) = dot_product(Q(:,i), Goal)
  end do

  call DTRTRS( 'U', 'N', 'N', N, 1, R, N, VarR2D, N, StatLoc )

  Coefficients = VarR2D(:,1)

  deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  if ( present(CVError)) then
    select type (CVMethod=>This%CVError)
      type is (CVLOO_Type)

        allocate(h(M), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)
        h = Zero

        allocate(Residual(M), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Residual', ProcName=ProcName, stat=StatLoc)
        
        ! get leverage from Q1
        h = sum(Q**2,2)

        ! get cv loo
        Residual = Zero
        i = 1
        do i = 1, N
          if (.not. dabs(Coefficients(i)) > Zero) cycle
          Residual = Residual + Coefficients(i)*System(:,i)
        end do
        Residual = Goal - Residual

        CVError = Zero
        i = 1
        do i = 1, M
          CVError = CVError + (Residual(i)/(One-h(i)))**2
        end do
        CVError = CVError / real(M,rkp)
        
        if (This%CVError%IsNormalized()) then
          GoalVariance = ComputeSampleVar(Values=Goal)
          if (GoalVariance > Zero) then
            CVError = CVError / GoalVariance
          else
            CVError = Zero
          end if  
        end if

        deallocate(h, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='h', ProcName=ProcName, stat=StatLoc)

        deallocate(Residual, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

      class default
        CVFit => CVFitOLS_OD
        CVError = This%CVError%Calculate(Fit=CVFit, FitData=Goal)
        nullify(CVFit)
    end select

    if (This%CorrectedCV) then
      if (M > N) then
        allocate(InvR, source=R, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='InvR', ProcName=ProcName, stat=StatLoc)
        i = 1
        do i = 1, N-1
          InvR(i+1:N,i) = R(i,i+1:N)
        end do
        call DTRTRI('L', 'N', N, InvR, N, StatLoc)
        if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                            ProcName=ProcName)
        T = Zero
        i = 1
        do i = 1, N
          T = T + dot_product(InvR(i:N,i),InvR(i:N,i))
        end do
        T = (real(M,rkp)/real(M-N,rkp))*(One+T)
        CVError = CVError*T
        deallocate(invR, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='invR', ProcName=ProcName, stat=StatLoc)
      else
        CVError = huge(One)
      end if

    end if
    
  end if

  contains

  !!----------------------------------------------------------------------------------------------------------------------------
  subroutine CVFitOLS_OD(TrainingSet, TrainingSetIndices, ValidationSet, ValidationSetIndices, Residual)

    real(rkp), dimension(:), intent(inout)                          ::    TrainingSet
    integer, dimension(:), intent(in)                               ::    TrainingSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    ValidationSet
    integer, dimension(:), intent(in)                               ::    ValidationSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    Residual

    character(*), parameter                                         ::    ProcName='CVFitOLS_OD'
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
      if (.not. dabs(Coefficients(iLoc)) > Zero) cycle
      iiLoc = 1
      do iiLoc = 1, NbValidation
        Residual(iiLoc) = Residual(iiLoc) - CoefficientsLoc(iLoc)*System(ValidationSetIndices(iiLoc),iLoc)
      end do
    end do
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    deallocate(CoefficientsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SolveQInvR(This, System, Goal, Coefficients, Q, InvR, CVError)

  class(LinSolverOLS_Type), intent(in)                                ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    System
  real(rkp), dimension(:), intent(inout)                              ::    Goal
  real(rkp), dimension(:), intent(inout)                              ::    Coefficients
  real(rkp), dimension(:,:), intent(in)                               ::    Q
  real(rkp), dimension(:,:), intent(in)                               ::    InvR
  real(rkp), optional, intent(out)                                    ::    CVError

  character(*), parameter                                             ::    ProcName='SolveSystemQR'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  integer                                                             ::    i
  integer                                                             ::    ii
  real(rkp), allocatable, dimension(:)                                ::    h
  real(rkp), allocatable, dimension(:)                                ::    Residual
  procedure(CVFitTarget), pointer                                     ::    CVFit=>null()
  real(rkp)                                                           ::    GoalVariance
  real(rkp)                                                           ::    T
  integer                                                             ::    M
  integer                                                             ::    N

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  M = size(System,1)
  N = size(System,2)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array supplied', ProcName=ProcName)

  if (M < N) call Error%Raise('Procedure only meant for tall arrays', ProcName=ProcName)

  if (size(Coefficients,1) /= N) call Error%Raise('Incompatible coefficients array', ProcName=ProcName)

  Coefficients = Zero

  allocate(VarR1D(N), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, N 
    VarR1D(i) = dot_product(Q(:,i), Goal)
  end do

  i = 1
  do i = 1,N 
    ii = 1
    do ii = i, N
      Coefficients(i) = Coefficients(i) + VarR1D(ii)*InvR(i,ii)
    end do
  end do
  
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  if ( present(CVError)) then
    select type (CVMethod=>This%CVError)
      type is (CVLOO_Type)

        allocate(h(M), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='h', ProcName=ProcName, stat=StatLoc)
        h = Zero

        allocate(Residual(M), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Residual', ProcName=ProcName, stat=StatLoc)
        
        ! get leverage from Q1
        h = sum(Q**2,2)

        ! get cv loo
        Residual = Zero
        i = 1
        do i = 1, N
          if (.not. dabs(Coefficients(i)) > Zero) cycle
          Residual = Residual + Coefficients(i)*System(:,i)
        end do
        Residual = Goal - Residual

        CVError = Zero
        i = 1
        do i = 1, M
          CVError = CVError + (Residual(i)/(One-h(i)))**2
        end do
        CVError = CVError / real(M,rkp)
        
        if (This%CVError%IsNormalized()) then
          GoalVariance = ComputeSampleVar(Values=Goal)
          if (GoalVariance > Zero) then
            CVError = CVError / GoalVariance
          else
            CVError = Zero
          end if  
        end if

        deallocate(h, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='h', ProcName=ProcName, stat=StatLoc)

        deallocate(Residual, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

      class default
        CVFit => CVFitOLS_QInvR
        CVError = This%CVError%Calculate(Fit=CVFit, FitData=Goal)
        nullify(CVFit)
    end select

    if (This%CorrectedCV) then
      if (M > N) then
        T = Zero
        i = 1
        do i = 1, N
          T = T + dot_product(InvR(i:N,i),InvR(i:N,i))
        end do
        T = (real(M,rkp)/real(M-N,rkp))*(One+T)
        CVError = CVError*T
      else
        CVError = huge(One)
      end if

    end if

  end if

  contains

  !!----------------------------------------------------------------------------------------------------------------------------
  subroutine CVFitOLS_QInvR(TrainingSet, TrainingSetIndices, ValidationSet, ValidationSetIndices, Residual)

    real(rkp), dimension(:), intent(inout)                          ::    TrainingSet
    integer, dimension(:), intent(in)                               ::    TrainingSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    ValidationSet
    integer, dimension(:), intent(in)                               ::    ValidationSetIndices
    real(rkp), dimension(:), intent(inout)                          ::    Residual

    character(*), parameter                                         ::    ProcName='CVFitOLS_QInvR'
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
        Residual(iiLoc) = Residual(iiLoc) - CoefficientsLoc(iLoc)*System(ValidationSetIndices(iiLoc),iLoc)
      end do
    end do
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    deallocate(CoefficientsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(LinSolverOLS_Type), intent(out)                               ::    LHS
  class(LinSolverMethod_Type), intent(in)                             ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (LinSolverOLS_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%CorrectedCV = RHS%CorrectedCV
        allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%CVError', ProcName=ProcName, stat=StatLoc)
      end if
    class default
      call Error%Raise(Line='Mismatching object types', ProcName=ProcName)
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(LinSolverOLS_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%CVError)) deallocate(This%CVError, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CVError', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
