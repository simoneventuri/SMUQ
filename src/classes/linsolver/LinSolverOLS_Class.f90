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
use StringRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type

implicit none

private

public                                                                ::    LinSolverOLS_Type

type, extends(LinSolverMethod_Type)                                   ::    LinSolverOLS_Type
  class(CVErrorMethod_Type), allocatable                              ::    CVError
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    SolveSystem
  procedure, private                                                  ::    SolveSystemUD
  procedure, private                                                  ::    SolveSystemOD
  procedure, public                                                   ::    SolveSystemQR
  procedure, private                                                  ::    ComputeCVLOOError
  procedure, private                                                  ::    ComputeCVLOOErrorQ1R
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(LinSolverOLS_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'linsolverols'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(LinSolverOLS_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized = .false.
    This%Constructed = .false.

    if (allocated(This%CVError)) deallocate(This%CVError, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%CVError', ProcName=ProcName, stat=StatLoc)

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(LinSolverOLS_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(LinSolverOLS_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    Found

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    SectionName = 'cross_validation'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if (Found) then
      call CVErrorMethod_Factory%Construct(Object=This%CVError, Input=InputSection, Prefix=PrefixLoc)
    else
      allocate(CVErrorLOO_Type :: This%CVError)
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          call CVErrorMethod%Construct(Corrected=.true.)
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, CVErrorMethod)

    use String_Library

    class(LinSolverOLS_Type), intent(inout)                           ::    This
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(CVErrorMethod)) then
      allocate(This%CVError, source=CVErrorMethod, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%CVErrorMethod', ProcName=ProcName, stat=StatLoc)
    else
      allocate(CVErrorLOO_Type :: This%CVError)
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          call CVErrorMethod%Construct(Corrected=.true.)
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(LinSolverOLS_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    SectionName = 'cross_validation'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/cross_validation'
    call GetInput%AddSection(Section=CVErrorMethod_Factory%GetObjectInput(Object=This%CVError, Name=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectoryLoc))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSystem(This, System, Goal, Coefficients, CVError)

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSystem'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    GoalMean = ComputeMean(Values=Goal)
    GoalVariance = ComputeSampleVar(Values=Goal)

    if (dsqrt(abs((GoalVariance*real(M-1,rkp))/real(M,rkp))) / abs(GoalMean) < 1e-10) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if (abs(VarianceLoc/MeanLoc) < 1e-10) then
          allocate(Coefficients(N), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
          Coefficients = Zero
          Coefficients(i) = GoalMean / MeanLoc
          if (present(CVError)) CVError = Zero
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

    if (M /= size(Goal,1)) call Error%Raise(Line='Incorrect system and goal sizes', ProcName=ProcName)

    if (M < N) then
      if (present(CVError)) then
        call This%SolveSystemUD(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
      else
        call This%SolveSystemUD(System=System, Goal=Goal, Coefficients=Coefficients)
      end if
    else
      if (present(CVError)) then
        call This%SolveSystemOD(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
      else
        call This%SolveSystemOD(System=System, Goal=Goal, Coefficients=Coefficients)
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSystemUD(This, System, Goal, Coefficients, CVError)

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSystemUD'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    real(rkp), allocatable, dimension(:,:)                            ::    GoalLoc
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    GoalMean = ComputeMean(Values=Goal)
    GoalVariance = ComputeSampleVar(Values=Goal)

    if (abs((GoalVariance*real(M-1,rkp))/real(M,rkp)) < 1e-10) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if (abs(VarianceLoc/MeanLoc) < 1e-10) then
          allocate(Coefficients(N), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
          Coefficients = Zero
          Coefficients(i) = GoalMean / MeanLoc
          if (present(CVError)) CVError = Zero
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

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

    if (present(CVError)) CVError = This%CVError%ComputeError(Solver=This, System=System, Goal=Goal, Coefficients=Coefficients)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSystemOD(This, System, Goal, Coefficients, CVError)

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSystemOD'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    real(rkp), allocatable, dimension(:,:)                            ::    GoalLoc
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:,:)                            ::    R
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    im1
    integer                                                           ::    iip1
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    GoalMean = ComputeMean(Values=Goal)
    GoalVariance = ComputeSampleVar(Values=Goal)

    if (abs((GoalVariance*real(M-1,rkp))/real(M,rkp)) < 1e-10) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if (abs(VarianceLoc/MeanLoc) < 1e-10) then
          allocate(Coefficients(N), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
          Coefficients = Zero
          Coefficients(i) = GoalMean / MeanLoc
          if (present(CVError)) CVError = Zero
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

    allocate(Q1, source=System, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q', ProcName=ProcName, stat=StatLoc)

    allocate(R(N,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)

    allocate(TAU(min(M,N)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TAU', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q1, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q1, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, N
      R(1:i,i) = Q1(1:i,i)
    end do

    call DORGQR(M, N, N, Q1, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DORGQR(M, N, N, Q1, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    allocate(VarR2D(M,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    VarR2D(:,1) = Goal

    ! solving system
    call DGEMV('T', M, N, 1.d0, Q1, M, Goal, 1, 0.d0, VarR2D(1:M,:), 1)

    VarR2D(N,1) = VarR2D(N,1) / R(N,N)
    i = 2
    do i = 2, N
      im1 = i - 1
      ii = N - im1
      iip1 = ii + 1
      VarR2D(ii,1) = (VarR2D(ii,1) - dot_product(R(ii,iip1:N), VarR2D(iip1:N,1))) / R(ii,ii)
    end do

    allocate(Coefficients(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    Coefficients = VarR2D(1:N,1)    

    if (present(CVError)) then
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          CVError = This%ComputeCVLOOErrorQ1R(System=System, Goal=Goal, Coefficients=Coefficients, Q1=Q1, R=R)
        class default
          CVError = This%CVError%ComputeError(Solver=This, System=System, Goal=Goal, Coefficients=Coefficients)
      end select
    end if

    deallocate(TAU, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TAU', ProcName=ProcName, stat=StatLoc)

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

    deallocate(R, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSystemQR(This, System, Goal, Coefficients, QR, TAU, CVError)

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), dimension(:,:), intent(in)                             ::    QR
    real(rkp), dimension(:), intent(in)                               ::    TAU
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSystemQR'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    real(rkp), allocatable, dimension(:,:)                            ::    GoalLoc
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:,:)                            ::    R
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    im1
    integer                                                           ::    iip1
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    GoalMean = ComputeMean(Values=Goal)
    GoalVariance = ComputeSampleVar(Values=Goal)

    if (abs((GoalVariance*real(M-1,rkp))/real(M,rkp)) < 1e-10) then
      i = 1
      do i = 1, N
        MeanLoc = ComputeMean(Values=System(:,i))
        VarianceLoc = ComputePopulationVar(Values=System(:,i))
        if (abs(VarianceLoc/MeanLoc) < 1e-10) then
          allocate(Coefficients(N), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
          Coefficients = Zero
          Coefficients(i) = GoalMean / MeanLoc
          if (present(CVError)) CVError = Zero
          return
        end if
      end do
      GoalVariance = tiny(One)
    end if

    if (M < N) call Error%Raise(Line='Routine optimized for overdetermined systems', ProcName=ProcName)

    if (M /= size(Goal,1)) call Error%Raise(Line='Incorrect system and goal sizes', ProcName=ProcName)

    allocate(Q1, source=QR, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

    allocate(R(N,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, N
      R(1:i,i) = Q1(1:i,i)
    end do

    call DORGQR(M, N, N, Q1, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DORGQR(M, N, N, Q1, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    allocate(VarR2D(M,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    VarR2D(:,1) = Goal

    ! solving system
    call DGEMV('T', M, N, 1.d0, Q1, M, Goal, 1, 0.d0, VarR2D(1:M,:), 1)

    VarR2D(N,1) = VarR2D(N,1) / R(N,N)
    i = 2
    do i = 2, N
      im1 = i - 1
      ii = N - im1
      iip1 = ii + 1
      VarR2D(ii,1) = (VarR2D(ii,1) - dot_product(R(ii,iip1:N), VarR2D(iip1:N,1))) / R(ii,ii)
    end do

    allocate(Coefficients(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    Coefficients = VarR2D(1:N,1) 

    if (present(CVError)) then
      select type (CVErrorMethod => This%CVError)
        type is (CVErrorLOO_Type)
          CVError = This%ComputeCVLOOErrorQ1R(System=System, Goal=Goal, Coefficients=Coefficients, Q1=Q1, R=R)
        class default
          CVError = This%CVError%ComputeError(Solver=This, System=System, Goal=Goal, Coefficients=Coefficients)
      end select
    end if

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

    deallocate(R, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCVLOOError(This, System, Goal, Coefficients)

    real(rkp)                                                         ::    ComputeCVLOOError

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    System
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), allocatable, dimension(:), intent(in)                  ::    Coefficients

    character(*), parameter                                           ::    ProcName='ComputeCVLOOError'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, dimension(:,:)                            ::    R
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
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iim1

    M = size(System,1)
    N = size(System,2)

    if (M < N) call Error%Raise(Line='This optimized routine works only with overdetermined systems', ProcName=ProcName)
    
    ! Getting Q and then Q1 from QR decomposition

    allocate(Q1, source=System, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q', ProcName=ProcName, stat=StatLoc)

    allocate(R(N,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)

    allocate(TAU(min(M,N)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TAU', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q1, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q1, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, N
      R(1:i,i) = Q1(1:i,i)
    end do

    call DORGQR(M, N, N, Q1, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DORGQR(M, N, N, Q1, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORGQR", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    deallocate(TAU, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TAU', ProcName=ProcName, stat=StatLoc)

    ! Getting diagonals of the hat matrix

    allocate(HatDiag(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)

    Q1 = Q1*Q1
    HatDiag = sum(Q1,2)

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

    ComputeCVLOOError = Zero
    i = 1
    do i = 1, M
      ComputeCVLOOError = ComputeCVLOOError + ((Goal(i) - dot_product(System(i,:),Coefficients)) / (One - HatDiag(i)))**2
    end do
    ComputeCVLOOError = ComputeCVLOOError / real(M,rkp)

    deallocate(HatDiag, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)

    if (This%CVError%IsNormalized()) then
      GoalVariance = ComputeSampleVar(Goal)
      if (GoalVariance <= Zero) GoalVariance = tiny(One)
      ComputeCVLOOError = ComputeCVLOOError / GoalVariance
    end if
    

    if (This%CVError%IsCorrected()) then
      CorrFactor = Zero
      allocate(VarR1D(N), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

      do i = 1, N
        VarR1D = Zero
        VarR1D(i) = One / R(i,i)
        ii = i+1
        do ii = i+1, N
          iim1 = ii - 1
          VarR1D(ii) = (- dot_product(R(i:iim1,ii), VarR1D(i:iim1))) / R(ii,ii)
        end do
        VarR1D(i:) = VarR1D(i:)*VarR1D(i:)
        CorrFactor = CorrFactor + sum(VarR1D(i:))
      end do

      CorrFactor = (real(M,rkp) / (real(M,rkp) - real(N,rkp))) * (One + CorrFactor)

      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

      ComputeCVLOOError = ComputeCVLOOError * CorrFactor
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCVLOOErrorQ1R(This, System, Goal, Coefficients, Q1, R)

    real(rkp)                                                         ::    ComputeCVLOOErrorQ1R

    class(LinSolverOLS_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    System
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), allocatable, dimension(:), intent(in)                  ::    Coefficients
    real(rkp), dimension(:,:), intent(in)                             ::    Q1
    real(rkp), dimension(:,:), intent(in)                             ::    R

    character(*), parameter                                           ::    ProcName='ComputeCVLOOErrorQ1R'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp)                                                         ::    CorrFactor=One
    real(rkp)                                                         ::    Prediction
    real(rkp)                                                         ::    GoalVariance
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iim1

    M = size(System,1)
    N = size(System,2)

    if (M < N) call Error%Raise(Line='This optimized routine works only with overdetermined systems', ProcName=ProcName)

    ! Getting diagonals of the hat matrix

    allocate(HatDiag(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)

    HatDiag = sum(Q1**2,2)

    ComputeCVLOOErrorQ1R = Zero
    i = 1
    do i = 1, M
      ComputeCVLOOErrorQ1R = ComputeCVLOOErrorQ1R + ((Goal(i) - dot_product(System(i,:),Coefficients)) / (One - HatDiag(i)))**2
    end do
    ComputeCVLOOErrorQ1R = ComputeCVLOOErrorQ1R / real(M,rkp)

    deallocate(HatDiag, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)

    if (This%CVError%IsNormalized()) then
      GoalVariance = ComputeSampleVar(Goal)
      if (GoalVariance <= Zero) GoalVariance = tiny(One)
      ComputeCVLOOErrorQ1R = ComputeCVLOOErrorQ1R / GoalVariance
    end if
    

    if (This%CVError%IsCorrected()) then
      CorrFactor = Zero
      allocate(VarR1D(N), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

      do i = 1, N
        VarR1D = Zero
        VarR1D(i) = One / R(i,i)
        ii = i+1
        do ii = i+1, N
          iim1 = ii - 1
          VarR1D(ii) = (- dot_product(R(i:iim1,ii), VarR1D(i:iim1))) / R(ii,ii)
        end do
        VarR1D(i:) = VarR1D(i:)*VarR1D(i:)
        CorrFactor = CorrFactor + sum(VarR1D(i:))
      end do

      CorrFactor = (real(M,rkp) / (real(M,rkp) - real(N,rkp))) * (One + CorrFactor)

      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

      ComputeCVLOOErrorQ1R = ComputeCVLOOErrorQ1R * CorrFactor
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(LinSolverOLS_Type), intent(out)                             ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (LinSolverOLS_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%CVError', ProcName=ProcName, stat=StatLoc)
        end if
      class default
        call Error%Raise(Line='Mismatching object types', ProcName=ProcName)
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
