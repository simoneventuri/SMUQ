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
use StringRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type

implicit none

private

public                                                                ::    CVErrorLOO_Type

type, extends(CVErrorMethod_Type)                                     ::    CVErrorLOO_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ComputeError
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(CVErrorLOO_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'cverrorloo'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(CVErrorLOO_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Reset'

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(CVErrorLOO_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Corrected=.true.
    This%Normalized=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    ParameterName = 'corrected'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if(Found) This%Corrected = VarL0D

    ParameterName = 'normalized'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if(Found) This%Normalized = VarL0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Corrected, Normalized)

    class(CVErrorLOO_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Corrected
    logical, optional, intent(in)                                     ::    Normalized

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if(present(Corrected)) This%Corrected = Corrected

    if(present(Normalized)) This%Normalized = Normalized

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(CVErrorLOO_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    call GetInput%AddParameter(Name='corrected', Value=ConvertToString(Value=This%Corrected))
    call GetInput%AddParameter(Name='normalized', Value=ConvertToString(Value=This%Normalized))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeError(This, Solver, System, Goal, Coefficients)

    use ieee_arithmetic

    real(rkp)                                                         ::    ComputeError

    class(CVErrorLOO_Type), intent(in)                                ::    This
    class(LinSolverMethod_Type), intent(in)                           ::    Solver
    real(rkp), dimension(:,:), intent(in)                             ::    System
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), dimension(:), intent(in)                               ::    Coefficients

    character(*), parameter                                           ::    ProcName='ComputeErrorDefaultQ'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    real(rkp), allocatable, dimension(:)                              ::    GoalLoc
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsLoc
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    allocate(SystemLoc(M-1,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    allocate(GoalLoc(M-1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GoalLoc', ProcName=ProcName, stat=StatLoc)

    allocate(CoefficientsLoc(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

    ComputeError = Zero

    i = 1
    do i = 1, M
      ii = 1
      iii = 0
      do ii = 1, M
        if (ii == i) cycle
        iii = iii + 1
        SystemLoc(iii,:) = System(ii,:)
        GoalLoc(iii) = Goal(ii)
      end do
      call Solver%SolveSystem(SystemLoc, GoalLoc, CoefficientsLoc)
      ComputeError = ComputeError + (Goal(i)-dot_product(System(i,:),CoefficientsLoc))**2
    end do
    ComputeError = ComputeError / M
    
    deallocate(SystemLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

    deallocate(GoalLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='GoalLoc', ProcName=ProcName, stat=StatLoc)

    if (This%Normalized) ComputeError = ComputeError / ComputeSampleVar(Goal)

    if (This%Corrected) then
      if (N >= M - 1) then
        ComputeError = ieee_value(ComputeError, ieee_positive_inf)
      else
        ComputeError = ComputeError * (M-1) / (M - N - 1)
      end if
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(CVErrorLOO_Type), intent(out)                               ::    LHS
    class(CVErrorMethod_Type), intent(in)                             ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (CVErrorLOO_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if (RHS%Constructed) then
          LHS%Corrected = RHS%Corrected
          LHS%Normalized = RHS%Normalized
        end if
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
