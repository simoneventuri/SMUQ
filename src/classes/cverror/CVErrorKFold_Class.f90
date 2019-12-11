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

module CVErrorKFold_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use StatisticsRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type

implicit none

private

public                                                                ::    CVErrorKFold_Type

type, extends(CVErrorMethod_Type)                                     ::    CVErrorKFold_Type
  integer                                                             ::    NbFolds=10
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ComputeError
  procedure, public                                                   ::    GetNbFolds
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This )

    class(CVErrorKFold_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'CVErrorKFold'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This )

    class(CVErrorKFold_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This )

    class(CVErrorKFold_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%NbFolds = 10
    This%Corrected = .true.
    This%Normalized = .true.

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix )

    class(CVErrorKFold_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    integer                                                           ::    VarI0D

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    ParameterName = 'nb_folds'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%NbFolds = VarI0D

    ParameterName = 'corrected'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if( Found ) This%Corrected=VarL0D

    ParameterName = 'normalized'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if( Found ) This%Normalized=VarL0D

    This%Constructed = .true.

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, NbFolds, Corrected, Normalized )

    class(CVErrorKFold_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NbFolds
    logical, optional, intent(in)                                     ::    Corrected
    logical, optional, intent(in)                                     ::    Normalized

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%NbFolds = NbFolds

    if( present(Corrected) ) This%Corrected=Corrected

    if( present(Normalized) ) This%Normalized = Normalized

    This%Constructed = .true.

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(CVErrorKFold_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='nb_folds', Value=ConvertToString(Value=This%NbFolds) )
    call GetInput%AddParameter( Name='corrected', Value=ConvertToString( Value=This%Corrected ) )
    call GetInput%AddParameter( Name='normalized', Value=ConvertToString( Value=This%Normalized ) )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeError( This, Solver, System, Goal, Coefficients )

    use ieee_arithmetic

    real(rkp)                                                         ::    ComputeError

    class(CVErrorKFold_Type), intent(in)                              ::    This
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
    integer, allocatable, dimension(:)                                ::    FoldSize
    integer                                                           ::    TrainingSize
    integer                                                           ::    Remainder
    integer                                                           ::    RemainderEnd
    integer, allocatable, dimension(:)                                ::    FoldEdges
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    real(rkp)                                                         ::    MSE
    integer, allocatable, dimension(:)                                ::    ScrambledRowIndices

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    M = size(System,1)
    N = size(System,2)

    if ( This%NbFolds > M ) call Error%Raise( Line='System not large enough to perform the requested number of folds',            &
                                                                                                               ProcName=ProcName )      

    allocate(FoldSize(This%NbFolds), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='FoldSize', ProcName=ProcName, stat=StatLoc )

    ScrambledRowIndices = LinSequence( SeqStart=1, SeqEnd=M, SeqSkip=1, Scrambled=.true. )

    FoldSize = M / This%NbFolds
    Remainder = mod(M,This%NbFolds)
    if ( Remainder > 0 ) then
      FoldSize(1:Remainder) = FoldSize(1:Remainder) + 1
    end if

    allocate(FoldEdges(This%NbFolds+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='FoldEdges', ProcName=ProcName, stat=StatLoc )
    FoldEdges = 0

    i = 1
    do i = 1, This%NbFolds
      FoldEdges(i+1) = FoldEdges(i) + FoldSize(i)
    end do

    ComputeError = Zero

    i = 1
    do i = 1, This%NbFolds
      TrainingSize = M - FoldSize(i)
      if ( allocated(SystemLoc) ) then
        if ( size(SystemLoc,1) /= TrainingSize ) then
          deallocate(SystemLoc, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )
          deallocate(GoalLoc, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='GoalLoc', ProcName=ProcName, stat=StatLoc )
        end if
      end if
      if ( .not. allocated(SystemLoc) ) then
        allocate(SystemLoc(TrainingSize,N), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )
        allocate(GoalLoc(TrainingSize), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='GoalLoc', ProcName=ProcName, stat=StatLoc )
      end if

      iii = 0
      ii = 1
      do ii = 1, M
        if ( ii > FoldEdges(i) .and. ii <= FoldEdges(i+1) ) cycle
        iii = iii + 1
        SystemLoc(iii,:) = System(ScrambledRowIndices(ii),:)
        GoalLoc(iii) = Goal(ScrambledRowIndices(ii))
      end do

      call Solver%SolveSystem( SystemLoc, GoalLoc, CoefficientsLoc )

      iii = 0
      MSE = Zero
      do ii = 1, FoldSize(i)
        iii = ScrambledRowIndices(FoldEdges(i)+ii)
        MSE = MSE + (Goal(iii) - dot_product(System(iii,:),CoefficientsLoc))**2
      end do
      MSE = MSE / FoldSize(i)
      ComputeError = ComputeError + MSE
    end do

    ComputeError = ComputeError / This%NbFolds
    
    deallocate(FoldSize, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='FoldSize', ProcName=ProcName, stat=StatLoc )

    deallocate(ScrambledRowIndices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ScrambledIndices', ProcName=ProcName, stat=StatLoc )

    deallocate(SystemLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(GoalLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='GoalLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(FoldEdges, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='FoldSize', ProcName=ProcName, stat=StatLoc )

    if ( This%Normalized ) ComputeError = ComputeError / ComputeSampleVar(Goal)

    if ( This%Corrected ) then
      if ( N >= M - 1 ) then
        ComputeError = ieee_value(ComputeError, ieee_positive_inf)
      else
        ComputeError = ComputeError * ( M-1 ) / ( M - N - 1 )
      end if
    end if

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetNbFolds( This )

    integer                                                           ::    GetNbFolds

    class(CVErrorKFold_Type), intent(in)                              ::    This

    character(*), parameter                                           ::    ProcName='ComputeErrorDefaultQ'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbFolds = This%NbFolds

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(CVErrorKFold_Type), intent(out)                             ::    LHS
    class(CVErrorMethod_Type), intent(in)                             ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (CVErrorKFold_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%NbFolds = RHS%NbFolds
          LHS%Corrected = RHS%Corrected
          LHS%Normalized = RHS%Normalized
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
