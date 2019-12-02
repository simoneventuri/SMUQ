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

module LARBasic_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use ComputingRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use LARMethod_Class                                               ,only:    LARMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type

implicit none

private

public                                                                ::    LARBasic_Type

type, extends(LARMethod_Type)                                         ::    LARBasic_Type
  logical                                                             ::    Hybrid=.true.
  class(CVErrorMethod_Type), allocatable                              ::    CVError
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
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LARBasic_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LARBasic'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LARBasic_Type), intent(inout)                               ::    This
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

    class(LARBasic_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Hybrid = .true.
    This%LASSO = .false.
    This%Tolerance = Zero

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use String_Library

    class(LARBasic_Type), intent(inout)                               ::    This
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
    real(rkp)                                                         ::    varR0D
    logical                                                           ::    Found
    
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'hybrid'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Hybrid = VarL0D

    ParameterName = 'lasso'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%LASSO = VarL0D

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Hybrid, LASSO, Tolerance, CVErrorMethod, Debug )

    use String_Library

    class(LARBasic_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Hybrid
    logical, optional, intent(in)                                     ::    LASSO
    real(rkp), optional, intent(in)                                   ::    Tolerance
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(Hybrid) ) This%Hybrid = Hybrid

    if ( present(LASSO) ) This%LASSO = LASSO

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(LARBasic_Type), intent(in)                                  ::    This
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

    call GetInput%AddParameter( Name='hybrid', Value=ConvertToString(This%Hybrid) )
    call GetInput%AddParameter( Name='tolerance', Value=ConvertToString(This%Tolerance) )
    call GetInput%AddParameter( Name='lasso', Value=ConvertToString(This%LASSO) )

    SectionName = 'cross_validation'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cross_validation'
    call GetInput%AddSection( Section=CVErrorMethod_Factory%GetObjectInput( Object=This%CVError, MainSectionName=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectoryLoc ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse( This, System, Goal, ModelSet, CoefficientsSet, CVError, Debug )

    class(LARBasic_Type), intent(in)                                  ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    integer                                                           ::    i
    integer                                                           ::    ModelSize
    type(LinSolverOLS_Type)                                           ::    OLS
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsLoc
    logical                                                           ::    ConstantModel
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    MeanLoc
    real(rkp)                                                         ::    VarianceLoc
    integer                                                           ::    M
    integer                                                           ::    N

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    M = size(System,1)
    N = size(System,2)

    if ( size(System,1) >= size(System,2) ) then

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
            if (DebugLoc) call Logger%Exiting()
            return
          end if
        end do
        GoalVariance = tiny(One)
      end if

      call OLS%Construct(CVErrorMethod=This%CVError)
      if ( present(CVError) ) then
        call OLS%SolveSystem( System=System, Goal=Goal, Coefficients=CoefficientsSet, CVError=CVError )
      else
        call OLS%SolveSystem( System=System, Goal=Goal, Coefficients=CoefficientsSet )
      end if

      ModelSet = LinSequence( SeqStart=1, SeqEnd=size(System,2) )

    else
      call This%BuildMetaModels( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,                    &
                                                                           Tolerance=This%Tolerance, ConstantModel=ConstantModel )

      if ( ConstantModel ) then
          if ( present(CVError) ) CVError = Zero
          if (DebugLoc) call Logger%Exiting()
          return
      end if

      ModelSize = size(ModelSet,1)

      if ( This%Hybrid ) then
        allocate(SystemLoc(size(System,1),ModelSize), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )

        SystemLoc = System(:,ModelSet)

        call OLS%Construct(CVErrorMethod=This%CVError)
        call OLS%SolveSystem( System=SystemLoc, Goal=Goal, Coefficients=CoefficientsSet )

        deallocate(SystemLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='SystemLoc', ProcName=ProcName, stat=StatLoc )
      end if

      if ( present(CVError) ) then
        allocate(CoefficientsLoc(size(System,2)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )
        CoefficientsLoc = Zero

        CoefficientsLoc(ModelSet) = CoefficientsSet
        CVError = This%CVError%ComputeError( Solver=This, System=System, Goal=Goal, Coefficients=CoefficientsLoc )

        deallocate(CoefficientsLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull( This, System, Goal, Coefficients, CVError, Debug )

    class(LARBasic_Type), intent(in)                                  ::    This
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
      call This%Solve( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet, CVError=CVError )
    else
      call This%Solve( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet )
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

    class(LARBasic_Type), intent(out)                                 ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (LARBasic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Tolerance = RHS%Tolerance
          LHS%Hybrid = RHS%Hybrid
          LHS%LASSO = RHS%LASSO
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
