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

module LARSubSetMCV_Class

use Input_Library
use Parameters_Library
use StatisticsRoutines_Module
use ComputingRoutines_Module
use StatisticsRoutines_Module
use QRUpdate_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use LARMethod_Class                                               ,only:    LARMethod_Type
use CVErrorMethod_Factory_Class                                   ,only:    CVErrorMethod_Factory
use CVErrorMethod_Class                                           ,only:    CVErrorMethod_Type
use CVErrorLOO_Class                                              ,only:    CVErrorLOO_Type
use CVErrorKFold_Class                                            ,only:    CVErrorKFold_Type

implicit none

private

public                                                                ::    LARSubSetMCV_Type

type, extends(LARMethod_Type)                                         ::    LARSubSetMCV_Type
  class(CVErrorMethod_Type), allocatable                              ::    CVError
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
  procedure, public                                                   ::    SelectModelCVLOO
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(LARSubSetMCV_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'LARSubSetMCV'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(LARSubSetMCV_Type), intent(inout)                           ::    This

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

    class(LARSubSetMCV_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%LASSO = .false.
    This%Tolerance = Zero
    This%CVStopRatio = 0.1
    This%CVStopMinIter = 3

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(LARSubSetMCV_Type), intent(inout)                           ::    This
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
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'lasso'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%LASSO = VarL0D

    ParameterName = 'tolerance'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Tolerance = VarR0D

    ParameterName = 'cv_stop_ratio'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%CVStopRatio = VarR0D

    ParameterName = 'cv_stop_min_iter'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%CVStopMinIter = VarI0D

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
  subroutine ConstructCase1(This, LASSO, Tolerance, CVStopRatio, CVStopMinIter, CVErrorMethod)

    class(LARSubSetMCV_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    LASSO
    real(rkp), optional, intent(in)                                   ::    Tolerance
    real(rkp), optional, intent(in)                                   ::    CVStopRatio
    integer, optional, intent(in)                                     ::    CVStopMinIter
    class(CVErrorMethod_Type), optional, intent(in)                   ::    CVErrorMethod

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(LASSO)) This%LASSO = LASSO

    if (present(Tolerance)) This%Tolerance = Tolerance

    if (present(CVStopRatio)) This%CVStopRatio = CVStopRatio

    if (present(CVStopMinIter)) This%CVStopMinIter = CVStopMinIter

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

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(LARSubSetMCV_Type), intent(in)                              ::    This
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

    call GetInput%AddParameter(Name='lasso', Value=ConvertToString(This%LASSO))
    call GetInput%AddParameter(Name='tolerance', Value=ConvertToString(This%Tolerance))
    call GetInput%AddParameter(Name='cv_stop_ratio', Value=ConvertToString(This%CVStopRatio))
    call GetInput%AddParameter(Name='cv_stop_min_iter', Value=ConvertToString(This%CVStopMinIter))

    SectionName = 'cross_validation'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/cross_validation'
    call GetInput%AddSection(Section=CVErrorMethod_Factory%GetObjectInput(Object=This%CVError, Name=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectoryLoc))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse(This, System, Goal, ModelSet, CoefficientsSet, CVError)

    class(LARSubSetMCV_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    SystemLoc
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    ModelSize
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    CVErrorLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    ModelSetLoc
    integer, allocatable, dimension(:)                                ::    BestModelSet
    real(rkp), allocatable, dimension(:)                              ::    BestCoefficientsSet
    integer                                                           ::    BestNbActive
    type(LinSolverOLS_Type)                                           ::    OLS
    integer, allocatable, dimension(:)                                ::    AddDrop
    integer                                                           ::    NbAddDrop
    integer                                                           ::    NbActive
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    OFCounter
    integer                                                           ::    OFTrip
    logical                                                           ::    ConstantModel

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    call This%BuildMetaModels(System=System, Goal=Goal, AddDrop=AddDrop, Tolerance=This%Tolerance, ConstantModel=ConstantModel)

    if (ConstantModel) then
        allocate(CoefficientsSet(1), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
        CoefficientsSet = ComputeMean(Values=Goal) / ComputeMean(Values=System(:,AddDrop(1)))
        allocate(ModelSet(1), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='ModelSet', ProcName=ProcName, stat=StatLoc)
        ModelSet = AddDrop(1)
        CVErrorLoc = Zero
        if (present(CVError)) CVError = CVErrorLoc
        return
    end if

    select type (CVErrorMethod => This%CVError)
      type is (CVErrorLOO_Type)
        call This%SelectModelCVLOO(System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,                 &
                                                                                             AddDrop=AddDrop, CVError=CVErrorLoc)
      class default

        OFCounter = 0
        OFTrip = max(This%CVStopMinIter, ceiling(This%CVStopRatio * size(AddDrop,1)))

        call OLS%Construct(CVErrorMethod=This%CVError)

        NbAddDrop = size(AddDrop,1)

        allocate(BestModelSet(N), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='BestModelSet', ProcName=ProcName, stat=StatLoc)
        BestModelSet = 0

        allocate(BestCoefficientsSet(N), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='BestCoefficientsSet', ProcName=ProcName, stat=StatLoc)
        BestCoefficientsSet = Zero

        allocate(SystemLoc(M,N), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)
        SystemLoc = Zero

        allocate(ModelSetLoc(N), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='ModelSetLoc', ProcName=ProcName, stat=StatLoc)
        ModelSetLoc = 0

        CVErrorLoc = huge(One)
        i = 1
        NbActive = 0
        do i = 1, NbAddDrop
          if (AddDrop(i) > 0) then
            NbActive = NbActive + 1
            ModelSetLoc(NbActive) = AddDrop(i)
            SystemLoc(:,NbActive) = System(:,ModelSetLoc(NbActive))
            if (i < NbAddDrop) then
              if (AddDrop(i+1) < 0) cycle
            end if
          else
            ii = abs(AddDrop(i))
            ModelSetLoc(ii:NbActive-1) = ModelSetLoc(ii+1:NbActive)
            ModelSetLoc(NbActive) = 0
            SystemLoc(:,ii:NbActive-1) = SystemLoc(:,ii+1:NbActive)
            SystemLoc(:,NbActive) = Zero
            NbActive = NbActive - 1
          end if

          call OLS%SolveSystem(System=SystemLoc(:,1:NbActive), Goal=Goal, Coefficients=VarR1D, CVError=VarR0D)

          if (VarR0D < CVErrorLoc) then
            BestNbActive = NbActive
            BestCoefficientsSet(1:NbActive) = VarR1D
            BestModelSet(1:NbActive) = ModelSetLoc(1:NbActive)
            CVErrorLoc = VarR0D
            OFCounter = 0
          else
            OFCounter = OFCounter + 1
            if (OFCounter > OFTrip) exit
          end if
        end do

        deallocate(SystemLoc, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='SystemLoc', ProcName=ProcName, stat=StatLoc)

        deallocate(ModelSetLoc, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='ModelSetLoc', ProcName=ProcName, stat=StatLoc)

        allocate(CoefficientsSet(BestNbActive), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc)
        CoefficientsSet = BestCoefficientsSet(1:BestNbActive)
        
        allocate(ModelSet(BestNbActive), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='ModelSet', ProcName=ProcName, stat=StatLoc)
        ModelSet = BestModelSet(1:BestNbActive)
        
        deallocate(BestModelSet, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='BestModelSet', ProcName=ProcName, stat=StatLoc)

        deallocate(BestCoefficientsSet, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='BestCoefficientsSet', ProcName=ProcName, stat=StatLoc)

    end select

    if (present(CVError)) CVError = CVErrorLoc

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull(This, System, Goal, Coefficients, CVError)

    class(LARSubSetMCV_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveFull'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    ModelSet
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsSet

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    if (present(CVError)) then
      call This%Solve(System, Goal, ModelSet, CoefficientsSet, CVError)
    else
      call This%Solve(System, Goal, ModelSet, CoefficientsSet)
    end if

    allocate(Coefficients(size(System,2)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
    Coefficients = Zero

    Coefficients(ModelSet) = CoefficientsSet

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SelectModelCVLOO(This, System, Goal, AddDrop, ModelSet, CoefficientsSet, CVError)

    class(LARSubSetMCV_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, dimension(:), intent(in)                                 ::    AddDrop
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), intent(out)                                            ::    CVError

    character(*), parameter                                           ::    ProcName='SelectModelCVLOO'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    BestNbActive
    real(rkp), allocatable, dimension(:)                              ::    BestCoeffs
    integer, allocatable, dimension(:)                                ::    BestModelSet
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
    integer                                                           ::    i, ii, iii, iv
    integer                                                           ::    im1, iim1, iiim1, iiip1
    real(rkp)                                                         ::    GoalVariance
    real(rkp)                                                         ::    GoalMean
    real(rkp), allocatable, dimension(:)                              ::    WORK
    integer                                                           ::    NbAddDrop
    integer, allocatable, dimension(:)                                ::    ModelSetLoc
    real(rkp)                                                         ::    WNorm
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    OFCounter
    integer                                                           ::    OFTrip
    real(8), external                                                 ::    DNRM2

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    M = size(System,1)
    N = size(System,2)

    if (N < 1) call Error%Raise(Line='Passed an empty system', ProcName=ProcName)

    P = min(M,N)

    OFCounter = 0
    OFTrip = max(This%CVStopMinIter, ceiling(This%CVStopRatio * size(AddDrop,1)))

    GoalMean = ComputeMean(Values=Goal)
    GoalVariance = ComputeSampleVar(Values=Goal, Mean=GoalMean)

    if (GoalVariance <= Zero)  GoalVariance = tiny(One)

    allocate(VarR1D_2(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D_2', ProcName=ProcName, stat=StatLoc)
    VarR1D_2 = Zero

    allocate(VarR1D_1(P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D_1', ProcName=ProcName, stat=StatLoc)
    VarR1D_1 = Zero

    allocate(BestCoeffs(P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='BestCoeffs', ProcName=ProcName, stat=StatLoc)
    BestCoeffs = Zero

    allocate(HatDiag(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)
    HatDiag = Zero

    allocate(Q1(M,P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
    Q1 = Zero

    allocate(R(P,P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Q1', ProcName=ProcName, stat=StatLoc)
    R = Zero

    allocate(WORK(P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
    WORK = Zero

    allocate(ModelSetLoc(P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ModelSetLoc', ProcName=ProcName, stat=StatLoc)
    ModelSetLoc = 0

    allocate(BestModelSet(P), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='BestModelSet', ProcName=ProcName, stat=StatLoc)
    BestModelSet = 0

    Q1 = Zero

    CVError = huge(CVError)
    NbAddDrop = size(AddDrop,1)
    BestNbActive = 0

    i = 0
    iv = 1
    do iv = 1, NbAddDrop

      ! updating q and r
      if (AddDrop(iv) > 0) then
        im1 = i
        i = i + 1
        
        ! updating QR
        if (i > 1) then
          call DGEMV('T', M, im1, 1.d0, Q1(:,1:im1), M, System(:,AddDrop(iv)), 1, 0.d0, VarR1D_2(1:im1), 1)

          WNorm = ComputeNorm(Vector=VarR1D_2(1:im1), Norm=2)
          VarR0D = dsqrt(ComputeNorm(Vector=System(:,AddDrop(iv)), Norm=2)**2 - WNorm**2)

          ii = 1
          do ii = 1, M
            VarR1D_1(1:im1) = Q1(ii,1:im1)
            Q1(ii,i) = (System(ii,AddDrop(iv)) - dot_product(VarR1D_1(1:im1),VarR1D_2(1:im1))) / VarR0D
          end do

          R(1:im1,i) = VarR1D_2(1:im1)
          R (i,i) = VarR0D
        else
          R(1,1) = ComputeNorm(Vector=System(:,AddDrop(iv)), Norm=2)
          Q1(:,1) = System(:,AddDrop(iv)) / R(1,1)
        end if

        ModelSetLoc(i) = AddDrop(iv)
        if (iv < NbAddDrop) then
          if (AddDrop(iv+1) < 0) cycle
        end if
      else
        iii = abs(AddDrop(iv))
        ModelSetLoc(iii:i-1) = ModelSetLoc(iii+1:i)
        ModelSetLoc(i) = 0
        call dqrdec(M, i, i, Q1(:,1:i), M, R(1:i,1:i), i, iii, WORK(1:max(1,i-1)))
        i = i - 1
      end if

      ! solving system
      call DGEMV('T', M, i, 1.d0, Q1(:,1:i), M, Goal, 1, 0.d0, VarR1D_2(1:M), 1)

      VarR1D_2(i) = VarR1D_2(i) / R(i,i)
      ii = 2
      do ii = 2, i
        iim1 = ii - 1
        iii = i - iim1
        iiip1 = iii + 1
        VarR1D_2(iii) = (VarR1D_2(iii) - dot_product(R(iii,iiip1:i), VarR1D_2(iiip1:i))) / R(iii,iii)
      end do

      ! computing hat diagonals
      HatDiag = sum(Q1(:,1:i)**2,2)

      ! calculating error
      CVErrorLoc = Zero
      ii = 1
      do ii = 1, M
        VarR1D_1(1:i) = System(ii, ModelSetLoc(1:i))
        CVErrorLoc = CVErrorLoc + ((Goal(ii)-dot_product(VarR1D_2(1:i), VarR1D_1(1:i))) / (One-HatDiag(ii)))**2
      end do
      CVErrorLoc = CVErrorLoc / real(M,rkp)

      if (This%CVError%IsNormalized()) CVErrorLoc = CVErrorLoc / GoalVariance

      if(This%CVError%IsCorrected()) then
        ! computing correction factor
        CorrFactor = Zero

        do ii = 1, i
          VarR1D_1(1:i) = Zero
          VarR1D_1(ii) = One / R(ii,ii)

          iii = ii+1
          do iii = ii+1, i
            iiim1 = iii - 1
            VarR1D_1(iii) = (- dot_product(R(ii:iiim1,iii), VarR1D_1(ii:iiim1))) / R(iii,iii)
          end do

          VarR1D_1(ii:i) = VarR1D_1(ii:i)*VarR1D_1(ii:i)
          CorrFactor = CorrFactor + sum(VarR1D_1(ii:i))
        end do

        CorrFactor = (real(M,rkp) / (real(M,rkp) - real(i,rkp))) * (One + CorrFactor)
        CVErrorLoc = CVErrorLoc * CorrFactor
      end if

      ! checking if the current metamodel has lower cv error
      if (CVErrorLoc < CVError) then
        CVError = CVErrorLoc
        BestNbActive = i
        BestCoeffs(1:i) = VarR1D_2(1:i)
        BestModelSet(1:i) = ModelSetLoc(1:i)
        OFCounter = 0
      else
        OFCounter = OFCounter + 1
        if (OFCounter > OFTrip) exit
      end if

    end do

    deallocate(Q1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Q1', ProcName=ProcName, stat=StatLoc)

    deallocate(R, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='R', ProcName=ProcName, stat=StatLoc)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    deallocate(HatDiag, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='HatDiag', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D_1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D_1', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D_2, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D_2', ProcName=ProcName, stat=StatLoc)

    allocate(ModelSet(BestNbActive), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ModelSet', ProcName=ProcName, stat=StatLoc)

    allocate(CoefficientsSet(BestNbActive), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)

    ModelSet = BestModelSet(1:BestNbActive)

    CoefficientsSet = BestCoeffs(1:BestNbActive)

    deallocate(BestCoeffs, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='BestCoefficients', ProcName=ProcName, stat=StatLoc)

    deallocate(BestModelSet, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='BestModelSet', ProcName=ProcName, stat=StatLoc)

    deallocate(ModelSetLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ModelSetLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(LARSubSetMCV_Type), intent(out)                             ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (LARSubSetMCV_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if (RHS%Constructed) then
          LHS%Tolerance = RHS%Tolerance
          LHS%CVStopRatio = RHS%CVStopRatio
          LHS%CVStopMinIter = RHS%CVStopMinIter
          LHS%LASSO = RHS%LASSO
          allocate(LHS%CVError, source=RHS%CVError, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%CVError', ProcName=ProcName, stat=StatLoc)
        end if
      class default
        call Error%Raise(Line='Mismatching object types', ProcName=ProcName)
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
