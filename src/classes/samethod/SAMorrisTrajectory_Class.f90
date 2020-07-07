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

module SAMorrisTrajectory_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SAMethod_Class                                                ,only:    SAMethod_Type
use Input_Class                                                   ,only:    Input_Type
use Output_Class                                                  ,only:    Output_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use SampleMethod_Factory_Class                                    ,only:    SampleMethod_Factory
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Model_Class                                                   ,only:    Model_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use OnlineVarEstimator_Class                                      ,only:    OnlineVarEstimator_Type
use OnlineMeanEstimator_Class                                     ,only:    OnlineMeanEstimator_Type
use DistProb_Class                                                ,only:    DistProb_Type
use TrajectoryDesign_Class                                        ,only:    TrajectoryDesign_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    SAMorrisTrajectory_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized
  logical                                                             ::    Constructed
  type(OnlineMeanEstimator_Type), allocatable, dimension(:)           ::    MuEstimator
  type(OnlineMeanEstimator_Type), allocatable, dimension(:)           ::    MuStarEstimator
  type(OnlineVarEstimator_Type), allocatable, dimension(:)            ::    VarEstimator
  type(LinkedList1D_Type)                                             ::    MuStarHistory
  type(LinkedList1D_Type)                                             ::    MuHistory
  type(LinkedList1D_Type)                                             ::    SigmaHistory
  real(rkp), allocatable, dimension(:)                                ::    SnapShot
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Cell
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    UpdateEstimators        =>    UpdateEstimators_Cell
  procedure, public                                                   ::    UpdateHistory           =>    UpdateHistory_Cell
  procedure, public                                                   ::    TakeSnapshot            =>    TakeSnapShot_Cell
  procedure, public                                                   ::    GetSnapShot             =>    GetSnapShot_Cell
  procedure, public                                                   ::    GetMu                   =>    GetMu_Cell
  procedure, public                                                   ::    GetMuStar               =>    GetMuStar_Cell
  procedure, public                                                   ::    GetSigma                =>    GetSigma_Cell
  procedure, public                                                   ::    GetMuStarHistory        =>    GetMuStarHistory_Cell
  procedure, public                                                   ::    GetMuHistory            =>    GetMuHistory_Cell
  procedure, public                                                   ::    GetSigmaHistory         =>    GetSigmaHistory_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell 
end type

type, extends(SAMethod_Type)                                          ::    SAMorrisTrajectory_Type
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
  integer                                                             ::    CheckpointFreq
  logical                                                             ::    Silent
  integer                                                             ::    NbTrajectories
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  integer                                                             ::    ParamSampleStep
  integer                                                             ::    HistoryFreq
  type(LinkedList0D_Type)                                             ::    HistoryStep
  real(rkp), allocatable, dimension(:,:)                              ::    StepSize
  integer, allocatable, dimension(:,:)                                ::    Indices
  type(TrajectoryDesign_Type)                                         ::    ScreeningDesign
  real(rkp)                                                           ::    AbsTolerance
  real(rkp)                                                           ::    RelTolerance
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'SAMorrisTrajectory'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%ScreeningDesign%Reset()

    if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
    This%NbCells = 0

    call This%HistoryStep%Purge()

    if (allocated(This%StepSize)) deallocate(This%StepSize, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%StepSize', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Indices)) deallocate(This%Indices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%CheckpointFreq = -1
    This%HistoryFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%ParamSampleStep = 0
    This%NbTrajectories = 0
    This%RelTolerance = 1.d-3
    This%AbsTolerance = 1.d-6

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, SectionChain, Prefix)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    integer                                                           ::    ii
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    SectionName = 'trajectory_design'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%ScreeningDesign%Construct(Input=InputSection, Prefix=PrefixLoc)
    else
      call This%ScreeningDesign%Construct()
    end if

    ParameterName= 'silent'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Silent=VarL0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%CheckpointFreq = VarI0D

    ParameterName = "history_frequency"
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%HistoryFreq = VarI0D

    ParameterName = "relative_tolerance"
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%RelTolerance = VarR0D

    if (This%RelTolerance > 1.d-3) then
      This%AbsTolerance = 1.d-6
    else
      This%AbsTolerance = This%RelTolerance * 1.d-3
    end if
    ParameterName = "absolute_tolerance"
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%AbsTolerance = VarR0D

    ParameterName = 'nb_trajectories'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%NbTrajectories = VarI0D
    if (This%NbTrajectories <= 0) call Error%Raise('Must specify at least 1 for number of trajectories', ProcName=ProcName)

    SectionName = 'restart'
    if (Input%HasSection(SubSectionName=SectionName)) then

      ParameterName = 'param_sample_step'
      call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%ParamSampleStep = VarI0D

      ParameterName = 'samples_obtained'
      call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%SamplesObtained= VarL0D

      ParameterName = 'samples_ran'
      call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%SamplesRan = VarL0D

      SubSectionName = SectionName // '>indices'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%Indices, Prefix=PrefixLoc)
      nullify(InputSection)

      SubSectionName = SectionName // '>step_size'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%StepSize, Prefix=PrefixLoc)
      nullify(InputSection)

      SubSectionName = SectionName // '>param_sample'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%ParamSample, Prefix=PrefixLoc)
      nullify(InputSection)

      This%NbCells = 0
      SubSectionName = SectionName // '>cells'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      This%NbCells = InputSection%GetNumberofSubSections()
      nullify(InputSection)
      allocate(This%Cells(This%NbCells), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

      i = 1
      do i = 1, This%NbCells
        SubSectionName = SectionName // '>cells>cell' // ConvertToString(Value=i)
        call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>mean', Mandatory=.true.)
        call This%Cells(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      end do

      SubSectionName = SectionName // '>history_step'
      if (Input%HasSection(SubSectionName=SubSectionName))then
        call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
        call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
        nullify(InputSection)
        call This%HistoryStep%Append(Values=VarR1D)
        deallocate(VarR1D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      end if
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    integer, dimension(:,:), pointer                                  ::    VarI2D=>null()
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName
    integer, allocatable, dimension(:)                                ::    VarI1D

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='silent', Value=ConvertToString(Value=This%Silent))
    call GetInput%AddParameter(Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq))
    call GetInput%AddParameter(Name='history_frequency', Value=ConvertToString(Value=This%HistoryFreq))
    call GetInput%AddParameter(Name='nb_trajectories', Value=ConvertToString(Value=This%NbTrajectories))
    call GetInput%AddParameter(Name='absolute_tolerance', Value=ConvertToString(Value=This%AbsTolerance))
    call GetInput%AddParameter(Name='relative_tolerance', Value=ConvertToString(Value=This%RelTolerance))

    SectionName = 'trajectory_design'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/trajectory_design'
    call GetInput%AddSection(Section=This%ScreeningDesign%GetInput(Name=SectionName, Prefix=PrefixLoc,                &
                                                                                                        Directory=DirectorySub))

    if (This%ParamSampleStep > 0) then
      SectionName = 'restart'
      call GetInput%AddSection(SectionName=SectionName)

      if (ExternalFlag) then
        
        if (allocated(This%ParamSample)) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          FileName = DirectoryLoc // '/param_sample.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Input=InputSection, Array=This%ParamSample, File=File)
          nullify(InputSection)

          SubSectionName = 'step_size'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          FileName = DirectoryLoc // '/step_size.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Input=InputSection, Array=This%StepSize, File=File)
          nullify(InputSection)

          SubSectionName = 'indices'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          FileName = DirectoryLoc // '/indices.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Input=InputSection, Array=This%Indices, File=File)
          nullify(InputSection)

        end if

        if (This%HistoryFreq > 0) then
          call This%HistoryStep%Get(Values=VarI1D)
          SubSectionName = 'history_step'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          FileName = DirectoryLoc // '/' // SubSectionName // '.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Input=InputSection, Array=VarI1D, File=File)
          nullify(InputSection)
          deallocate(VarI1D, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
        end if

      else
        if (allocated(This%ParamSample)) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          call ExportArray(Input=InputSection, Array=This%ParamSample)
          nullify(InputSection)

          SubSectionName = 'indices'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          call ExportArray(Input=InputSection, Array=This%Indices)
          nullify(InputSection)

          SubSectionName = 'step_size'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          call ExportArray(Input=InputSection, Array=This%StepSize)
          nullify(InputSection)

        end if

        if (This%HistoryFreq > 0) then
          call This%HistoryStep%Get(Values=VarI1D)
          SubSectionName = 'history_step'
          call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true.)
          call ExportArray(Input=InputSection, Array=VarI1D)
          nullify(InputSection)
          deallocate(VarI1D, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
        end if

      end if

      call GetInput%AddParameter(Name='samples_obtained', Value=ConvertToString(Value=This%SamplesObtained),                    &
                                                                                                         SectionName=SectionName)
      call GetInput%AddParameter(Name='samples_ran', Value=ConvertToString(Value=This%SamplesRan), SectionName=SectionName)
      call GetInput%AddParameter(Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                    &
                                                                                                         SectionName=SectionName)

      SubSectionName = 'cells'
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      i = 1
      do i = 1, This%NbCells
        if (ExternalFlag) DirectorySub = DirectoryLoc // '/cell' // ConvertToString(Value=i)
        call GetInput%AddSection(Section=This%Cells(i)%GetInput(Name='cell' // ConvertToString(Value=i),             &
                                  Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName)
      end do

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run(This, SampleSpace, Responses, Model, OutputDirectory)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory

    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    type(ModelInterface_Type)                                         ::    ModelInterface
    logical                                                           ::    SilentLoc
    integer                                                           ::    NbDim
    real(rkp), pointer, dimension(:,:)                                ::    VarR2DPtr=>null()
    type(Input_Type), allocatable, dimension(:)                       ::    Input
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    v
    integer                                                           ::    iCellMin
    integer                                                           ::    iCellMax
    integer                                                           ::    iRunMin
    integer                                                           ::    iRunMax
    integer                                                           ::    NbInputs
    type(Output_Type), allocatable, dimension(:,:)                    ::    Outputs
    integer                                                           ::    NbResponses
    integer, allocatable, dimension(:)                                ::    NbCellsOutput
    character(:), allocatable                                         ::    Line
    integer, allocatable, dimension(:)                                ::    RunStatLoc
    class(DistProb_Type), pointer                                     ::    DistProbPtr=>null()
    integer                                                           ::    NbTrajectories
    logical, allocatable, dimension(:)                                ::    SampleRan
    real(rkp), allocatable, dimension(:)                              ::    TrajectoryOutput
    logical                                                           ::    Converged
    real(rkp), allocatable, dimension(:)                              ::    Delta
    real(rkp), allocatable, dimension(:)                              ::    Snapshot
    type(SMUQString_Type), allocatable, dimension(:)                  ::    Labels 

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    NbResponses = size(Responses,1)
    NbDim = SampleSpace%GetNbDim()
    SilentLoc = This%Silent

    if (SampleSpace%IsCorrelated()) then
      call Error%Raise('Morris method is only able to deal with non-correlated spaces', ProcName=ProcName)
    end if

    i = 1
    do i = 1, NbDim
      DistProbPtr => SampleSpace%GetDistributionPointer(Num=i)
      if (.not. (DistProbPtr%IsTruncatedLeft() .and. DistProbPtr%IsTruncatedRight())) call Error%Raise('Morris method ' //     &
                'implementation will yield values beyond numerical accuracy when working with distributions with infinite ' //    &
                'or semi-infinite support', ProcName=ProcName)
      nullify(DistProbPtr)
    end do

    call ModelInterface%Construct(Model=Model, Responses=Responses)

    NbInputs = 0
    NbTrajectories = 0

    allocate(TrajectoryOutput(NbDim+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TrajectoryOutput', ProcName=ProcName, stat=StatLoc)
    TrajectoryOutput = Zero

    allocate(SampleRan(NbDim+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SampleRan', ProcName=ProcName, stat=StatLoc)
    SampleRan = .false.

    allocate(NbCellsOutput(NbResponses), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc)

    allocate(Labels(SampleSpace%GetNbDim()), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)
    call SampleSpace%GetLabels(Labels=Labels)

    This%NbCells = 0
    i = 1
    do i = 1, NbResponses
      NbCellsOutput(i) = Responses(i)%GetNbNodes()
      This%NbCells = This%NbCells + NbCellsOutput(i)
    end do

    if (.not. allocated(This%Cells)) then
      allocate(This%Cells(This%NbCells), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, This%NbCells
        call This%Cells(i)%Construct(Dimensionality=NbDim)
      end do 
    end if

    SilentLoc = This%Silent

    if (This%ParamSampleStep == 0) then
      i = 1
      do i = 1, This%NbCells
        call This%Cells(i)%TakeSnapshot()
      end do
    end if

    if (This%HistoryFreq > 0 .and. This%ParamSampleStep == 0) then
      call This%HistoryStep%Append(Value=0)
      ii = 1
      do ii = 1, This%NbCells
        call This%Cells(ii)%UpdateHistory()
      end do
    end if

    !***************************************************************************************************************************
    ! Obtaining samples
    if (.not. This%SamplesObtained) then
      if (.not. SilentLoc) then
        Line = 'Initial population of trajectory samples'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if

      allocate(This%ParamSample(NbDim,This%NbTrajectories*(NbDim+1)), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

      allocate(This%Indices(NbDim,This%NbTrajectories), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

      allocate(This%StepSize(NbDim,This%NbTrajectories), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%StepSize', ProcName=ProcName, stat=StatLoc)

      call This%ScreeningDesign%Draw(NbDim=NbDim, NbTrajectories=This%NbTrajectories, Trajectories=This%ParamSample,              &
                                                                                StepSize=This%StepSize, Indices=This%Indices)

      i = 1
      do i = 1, NbDim
        DistProbPtr => SampleSpace%GetDistributionPointer(Num=i)
        ii = 1
        do ii = 1, size(This%ParamSample,2)
          This%ParamSample(i,ii) = DistProbPtr%InvCDF(P=This%ParamSample(i,ii))
        end do
        nullify(DistProbPtr)
      end do
        
      This%SamplesRan = .false.
      This%SamplesObtained = .true.
    end if

    !***************************************************************************************************************************
    ! Running samples
    if (.not. This%SamplesRan) then
      if (.not. SilentLoc) then
        Line = 'Running Samples'
        write(*,*)
        write(*,'(A)') Line
      end if

      i = This%ParamSampleStep/(NbDim+1)
      do
        if (i >= This%NbTrajectories) exit

        NbTrajectories = This%NbTrajectories - i
        if (This%CheckPointFreq > 0) NbTrajectories = min(This%CheckPointFreq, This%NbTrajectories-i)

        NbInputs = NbTrajectories*(NbDim+1)

        allocate(Input(NbInputs), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Input', ProcName=ProcName, stat=StatLoc)

        allocate(Outputs(NbResponses,NbInputs), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Outputs', ProcName=ProcName, stat=StatLoc)

        allocate(RunStatLoc(NbInputs), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='RunStatLoc', ProcName=ProcName, stat=StatLoc)
        RunStatLoc = 1

        iii = This%ParamSampleStep
        ii = 1

        do ii = 1, NbInputs
          iii = iii + 1
          call Input(ii)%Construct(Input=This%ParamSample(:,iii), Labels=Labels)
        end do

        if (.not. SilentLoc) then
          Line = '  Running Trajectories #' // ConvertToString(Value=i+1) 
          if(NbTrajectories > 1) Line = Line // '-' // ConvertToString(Value=i+NbTrajectories)
          write(*,'(A)') Line
        end if
        
        call ModelInterface%Run(Input=Input, Output=Outputs, Stat=RunStatLoc)

        ! checking if any responses are stochastic
        ii = 1
        do ii = 1, NbInputs
          iii = 1
          do iii = 1, NbResponses
            if (RunStatLoc(ii) == 0) then
              if (Outputs(iii,ii)%GetNbDegen() > 1) call Error%Raise('Morris method does not support stochastic output',       &
                                                                                                               ProcName=ProcName)
            end if
          end do
        end do

        ! processing samples
        ii = 1
        do ii = 1, NbTrajectories
          iRunMin = (ii-1)*(NbDim+1)+1
          iRunMax = iRunMin + NbDim
          SampleRan = .false.

          iii = iRunMin
          do iii = iRunMin, iRunMax
            if (RunStatLoc(iii) == 0) SampleRan(iii-iRunMin+1) = .true.
          end do

          iii = 1
          do iii = 1, NbResponses
            iCellMin = 1
            if (iii > 1) iCellMin = sum(NbCellsOutput(1:iii-1)) + 1
            iCellMax = iCellMin + NbCellsOutput(iii) - 1

            iv = iCellMin
            do iv = iCellMin, iCellMax
              v = iRunMin
              do v = iRunMin, iRunMax
                if (.not. SampleRan(v-iRunMin+1)) cycle
                VarR2DPtr => Outputs(iii,v)%GetValuesPointer()
                TrajectoryOutput(v-iRunMin+1) = VarR2DPtr(iv-iCellMin+1,1)
                nullify(VarR2DPtr)
              end do

              call This%Cells(iv)%UpdateEstimators(TrajectoryOutput=TrajectoryOutput, SampleRan=SampleRan,                       &
                                                                    StepSize=This%StepSize(:,ii+i), Indices=This%Indices(:,ii+i))
            end do
          end do

          if (This%HistoryFreq > 0 .and. (mod(i+ii, abs(This%HistoryFreq)) == 0 .and. i + ii /= This%NbTrajectories)) then
            call This%HistoryStep%Append(Value=i+ii)
            iii = 1
            do iii = 1, This%NbCells
              call This%Cells(iii)%UpdateHistory()
            end do
          end if

        end do

        deallocate(RunStatLoc, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='RunStatLoc', ProcName=ProcName, stat=StatLoc)

        deallocate(Outputs, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Outputs', ProcName=ProcName, stat=StatLoc)

        deallocate(Input, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Input', ProcName=ProcName, stat=StatLoc)

        i = i + NbTrajectories
        This%ParamSampleStep = This%ParamSampleStep + NbInputs

        Converged = .true.
        if (This%ParamSampleStep < 2) Converged = .false.
        ii = 1
        do ii = 1, This%NbCells
          call This%Cells(ii)%GetSnapShot(Values=Delta)
          call This%Cells(ii)%TakeSnapShot()
          if (.not. Converged) cycle
          call This%Cells(ii)%GetSnapShot(Values=SnapShot)
          Delta = dabs(Delta - SnapShot)

          iii = 1
          do iii = 1, size(Delta,1)
            if (Delta(iii) < max(This%AbsTolerance, This%RelTolerance*dabs(SnapShot(iii)))) cycle
            Converged = .false.
            exit
          end do
        end do

        if (Converged) then
          if (.not. SilentLoc) then
            Line = 'All cells converged'
            write(*,'(A)') '' 
            write(*,'(A)') Line
          end if
          exit
        end if

        if (i /= This%NbTrajectories) then
          call RestartUtility%Update(InputSection=This%GetInput(Name='temp', Prefix=RestartUtility%GetPrefix(),     &
                        Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain)
        end if

      end do

      This%SamplesRan = .true.

    end if

    if (This%HistoryFreq > 0) then
      call This%HistoryStep%Append(Value=i)
      ii = 1
      do ii = 1, This%NbCells
        call This%Cells(ii)%UpdateHistory()
      end do
    end if

    call RestartUtility%Update(InputSection=This%GetInput(Name='temp', Prefix=RestartUtility%GetPrefix(),         &
                      Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain)

    if (present(OutputDirectory)) call This%WriteOutput(Directory=OutputDirectory, SampleSpace=SampleSpace,                    &
                                                                                                             Responses=Responses)

    This%ParamSampleStep = 0
    
    This%SamplesObtained = .false.
    This%SamplesRan = .false.

    call This%HistoryStep%Purge()

    deallocate(SnapShot, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SnapShot', ProcName=ProcName, stat=StatLoc)

    deallocate(Delta, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Delta', ProcName=ProcName, stat=StatLoc)

    deallocate(This%ParamSample, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

    deallocate(This%StepSize, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%StepSize', ProcName=ProcName, stat=StatLoc)

    deallocate(This%Indices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

    deallocate(This%Cells, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
    This%NbCells = 0

    deallocate(SampleRan, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SampleRan', ProcName=ProcName, stat=StatLoc)

    deallocate(TrajectoryOutput, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TrajectoryOutput', ProcName=ProcName, stat=StatLoc)

    deallocate(NbCellsOutput, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc)

    deallocate(Labels, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput(This, Directory, SampleSpace, Responses)

    class(SAMorrisTrajectory_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    Directory
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses

    character(*), parameter                                           ::    ProcName='WriteOutput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    Line
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    DirectoryLoc
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    v
    integer                                                           ::    NbResponses
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    character(:), allocatable                                         ::    ResponseLabel

    if (len_trim(Directory) /= 0) then

      call MakeDirectory(Path=Directory, Options='-p')

      SilentLoc = This%Silent

      if (.not. SilentLoc) then
        Line = 'Writing solver data to the output folder'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

      PrefixLoc = Directory

      NbResponses = size(Responses)

      FileName = '/sampled_parameters.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Array=This%ParamSample(:,1:This%ParamSampleStep), File=File)

      i = 1
      ii = 1
      iii = 0
      do i = 1, NbResponses

        ResponseLabel = Responses(i)%GetLabel()

        call MakeDirectory(Path=Directory // '/' // ResponseLabel, Options='-p')

        iii = iii + Responses(i)%GetNbNodes()
        v = 0
        do iv = ii, iii
          v = v + 1

          call MakeDirectory(Path=Directory // '/' // ResponseLabel // '/cell' // ConvertToString(Value=v), Options='-p')

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetMu(Values=VarR1D)
          call ExportArray(Array=VarR1D, File=File)

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_star.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetMuStar(Values=VarR1D)
          call ExportArray(Array=VarR1D, File=File)

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sigma.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetSigma(Values=VarR1D)
          call ExportArray(Array=VarR1D, File=File)

          if (This%HistoryFreq > 0) then
            call This%HistoryStep%Get(Values=VarI1D)
            FileName = '/' // ResponseLabel // '/history_step.dat'
            call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
            call ExportArray(Array=VarI1D, File=File, RowMajor=.true.)
            deallocate(VarI1D, stat=StatLoc)
            if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_history.dat'
            call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ') 
            call This%Cells(iv)%GetMuHistory(Values=VarR2D)
            call ExportArray(Array=VarR2D, File=File, RowMajor=.false.)

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_star_history.dat'
            call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ') 
            call This%Cells(iv)%GetMuStarHistory(Values=VarR2D)
            call ExportArray(Array=VarR2D, File=File, RowMajor=.false.)

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sigma_history.dat'
            call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
            call This%Cells(iv)%GetSigmaHistory(Values=VarR2D)
            call ExportArray(Array=VarR2D, File=File, RowMajor=.false.)

            deallocate(VarR2D, stat=StatLoc)
            if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

          end if

          ii = iii + 1

        end do

      end do

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(SAMorrisTrajectory_Type), intent(out)                       ::    LHS
    class(SAMethod_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (SAMorrisTrajectory_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%Silent = RHS%Silent
          LHS%NbTrajectories = RHS%NbTrajectories
          LHS%CheckPointFreq = RHS%CheckPointFreq
          LHS%HistoryFreq = RHS%HistoryFreq
          LHS%RelTolerance = RHS%RelTolerance
          LHS%AbsTolerance = RHS%AbsTolerance
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(SAMorrisTrajectory_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
    
    if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%StepSize)) deallocate(This%StepSize, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%StepSize', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Indices)) deallocate(This%Indices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Cell(This)

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Initialize_Cell'

    if (.not. This%Initialized) then
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Cell(This)

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Reset_Cell'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%MuHistory%Purge()
    call This%MuStarHistory%Purge()
    call This%SigmaHistory%Purge
    
    if (allocated(This%MuEstimator)) deallocate(This%MuEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%MuStarEstimator)) deallocate(This%MuStarEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%VarEstimator)) deallocate(This%VarEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%SnapShot)) deallocate(This%SnapShot, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Cell(This)

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_Cell'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell(This, Input, Prefix)

    class(Cell_Type), intent(inout)                                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'nb_dim'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    NbDim = VarI0D

    SectionName = 'snapshot'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=This%SnapShot, Prefix=PrefixLoc)
    nullify(InputSection)

    allocate(This%MuEstimator(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc)

    allocate(This%MuStarEstimator(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc)

    allocate(This%VarEstimator(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc)

    SectionName = 'dimensions'
    i = 1
    do i = 1, NbDim
      SubSectionName = '>dimension' // ConvertToString(Value=i)

      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>mu_estimator',                 &
                                                                                                                Mandatory=.true.)
      call This%MuEstimator(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)

      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>mu_star_estimator',            &
                                                                                                                Mandatory=.true.)
      call This%MuStarEstimator(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)

      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>variance_estimator',           &
                                                                                                                Mandatory=.true.)
      call This%VarEstimator(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    SectionName = 'mu_history'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%MuHistory%Append(Values=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

    SectionName = 'mu_star_history'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%MuStarHistory%Append(Values=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

    SectionName = 'sigma_history'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%SigmaHistory%Append(Values=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell(This, Dimensionality)

    class(Cell_Type), intent(inout)                                   ::    This
    integer, intent(in)                                               ::    Dimensionality

    character(*), parameter                                           ::    ProcName='ConstructCase1_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    allocate(This%SnapShot(Dimensionality*3), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)
    This%SnapShot = Zero

    allocate(This%MuEstimator(Dimensionality), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc)

    allocate(This%MuStarEstimator(Dimensionality), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc)

    allocate(This%VarEstimator(Dimensionality), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, Dimensionality
      call This%MuEstimator(i)%Construct()
      call This%MuStarEstimator(i)%Construct()
      call This%VarEstimator(i)%Construct(SampleVariance=.true.)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell(This, Name, Prefix, Directory)

    use SMUQFile_Class                                            ,only:    SMUQFile_Type

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput_Cell'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    NbDim
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput_Cell%SetName(SectionName = trim(adjustl(Name)))

    NbDim = size(This%MuEstimator,1)

    SectionName = 'dimensions'
    call GetInput_Cell%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, NbDim
      SubSectionName = 'dimension' // ConvertToString(Value=i)
      call GetInput_Cell%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)

      call GetInput_Cell%AddSection(Section=This%MuEstimator(i)%GetInput(Name='mu_estimator', Prefix=PrefixLoc,       &
                                                     Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName)

      call GetInput_Cell%AddSection(Section=This%MuStarEstimator(i)%GetInput(Name='mu_star_estimator',                &
                                   Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName)

      call GetInput_Cell%AddSection(Section=This%VarEstimator(i)%GetInput(Name='variance_estimator', Prefix=PrefixLoc,&
                                                     Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName)

    end do

    if (ExternalFlag) then

      SectionName = 'snapshot'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // '/snapshot.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Input=InputSection, Array=This%SnapShot, File=File)
      nullify(InputSection)

      if (This%MuHistory%GetLength() > 0) then
        SectionName = 'mu_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        FileName = DirectoryLoc // '/mu_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call This%MuHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D, File=File)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

      if (This%MuStarHistory%GetLength() > 0) then
        SectionName = 'mu_star_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        FileName = DirectoryLoc // '/mu_star_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call This%MuStarHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D, File=File)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

      if (This%SigmaHistory%GetLength() > 0) then
        SectionName = 'sigma_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        FileName = DirectoryLoc // '/sigma_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call This%SigmaHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D, File=File)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

    else

      SectionName = 'snapshot'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      allocate(VarR1D, source=This%SnapShot, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      call ExportArray(Input=InputSection, Array=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      if (This%MuHistory%GetLength() > 0) then
        SectionName = 'mu_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        call This%MuHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

      if (This%MuStarHistory%GetLength() > 0) then
        SectionName = 'mu_star_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        call This%MuStarHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

      if (This%SigmaHistory%GetLength() > 0) then
        SectionName = 'sigma_history'
        call GetInput_Cell%AddSection(SectionName=SectionName)
        call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        call This%SigmaHistory%Get(Values=VarR2D)
        call ExportArray(Input=InputSection, Array=VarR2D)
        deallocate(VarR2D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
        nullify(InputSection)
      end if

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateEstimators_Cell(This, TrajectoryOutput, SampleRan, StepSize, Indices)

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    TrajectoryOutput
    logical, dimension(:), intent(in)                                 ::    SampleRan
    real(rkp), dimension(:), intent(in)                               ::    StepSize
    integer, dimension(:), intent(in)                                 ::    Indices

    character(*), parameter                                           ::    ProcName='UpdateEstimators_Cell'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    EE
    integer                                                           ::    NbDim
    integer                                                           ::    i
    integer                                                           ::    DimIndex

    NbDim = size(This%MuEstimator,1)
    DimIndex = 0

    i = 1
    do i = 1, NbDim
      if (SampleRan(i) .and. SampleRan(i+1)) then
        DimIndex = Indices(i)
        EE = (TrajectoryOutput(i+1)-TrajectoryOutput(i)) / StepSize(i)
        call This%MuEstimator(DimIndex)%Update(Value=EE)
        call This%MuStarEstimator(DimIndex)%Update(Value=dabs(EE))
        call This%VarEstimator(DimIndex)%Update(Value=EE)
      end if
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateHistory_Cell(This)

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='UpdateHistory_Cell'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    call This%GetMu(Values=VarR1D)
    call This%MuHistory%Append(Values=VarR1D)
    call This%GetMuStar(Values=VarR1D)
    call This%MuStarHistory%Append(Values=VarR1D)
    call This%GetSigma(Values=VarR1D)
    call This%SigmaHistory%Append(Values=VarR1D)

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetMu_Cell(This, Values)

    class(Cell_Type), intent(in)                                      ::    This
    real(rkp), allocatable, dimension(:), intent(inout)               ::    Values 

    character(*), parameter                                           ::    ProcName='GetMu_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)

    call EnsureArraySize(Array=Values, Size1=NbDIm, DefaultValue=.false.)

    i = 1
    do i = 1, NbDim
      Values(i) = This%MuEstimator(i)%GetMean()
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetMuStar_Cell(This, Values)

    class(Cell_Type), intent(in)                                      ::    This
    real(rkp), allocatable, dimension(:), intent(inout)               ::    Values 

    character(*), parameter                                           ::    ProcName='GetMuStar_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)
    call EnsureArraySize(Array=Values, Size1=NbDIm, DefaultValue=.false.)

    i = 1
    do i = 1, NbDim
      Values(i) = This%MuStarEstimator(i)%GetMean()
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetSigma_Cell(This, Values)

    class(Cell_Type), intent(in)                                      ::    This
    real(rkp), allocatable, dimension(:), intent(inout)               ::    Values 

    character(*), parameter                                           ::    ProcName='GetSigma_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)
    call EnsureArraySize(Array=Values, Size1=NbDIm, DefaultValue=.false.)

    i = 1
    do i = 1, NbDim
      Values(i) = dsqrt(This%VarEstimator(i)%GetVariance())
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetMuHistory_Cell(This, Values)

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Values 

    character(*), parameter                                           ::    ProcName='GetMuHistory_Cell'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (This%MuHistory%GetLength() > 0) then
      call This%MuHistory%Get(Values=Values)
    else
      call EnsureArraySize(Array=Values, Size1=size(This%MuEstimator,1), Size2=1)
      call This%GetMu(Values=VarR1D)
      Values(:,1) = VarR1D
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetMuStarHistory_Cell(This, Values)

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Values 

    character(*), parameter                                           ::    ProcName='GetMuStarHistory_Cell'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (This%MuStarHistory%GetLength() > 0) then
      call This%MuStarHistory%Get(Values=Values)
    else
      call EnsureArraySize(Array=Values, Size1=size(This%MuStarEstimator,1), Size2=1)
      call This%GetMuStar(Values=VarR1D)
      Values(:,1) = VarR1D
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetSigmaHistory_Cell(This, Values)

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Values 

    character(*), parameter                                           ::    ProcName='GetSigmaHistory_Cell'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (This%SigmaHistory%GetLength() > 0) then
      call This%SigmaHistory%Get(Values=Values)
    else
      call EnsureArraySize(Array=Values, Size1=size(This%VarEstimator,1), Size2=1)
      call This%GetSigma(Values=VarR1D)
      Values(:,1) = VarR1D
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine TakeSnapShot_Cell(This)

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='TakeSnapShot_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    NbDim = size(This%MuStarEstimator,1)

    call This%GetMuStar(Values=VarR1D)
    This%SnapShot(1:NbDim) = VarR1D
    call This%GetMu(Values=VarR1D)
    This%SnapShot(NbDim+1:2*NbDim) = VarR1D
    call This%GetSigma(Values=VarR1D)
    This%SnapShot(2*NbDim+1:3*NbDim) = VarR1D

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetSnapShot_Cell(This, Values)

    class(Cell_Type), intent(in)                                      ::    This
    real(rkp), allocatable, dimension(:), intent(inout)               ::    Values 

    character(*), parameter                                           ::    ProcName='GetSnapShot_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    call EnsureArraySize(Array=Values, Size1=size(This%MuEstimator,1))
    Values = This%SnapShot

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_Cell(LHS, RHS)

    class(Cell_Type), intent(out)                                     ::    LHS
    class(Cell_Type), intent(in)                                      ::    RHS

    character(*), parameter                                           ::    ProcName='Copy_Cell'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if (RHS%Constructed) then
      allocate(LHS%MuEstimator, source=RHS%MuEstimator, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%MuEstimator', ProcName=ProcName, stat=StatLoc)
      allocate(LHS%MuStarEstimator, source=RHS%MuStarEstimator, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%MuStarEstimator', ProcName=ProcName, stat=StatLoc)
      allocate(LHS%VarEstimator, source=RHS%VarEstimator, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%varEstimator', ProcName=ProcName, stat=StatLoc)
      allocate(LHS%SnapShot, source=RHS%SnapShot, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%SnapShot', ProcName=ProcName, stat=StatLoc)
      LHS%MuHistory = RHS%MuHistory
      LHS%MuStarHistory = RHS%MuStarHistory
      LHS%SigmaHistory = RHS%SigmaHistory
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_Cell(This)

    type(Cell_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    integer                                                           ::    StatLoc=0

    if (allocated(This%SnapShot)) deallocate(This%SnapShot, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%MuEstimator)) deallocate(This%MuEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%MuStarEstimator)) deallocate(This%MuStarEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%VarEstimator)) deallocate(This%VarEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc)

    call This%MuHistory%Purge()
    call This%MuStarHistory%Purge()
    call This%SigmaHistory%Purge()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
