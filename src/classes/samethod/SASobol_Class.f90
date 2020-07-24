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

module SASobol_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SAMethod_Class                                                ,only:    SAMethod_Type
use Input_Class                                                   ,only:    Input_Type
use Output_Class                                                  ,only:    Output_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility, RestartTarget
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Model_Class                                                   ,only:    Model_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use OnlineVarEstimator_Class                                      ,only:    OnlineVarEstimator_Type
use RadialDesign_Class                                            ,only:    RadialDesign_Type
use DistProb_Class                                                ,only:    DistProb_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    SASobol_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Constructed
  type(OnlineVarEstimator_Type)                                       ::    MomentEstimator
  real(rkp), allocatable, dimension(:)                                ::    StEstimator
  real(rkp), allocatable, dimension(:)                                ::    SnapShot
  integer, allocatable, dimension(:)                                  ::    StEstimatorNbSamples
  type(LinkedList1D_Type)                                             ::    StHistory
  type(LinkedList0D_Type)                                             ::    MeanHistory
  type(LinkedList0D_Type)                                             ::    VarianceHistory
contains
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    UpdateEstimators        =>    UpdateEstimators_Cell
  procedure, public                                                   ::    UpdateHistory           =>    UpdateHistory_Cell
  procedure, public                                                   ::    TakeSnapshot            =>    TakeSnapShot_Cell
  procedure, public                                                   ::    GetSnapShot             =>    GetSnapShot_Cell
  procedure, public                                                   ::    GetSt                   =>    GetSt_Cell
  procedure, public                                                   ::    GetMean                 =>    GetMean_Cell
  procedure, public                                                   ::    GetVariance             =>    GetVariance_Cell
  procedure, public                                                   ::    GetStHistory            =>    GetStHistory_Cell
  procedure, public                                                   ::    GetMeanHistory          =>    GetMeanHistory_Cell
  procedure, public                                                   ::    GetVarianceHistory      =>    GetVarianceHistory_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell 
end type

type, extends(SAMethod_Type)                                          ::    SASobol_Type
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
  integer                                                             ::    CheckpointFreq
  logical                                                             ::    Silent
  integer                                                             ::    NbBlocks
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  integer                                                             ::    ParamSampleStep
  real(rkp)                                                           ::    AbsTolerance
  real(rkp)                                                           ::    RelTolerance
  integer                                                             ::    HistoryFreq
  type(LinkedList0D_Type)                                             ::    HistoryStep
  type(RadialDesign_Type)                                             ::    ScreeningDesign
contains
  procedure, public                                                   ::    Reset
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
subroutine Reset(This)

  class(SASobol_Type), intent(inout)                                  ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
  This%NbCells = 0

  call This%HistoryStep%Purge()

  call This%ScreeningDesign%Reset()

  This%CheckpointFreq = -1
  This%HistoryFreq = -1
  This%Silent = .false.
  This%SamplesObtained = .false.
  This%SamplesRan = .false.
  This%ParamSampleStep = 0
  This%RelTolerance = 1.d-3
  This%AbsTolerance = 1.d-6
  This%NbBlocks = 0
  
end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  class(SASobol_Type), intent(inout)                                  ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    SectionChain
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  integer                                                             ::    ii
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  integer, allocatable, dimension(:)                                  ::    VarI1D
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  character(:), allocatable                                           ::    CellSource
  character(:), allocatable                                           ::    CellFile
  type(InputReader_Type)                                              ::    CellSection
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%SectionChain = SectionChain

  ParameterName= 'silent'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Silent=VarL0D

  ParameterName = "checkpoint_frequency"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%CheckpointFreq = VarI0D

  ParameterName = "history_frequency"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%HistoryFreq = VarI0D

  ParameterName = "relative_tolerance"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%RelTolerance = VarR0D

  if (This%RelTolerance > 1.d-3) then
    This%AbsTolerance = 1.d-6
  else
    This%AbsTolerance = This%RelTolerance * 1.d-3
  end if
  ParameterName = "absolute_tolerance"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%AbsTolerance = VarR0D

  ParameterName = 'nb_blocks'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%NbBlocks = VarI0D

  if (This%NbBlocks <= 0) call Error%Raise('Must specify number of samples above zero', ProcName=ProcName)

  SectionName = 'radial_design'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%ScreeningDesign%Construct(Input=InputSection, Prefix=PrefixLoc)
  else
    call This%ScreeningDesign%Construct()
  end if

  SectionName = 'restart'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then

    ParameterName = 'param_sample_step'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%ParamSampleStep = VarI0D

    ParameterName = 'samples_obtained'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%SamplesObtained= VarL0D

    ParameterName = 'samples_ran'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%SamplesRan = VarL0D

    SubSectionName = SectionName // '>param_sample'
    call InputVerifier%AddSection(Section='param_sample', ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
    nullify(InputSection)
    This%ParamSample = VarR2D
    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

    This%NbCells = 0
    SubSectionName = SectionName // '>cells'
    call InputVerifier%AddSection(Section='cells', ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    This%NbCells = InputSection%GetNumberofSubSections()
    nullify(InputSection)
    allocate(This%Cells(This%NbCells), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbCells
      SubSectionName = SectionName // '>cells>cell' // ConvertToString(Value=i)
      call InputVerifier%AddSection(Section='cell' // ConvertToString(Value=i), ToSubSection=SectionName // '>cells')
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>mean', Mandatory=.true.)
      call This%Cells(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
    end do

    SubSectionName = SectionName // '>history_step'
    call InputVerifier%AddSection(Section='history_step', ToSubSection=SectionName)
    if (Input%HasSection(SubSectionName=SubSectionName))then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%HistoryStep%Append(Values=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    end if

    ParameterName = 'cell_source'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    CellSource = VarC0D 

    SubSectionName = SectionName // '>cells'
    call InputVerifier%AddSection(Section='cells', ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)

    select case (CellSource)
      case ('external')
        This%NbCells = InputSection%GetNumberofParameters()
        nullify(InputSection)
        allocate(This%Cells(This%NbCells), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

        i = 1
        do i = 1, This%NbCells
          ParameterName = 'cell' // ConvertToString(Value=i) // '_file'
          call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SubSectionName)
          call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
          CellFile = PrefixLoc // VarC0D
          call CellSection%Read(FileName=CellFile)
          call This%Cells(i)%Construct(Input=CellSection, Prefix=PrefixLoc)
          call CellSection%Free()
        end do
      case ('internal')
        This%NbCells = InputSection%GetNumberofSubSections()
        nullify(InputSection)
        allocate(This%Cells(This%NbCells), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
    
        i = 1
        do i = 1, This%NbCells
          call InputVerifier%AddSection(Section='cell' // ConvertToString(Value=i), ToSubSection=SubSectionName)
          call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName // '>cell' // &
                                       ConvertToString(Value=i), Mandatory=.true.)
          call This%Cells(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
          nullify(InputSection)
        end do
      case default
        call Error%Raise('Unrecognized cell source option', ProcName=ProcName)
    end select

  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(SASobol_Type), intent(inout)                                  ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    i
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    FileName
  integer, allocatable, dimension(:)                                  ::    VarI1D
  character(:), allocatable                                           ::    VarC0D
  type(InputSection_Type)                                             ::    CellInput 
  character(:), allocatable                                           ::    CellInputDir
  integer                                                             ::    UnitLoc 

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
  call GetInput%AddParameter(Name='absolute_tolerance', Value=ConvertToString(Value=This%AbsTolerance))
  call GetInput%AddParameter(Name='relative_tolerance', Value=ConvertToString(Value=This%RelTolerance))

  call GetInput%AddParameter(Name='nb_blocks', Value=ConvertToString(Value=This%NbBlocks))

  SectionName = 'radial_design'
  if (ExternalFlag) DirectorySub = DirectoryLoc // 'radial_design/'
  call GetInput%AddSection(Section=This%ScreeningDesign%GetInput(Name=SectionName, Prefix=PrefixLoc,                &
                                                                                                      Directory=DirectorySub))

  if (This%ParamSampleStep > 0) then
    SectionName = 'restart'
    call GetInput%AddSection(SectionName=SectionName)

    if (ExternalFlag) then
      DirectorySub = DirectoryLoc // 'cells/'
      CellInputDir = DirectorySub // 'cell_inputs/'

      call MakeDirectory(Path=PrefixLoc // DirectorySub, Options='-p')
      call MakeDirectory(Path=PrefixLoc // CellInputDir, Options='-p')
      call GetInput%AddParameter(Name='cell_source', Value='external', SectionName=SectionName)
      SubSectionName = 'cells'
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      i = 1
      do i = 1, This%NbCells
        VarC0D = 'cell' // ConvertToString(Value=i) // '_file'
        FileName = 'cell' // ConvertToString(Value=i) // '_input.dat'
        call GetInput%AddParameter(Name=VarC0D, Value=CellInputDir // FileName, SectionName=SectionName // '>' // SubSectionName)

        DirectorySub = DirectoryLoc // 'cells/cell' // ConvertToString(Value=i) // '/'
        CellInput = This%Cells(i)%GetInput(Name='cell' // ConvertToString(Value=i), Prefix=PrefixLoc, Directory=DirectorySub)

        call File%Construct(File=FileName, Prefix=PrefixLoc // CellInputDir)
        call File%Open(Unit=UnitLoc, Action='write', Status='replace')
        call CellInput%Write(FileUnit=UnitLoc)
        call File%Close()
        call CellInput%Free()
      end do

      if (allocated(This%ParamSample)) then
        SubSectionName = 'param_sample'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        FileName = DirectoryLoc // 'param_sample.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=This%ParamSample, File=File)
        nullify(InputSection)
      end if

      if (This%HistoryFreq > 0) then
        call This%HistoryStep%Get(Values=VarI1D)
        SubSectionName = 'history_step'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        FileName = DirectoryLoc // SubSectionName // '.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=VarI1D, File=File)
        nullify(InputSection)
        deallocate(VarI1D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      end if

    else

      call GetInput%AddParameter(Name='cell_source', Value='internal', SectionName=SectionName)
      SubSectionName = 'cells'
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      i = 1
      do i = 1, This%NbCells
        call GetInput%AddSection(Section=This%Cells(i)%GetInput(Name='cell' // ConvertToString(Value=i)), &
                                 To_SubSection=SectionName // '>' // SubSectionName)
      end do

      if (allocated(This%ParamSample)) then
        SubSectionName = 'param_sample'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        call ExportArray(Input=InputSection, Array=This%ParamSample)
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

  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, SampleSpace, Responses, Model, OutputDirectory)

  class(SASobol_Type), intent(inout)                                  ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(Model_Type), intent(inout)                                    ::    Model
  character(*), optional, intent(in)                                  ::    OutputDirectory

  character(*), parameter                                             ::    ProcName='Run'
  integer                                                             ::    StatLoc=0
  type(ModelInterface_Type)                                           ::    ModelInterface
  logical                                                             ::    SilentLoc
  integer                                                             ::    NbDim
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  real(rkp), pointer, dimension(:,:)                                  ::    VarR2DPtr=>null()
  type(Input_Type), allocatable, dimension(:)                         ::    Input
  integer                                                             ::    iCellMin
  integer                                                             ::    iCellMax
  integer                                                             ::    iRunMin
  integer                                                             ::    iRunMax
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  integer                                                             ::    iv
  integer                                                             ::    v
  integer                                                             ::    NbInputs
  integer                                                             ::    NbBlocks
  type(Output_Type), allocatable, dimension(:,:)                      ::    Outputs
  logical, allocatable, dimension(:)                                  ::    SampleRan
  integer                                                             ::    NbResponses
  integer, allocatable, dimension(:)                                  ::    NbCellsOutput
  real(rkp), allocatable, dimension(:)                                ::    Delta
  real(rkp), allocatable, dimension(:)                                ::    SnapShot
  character(:), allocatable                                           ::    Line
  integer, allocatable, dimension(:)                                  ::    RunStatLoc
  integer                                                             ::    VarI0D
  real(rkp), allocatable, dimension(:)                                ::    BlockOutput
  class(DistProb_Type), pointer                                       ::    DistProbPtr=>null()
  logical                                                             ::    Converged
  procedure(RestartTarget), pointer                                   ::    RestartInput=>null()
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Labels 

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  RestartInput => GetRestartInput

  if (SampleSpace%IsCorrelated()) then
    call Error%Raise('Sobol method is only able to deal with non-correlated spaces', ProcName=ProcName)
  end if

  call ModelInterface%Construct(Model=Model, Responses=Responses)

  NbResponses = size(Responses,1)
  NbDim = SampleSpace%GetNbDim()
  SilentLoc = This%Silent

  allocate(SnapShot(NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='SnapShot', ProcName=ProcName, stat=StatLoc)

  allocate(BlockOutput(NbDim+1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='BlockOutput', ProcName=ProcName, stat=StatLoc)
  BlockOutput = Zero

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
      Line = 'Initial population of block samples'
      write(*,'(A)') '' 
      write(*,'(A)') Line
    end if

    allocate(This%ParamSample(NbDim,This%NbBlocks*(NbDim+1)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

    call This%ScreeningDesign%Draw(NbDim=NbDim, NbBlocks=This%NbBlocks, Blocks=This%ParamSample)

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
      if (i >= This%NbBlocks) exit

      NbBlocks = This%NbBlocks - i
      if (This%CheckPointFreq > 0) NbBlocks = min(This%CheckPointFreq, This%NbBlocks-i)

      NbInputs = NbBlocks*(NbDim+1)

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
        Line = '  Running blocks #' // ConvertToString(Value=i+1) 
        if(NbBlocks > 1) Line = Line // '-' // ConvertToString(Value=i+NbBlocks)
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
      do ii = 1, NbBlocks
        iRunMin = (ii-1)*(NbDim+1)+1
        iRunMax = ii*(NbDim+1)
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
              BlockOutput(v-iRunMin+1) = VarR2DPtr(iv-iCellMin+1,1)
              nullify(VarR2DPtr)
            end do
            call This%Cells(iv)%UpdateEstimators(BlockOutput=BlockOutput, SampleRan=SampleRan)
          end do
        end do

        if (This%HistoryFreq > 0 .and. (mod(i+ii, abs(This%HistoryFreq)) == 0 .and. i + ii /= This%NbBlocks)) then
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

      i = i + NbBlocks
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

      if (i /= This%NbBlocks) call RestartUtility%Update(Input=RestartInput, SectionChain=This%SectionChain)

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

  call RestartUtility%Update(Input=RestartInput, SectionChain=This%SectionChain)

  if (present(OutputDirectory)) call This%WriteOutput(Directory=OutputDirectory, SampleSpace=SampleSpace, Responses=Responses)

  This%SamplesObtained = .false.
  This%SamplesRan = .false.

  This%ParamSampleStep = 0
  This%NbCells = 0

  call This%HistoryStep%Purge()

  deallocate(Labels, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

  deallocate(SnapShot, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='SnapShot', ProcName=ProcName, stat=StatLoc)

  deallocate(Delta, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Delta', ProcName=ProcName, stat=StatLoc)

  deallocate(This%ParamSample, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

  deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

  deallocate(BlockOutput, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='BlockOutput', ProcName=ProcName, stat=StatLoc)

  deallocate(SampleRan, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='SampleRan', ProcName=ProcName, stat=StatLoc)

  deallocate(NbCellsOutput, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc)

  nullify(RestartInput)

  contains

    !!--------------------------------------------------------------------------------------------------------------------------
    function GetRestartInput(Name, Prefix, Directory)

      type(InputSection_Type), allocatable                            ::    GetRestartInput

      character(*), intent(in)                                        ::    Name
      character(*), intent(in)                                        ::    Prefix
      character(*), intent(in)                                        ::    Directory

      character(*), parameter                                         ::    ProcName='Run'
      integer                                                         ::    StatLoc=0

      allocate(GetRestartInput, source= This%GetInput(Name=Name, Prefix=Prefix, Directory=Directory), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='GetRestartInput', ProcName=ProcName, stat=StatLoc)

    end function
    !!--------------------------------------------------------------------------------------------------------------------------

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteOutput(This, Directory, SampleSpace, Responses)

  class(SASobol_Type), intent(inout)                                  ::    This
  character(*), intent(in)                                            ::    Directory
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses

  character(*), parameter                                             ::    ProcName='WriteOutput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    Line
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    PrefixLoc
  logical                                                             ::    SilentLoc
  character(:), allocatable                                           ::    DirectoryLoc
  type(SMUQFile_Type)                                                 ::    File
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  integer                                                             ::    iv
  integer                                                             ::    v
  integer                                                             ::    NbResponses
  integer, allocatable, dimension(:)                                  ::    VarI1D
  real(rkp), allocatable, dimension(:)                                ::    VarR1D 
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D 
  character(:), allocatable                                           ::    ResponseLabel

  if (len_trim(Directory) /= 0) then

    call MakeDirectory(Path=Directory, Options='-p')

    SilentLoc = This%Silent

    if (.not. SilentLoc) then
      Line = 'Writing procedure data to the output folder'
      write(*,'(A)') ''
      write(*,'(A)') Line
    end if

    PrefixLoc = Directory

    NbResponses = size(Responses)

    FileName = 'sampled_parameters.dat'
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

        call MakeDirectory(Path=Directory // ResponseLabel // '/cell' // ConvertToString(Value=v), Options='-p')

        FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mean.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call File%Export(String=ConvertToString(Value=This%Cells(iv)%GetMean()))

        FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/variance.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call File%Export(String=ConvertToString(Value=This%Cells(iv)%GetVariance()))

        FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sobol_total.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call This%Cells(iv)%GetSt(Values=VarR1D)
        call ExportArray(Array=VarR1D, File=File, RowMajor=.true.)

        if (This%HistoryFreq > 0) then
          call This%HistoryStep%Get(Values=VarI1D)
          FileName = ResponseLabel // '/history_step.dat'
          call This%HistoryStep%Get(Values=VarI1D)
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Array=VarI1D, File=File, RowMajor=.true.)

          FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mean_history.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetMeanHistory(Values=VarR1D)
          call ExportArray(Array=VarR1D, File=File, RowMajor=.true.)

          FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/variance_history.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetVarianceHistory(Values=VarR1D)
          call ExportArray(Array=VarR1D, File=File, RowMajor=.true.)

          FileName = ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sobol_total_history.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call This%Cells(iv)%GetStHistory(Values=VarR2D)
          call ExportArray(Array=VarR2D, File=File, RowMajor=.true.)
        end if

        ii = iii + 1

      end do

    end do

  end if

  if (allocated(VarI1D)) deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

  if (allocated(VarR1D)) deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  if (allocated(VarR2D)) deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(SASobol_Type), intent(out)                                    ::    LHS
  class(SAMethod_Type), intent(in)                                    ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (SASobol_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%Silent = RHS%Silent
        LHS%ScreeningDesign = RHS%ScreeningDesign
        LHS%NbBlocks = RHS%NbBlocks
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

  type(SASobol_Type), intent(inout)                                   ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
  
  if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
!!------------------------------------------------------------------------------------------------------------------------------
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_Cell(This)

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='Reset_Cell'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%StEstimator)) deallocate(This%StEstimator, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%StEstimator', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%StEstimatorNbSamples)) deallocate(This%StEstimatorNbSamples, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%SnapShot)) deallocate(This%SnapShot, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)

  call This%StHistory%Purge()
  call This%MeanHistory%Purge()
  call This%VarianceHistory%Purge
  
  call This%MomentEstimator%Reset()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_Cell(This, Input, Prefix)

  class(Cell_Type), intent(inout)                                     ::    This
  class(InputSection_Type), intent(in)                                ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_Cell'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer, allocatable, dimension(:)                                  ::    VarI1D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    i
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  SectionName = 'moment_estimator'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call This%MomentEstimator%Construct(Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'sobol_total_estimator'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=This%StEstimator, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'snapshot'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=This%SnapShot, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'sobol_estimator_nb_samples'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=This%StEstimatorNbSamples, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'sobol_total_history'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName = SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%StHistory%Append(Values=VarR2D)
    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'mean_history'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName = SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%MeanHistory%Append(Values=VarR1D)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'variance_history'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName = SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%VarianceHistory%Append(Values=VarR1D)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1_Cell(This, Dimensionality)

  class(Cell_Type), intent(inout)                                     ::    This
  integer, intent(in)                                                 ::    Dimensionality

  character(*), parameter                                             ::    ProcName='ConstructCase1_Cell'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  allocate(This%StEstimator(Dimensionality), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%StEstimator', ProcName=ProcName, stat=StatLoc)
  This%StEstimator = Zero

  allocate(This%StEstimatorNbSamples(Dimensionality), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc)
  This%StEstimatorNbSamples = Zero

  call This%MomentEstimator%Construct(SampleVariance=.true.)

  allocate(This%SnapShot(Dimensionality), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)
  This%SnapShot = Zero

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput_Cell(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput_Cell

  class(Cell_Type), intent(inout)                                     ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput_Cell'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    FileName
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  integer, allocatable, dimension(:)                                  ::    VarI1D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  type(SMUQFile_Type)                                                 ::    File

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

  call GetInput_Cell%SetName(SectionName = trim(adjustl(Name)))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'moment_estimator/'
  SectionName = 'moment_estimator'
  call GetInput_Cell%AddSection(Section=This%MomentEstimator%GetInput(Name=SectionName,                            &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  if (ExternalFlag) then
    SectionName = 'sobol_total_estimator'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'sobol_total_estimator.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%StEstimator, File=File)
    nullify(InputSection)

    SectionName = 'snapshot'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'snapshot.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%SnapShot, File=File)
    nullify(InputSection)

    SectionName = 'sobol_estimator_nb_samples'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'sobol_estimator_nb_samples.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%StEstimatorNbSamples, File=File)
    nullify(InputSection)

    if (This%StHistory%GetLength() > 0) then
      SectionName = 'sobol_total_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'sobol_total_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%StHistory%Get(Values=VarR2D)
      call ExportArray(Input=InputSection, Array=VarR2D, File=File)
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

    if (This%MeanHistory%GetLength() > 0) then
      SectionName = 'mean_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'mean_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%MeanHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D, File=File)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

    if (This%VarianceHistory%GetLength() > 0) then
      SectionName = 'variance_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'variance_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%VarianceHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D, File=File)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
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

    SectionName = 'sobol_total_estimator'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    allocate(VarR1D, source=This%StEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    call ExportArray(Input=InputSection, Array=VarR1D)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)

    SectionName = 'sobol_estimator_nb_samples'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    allocate(VarI1D, source=This%StEstimatorNbSamples, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
    call ExportArray(Input=InputSection, Array=VarI1D)
    deallocate(VarI1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)

    if (This%StHistory%GetLength() > 0) then
      SectionName = 'sobol_total_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%StHistory%Get(Values=VarR2D)
      call ExportArray(Input=InputSection, Array=VarR2D)
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

    if (This%MeanHistory%GetLength() > 0) then
      SectionName = 'mean_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%MeanHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

    if (This%VarianceHistory%GetLength() > 0) then
      SectionName = 'variance_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%VarianceHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine UpdateEstimators_Cell(This, BlockOutput, SampleRan)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), dimension(:), intent(in)                                 ::    BlockOutput
  logical, dimension(:), intent(in)                                   ::    SampleRan

  character(*), parameter                                             ::    ProcName='UpdateEstimators_Cell'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbDim
  real(rkp)                                                           ::    Variance
  integer                                                             ::    i

  if (.not. SampleRan(1)) return 

  NbDim = size(This%StEstimator,1)

  call This%MomentEstimator%Update(Value=BlockOutput(1))
  Variance = This%MomentEstimator%GetVariance()

  i = 1
  do i = 1, NbDim
    if (SampleRan(i+1)) then
      This%StEstimator(i) = This%StEstimator(i) + (BlockOutput(1) - BlockOutput(i+1))**2
      This%StEstimatorNbSamples(i) = This%StEstimatorNbSamples(i) + 1
    end if
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine UpdateHistory_Cell(This)

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='UpdateHistory_Cell'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D

  call This%MeanHistory%Append(Value=This%GetMean())
  call This%VarianceHistory%Append(Value=This%GetVariance())
  call This%GetSt(Values=VarR1D)
  call This%StHistory%Append(Values=VarR1D)

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine TakeSnapShot_Cell(This)

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='TakeSnapShot_Cell'
  integer                                                             ::    StatLoc=0

  call This%GetSt(Values=This%SnapShot)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetSnapShot_Cell(This, Values)

  class(Cell_Type), intent(in)                                        ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values 

  character(*), parameter                                             ::    ProcName='GetSnapShot_Cell'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call EnsureArraySize(Array=Values, Size1=size(This%StEstimator,1), DefaultValue=.false.)
  Values = This%SnapShot

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetSt_Cell(This, Values)

  class(Cell_Type), intent(in)                                        ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values 

  character(*), parameter                                             ::    ProcName='GetSt_Cell'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call EnsureArraySize(Array=Values, Size1=size(This%StEstimator,1), DefaultValue=.false.)
  Values = Zero 

  i = 1
  do i = 1, size(Values,1)
    if (This%StEstimatorNbSamples(i) < 2) cycle
    Values(i) = This%StEstimator(i) / (Two*real(This%StEstimatorNbSamples(i),rkp) * This%MomentEstimator%GetVariance())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetMean_Cell(This)

  real(rkp)                                                           ::    GetMean_Cell

  class(Cell_Type), intent(in)                                        ::    This

  character(*), parameter                                             ::    ProcName='GetMean_Cell'
  integer                                                             ::    StatLoc=0

  GetMean_Cell = This%MomentEstimator%GetMean()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetVariance_Cell(This)

  real(rkp)                                                           ::    GetVariance_Cell

  class(Cell_Type), intent(in)                                        ::    This

  character(*), parameter                                             ::    ProcName='GetVariance_Cell'
  integer                                                             ::    StatLoc=0

  GetVariance_Cell = This%MomentEstimator%GetVariance()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetMeanHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values 

  character(*), parameter                                             ::    ProcName='GetMeanHistory_Cell'
  integer                                                             ::    StatLoc=0

  if (This%MeanHistory%GetLength() > 0) then
    call This%MeanHistory%Get(Values=Values)
  else
    call EnsureArraySize(Array=Values, Size1=1, DefaultValue=.false.)
    Values = This%MomentEstimator%GetMean()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetVarianceHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values 

  character(*), parameter                                             ::    ProcName='GetVariance_Cell'
  integer                                                             ::    StatLoc=0

  if (This%VarianceHistory%GetLength() > 0) then
    call This%VarianceHistory%Get(Values=Values)
  else
    call EnsureArraySize(Array=Values, Size1=1, DefaultValue=.false.)
    Values = This%MomentEstimator%GetVariance()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetStHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Values 

  character(*), parameter                                             ::    ProcName='GetStHistory_Cell'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D

  if (This%StHistory%GetLength() > 0) then
    call This%StHistory%Get(Values=Values)
  else
    call EnsureArraySize(Array=Values, Size1=size(This%StEstimator,1), Size2=1, DefaultValue=.false.)
    call This%GetSt(Values=VarR1D)
    Values(:,1) = VarR1D 
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy_Cell(LHS, RHS)

  class(Cell_Type), intent(out)                                       ::    LHS
  class(Cell_Type), intent(in)                                        ::    RHS

  character(*), parameter                                             ::    ProcName='Copy_Cell'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call LHS%Reset()
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%MomentEstimator = RHS%MomentEstimator
    allocate(LHS%StEstimator, source=RHS%StEstimator, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%StEstimator', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%StEstimatorNbSamples, source=RHS%StEstimatorNbSamples, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc)
    LHS%StHistory = RHS%StHistory
    LHS%MeanHistory = RHS%MeanHistory
    LHS%VarianceHistory = RHS%VarianceHistory
    allocate(LHS%SnapShot, source=RHS%SnapShot, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%StSnapShot', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_Cell(This)

  type(Cell_Type), intent(inout)                                      ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_Cell'
  integer                                                             ::    StatLoc=0

  if (allocated(This%StEstimator)) deallocate(This%StEstimator, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%StEstimator', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%StEstimatorNbSamples)) deallocate(This%StEstimatorNbSamples, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%SnapShot)) deallocate(This%SnapShot, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SnapShot', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
