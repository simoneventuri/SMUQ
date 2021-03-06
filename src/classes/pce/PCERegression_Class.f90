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

module PCERegression_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use PCEMethod_Class                                               ,only:    PCEMethod_Type, ComputeSobolIndices
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use IndexSet_Class                                                ,only:    IndexSet_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use SampleMethod_Factory_Class                                    ,only:    SampleMethod_Factory
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use SampleEnrichScheme_Class                                      ,only:    SampleEnrichScheme_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Input_Class                                                   ,only:    Input_Type
use Output_Class                                                  ,only:    Output_Type
use LinSolverMethod_Factory_Class                                 ,only:    LinSolverMethod_Factory
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverLAR_Class                                            ,only:    LinSolverLAR_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility, RestartTarget
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use List2D_Class                                                  ,only:    List2D_Type
use Model_Class                                                   ,only:    Model_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    PCERegression_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Constructed=.false.
  type(LinkedList0D_Type)                                             ::    OutputRecord
  real(rkp), dimension(:), allocatable                                ::    Coefficients
  integer, dimension(:,:), allocatable                                ::    Indices
  real(rkp), dimension(:), allocatable                                ::    SobolIndices
  real(rkp)                                                           ::    CVError=huge(One)
  integer                                                             ::    IndexOrder=0
  type(LinkedList0D_Type)                                             ::    NbRunsHistory
  type(LinkedList0D_Type)                                             ::    OrderHistory
  type(LinkedList0D_Type)                                             ::    CardinalityHistory
  type(LinkedList0D_Type)                                             ::    CVErrorHistory
  type(LinkedList1D_Type)                                             ::    SobolIndicesHistory
contains
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  generic, public                                                     ::    AppendRecord            =>    AppendRecordR0D_Cell,   &
                                                                                                          AppendRecordR1D_Cell
  procedure, public                                                   ::    AppendRecordR0D_Cell
  procedure, public                                                   ::    AppendRecordR1D_Cell
  procedure, public                                                   ::    GetRecord               =>    GetRecord_Cell
  procedure, public                                                   ::    SetModel                =>    SetModel_Cell
  procedure, public                                                   ::    GetIndicesPointer       =>    GetIndicesPointer_Cell
  procedure, public                                                   ::    GetCoefficientsPointer  =>    GetCoeffsPointer_Cell
  procedure, public                                                   ::    GetCVError              =>    GetCVError_Cell
  procedure, public                                                   ::    GetTruncationOrder      =>    GetTruncationOrder_Cell
  procedure, public                                                   ::    GetSobolIndices         =>    GetSobolIndices_Cell
  procedure, public                                                   ::    GetCVErrorHistory       =>    GetCVErrorHistory_Cell
  procedure, public                                                   ::    GetOrderHistory         =>    GetOrderHistory_Cell
  procedure, public                                                   ::    GetNbRunsHistory        =>    GetNbRunsHistory_Cell
  procedure, public                                                   ::    GetCardinalityHistory   =>  GetCardinalityHistory_Cell
  procedure, public                                                   ::    GetSobolIndicesHistory  => GetSobolIndicesHistory_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell  
end type

type, extends(PCEMethod_Type)                                         ::    PCERegression_Type
  logical                                                             ::    Silent
  real(rkp)                                                           ::    StopError
  integer                                                             ::    ModelRunCounter
  integer                                                             ::    MaxNumOverfit
  integer                                                             ::    CheckpointFreq
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells  
  integer                                                             ::    NbCells
  class(LinSolverMethod_Type), allocatable                            ::    Solver
  real(rkp), allocatable, dimension(:,:)                              ::    ParamRecord
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  class(SampleMethod_Type), allocatable                               ::    Sampler
  type(SampleEnrichScheme_Type)                                       ::    SampleEnrichScheme
  integer, allocatable, dimension(:)                                  ::    ParamSampleRan
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  logical                                                             ::    SamplesAnalyzed
  integer                                                             ::    ParamSampleStep
  integer                                                             ::    NbSamples
  integer                                                             ::    iStage
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    BuildModel
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(PCERegression_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
  This%NbCells = 0

  if (allocated(This%ParamRecord)) deallocate(This%ParamRecord, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc)
  
  if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamSampleRan)) deallocate(This%ParamSampleRan, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sampler)) deallocate(This%Sampler, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)

  call This%SampleEnrichScheme%Reset()
  This%iStage = 0

  This%MaxNumOverfit = 3
  This%StopError = Zero
  This%SectionChain = ''
  This%CheckpointFreq = -1
  This%Silent = .false.
  This%SamplesObtained = .false.
  This%SamplesRan = .false.
  This%SamplesAnalyzed = .false.
  This%ParamSampleStep = 0
  This%ModelRunCounter = 0
  This%NbSamples = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  class(PCERegression_Type), intent(inout)                            ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    SectionChain
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer, allocatable, dimension(:)                                  ::    VarI1D
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

  ParameterName = 'nb_samples'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%NbSamples = VarI0D
  if (This%NbSamples <= 0) call Error%Raise('Must specify number of samples above 0', ProcName=ProcName)

  ParameterName = "max_num_overfit"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%MaxNumOverfit = VarI0D

  ParameterName = "checkpoint_frequency"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%CheckpointFreq = VarI0D

  ParameterName = "stop_error"
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%StopError = VarR0D

  SectionName = 'sampler'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call SampleMethod_Factory%Construct(Object=This%Sampler, Input=InputSection, Prefix=PrefixLoc)
  else
    allocate(SampleLHS_Type     :: This%Sampler)
    select type (Object => This%Sampler)
      type is (SampleLHS_Type)
        call Object%Construct()
      class default
        call Error%Raise(Line='Something went wrong', ProcName=ProcName)
    end select
  end if

  SectionName = 'sample_enrichment'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%SampleEnrichScheme%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
  else
    call This%SampleEnrichScheme%Construct(MaxNbSamples=This%NbSamples)
  end if

  if (This%NbSamples > This%SampleEnrichScheme%GetMaxNbSamples()) call Error%Raise(                                        &
                                        'Specified number of samples greater than maximum number of samples', ProcName=ProcName)

  SectionName = "solver"
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call LinSolverMethod_Factory%Construct(Object=This%Solver, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
  else 
    allocate(LinSolverLAR_Type     :: This%Solver)
    select type (Object => This%Solver)
      type is (LinSolverLAR_Type)
        call Object%Construct()
      class default
        call Error%Raise(Line='Something went wrong', ProcName=ProcName)
    end select
  end if

  SectionName = 'restart'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then

    ParameterName = 'model_run_counter'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%ModelRunCounter = VarI0D

    SubSectionName = SectionName // '>param_record'
    call InputVerifier%AddSection(Section='param_record', ToSubSection=SectionName)
    if (Input%HasSection(SubSectionName=SubSectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
      nullify(InputSection)
      This%ParamRecord = VarR2D
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    end if

    SubSectionName = SectionName // '>param_sample'
    call InputVerifier%AddSection(Section='param_sample' ,ToSubSection=SectionName)
    if (Input%HasSection(SubSectionName=SubSectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
      nullify(InputSection)
      This%ParamSample = VarR2D
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

      SubSectionName = SectionName // '>param_sample_ran'
      call InputVerifier%AddSection(Section='param_sample_ran' ,ToSubSection=SectionName)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarI1D, Prefix=PrefixLoc)
      nullify(InputSection)
      This%ParamSampleRan = VarI1D

      ParameterName = 'param_sample_step'
      call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
      call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%ParamSampleStep = VarI0D
    end if

    ParameterName = 'stage'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%iStage = VarI0D

    ParameterName = 'samples_obtained'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%SamplesObtained= VarL0D

    ParameterName = 'samples_ran'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%SamplesRan = VarL0D

    ParameterName = 'samples_analyzed'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%SamplesAnalyzed = VarL0D

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

  if (This%MaxNumOverfit < 2) call Error%Raise(Line='Number of allowable overfits below minimum off 2', ProcName=ProcName)

  if (This%StopError < Zero) call Error%Raise(Line='Stop error below minimum of zero', ProcName=ProcName)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(PCERegression_Type), intent(inout)                            ::    This
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
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    FileName
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
  call GetInput%AddParameter(Name='max_num_overfit', Value=ConvertToString(Value=This%MaxNumOverfit))
  call GetInput%AddParameter(Name='stop_error', Value=ConvertToString(Value=This%StopError))
  call GetInput%AddParameter(Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'sample_enrichment/'
  SectionName = 'sample_enrichment'
  call GetInput%AddSection(Section=This%SampleEnrichScheme%GetInput(Name=SectionName,                               &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  call GetInput%AddParameter(Name='nb_samples', Value=ConvertToString(Value=This%NbSamples))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'sampler/'
  call GetInput%AddSection(Section=SampleMethod_Factory%GetObjectInput(Object=This%Sampler, Name='sampler',        &
                                                                                    Prefix=PrefixLoc, Directory=DirectorySub))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'sparse_solver/'
  SectionName='solver'
  call GetInput%AddSection(Section=LinSolverMethod_Factory%GetObjectInput(Object=This%Solver, Name=SectionName,  &
                                                                                    Prefix=PrefixLoc, Directory=DirectorySub))

  if (This%ModelRunCounter > 0) then
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

      if (allocated(This%ParamRecord)) then
        SubSectionName = 'param_record'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        FileName = DirectoryLoc // 'param_record.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=This%ParamRecord, File=File)
        nullify(InputSection)
      end if

      if (allocated(This%ParamSample)) then
        SubSectionName = 'param_sample'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        FileName = DirectoryLoc // 'param_sample.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=This%ParamSample, File=File)
        nullify(InputSection)

        SubSectionName = 'param_sample_ran'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        FileName = DirectoryLoc // 'param_sample_ran.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=This%ParamSampleRan, File=File)
        nullify(InputSection)

        call GetInput%AddParameter(Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                &
                                                                                                        SectionName=SectionName)
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

      if (allocated(This%ParamRecord)) then
        SubSectionName = 'param_record'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        call ExportArray(Input=InputSection, Array=This%ParamRecord)
        nullify(InputSection)
      end if

      if (allocated(This%ParamSample)) then
        SubSectionName = 'param_sample'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        call ExportArray(Input=InputSection, Array=This%ParamSample)
        nullify(InputSection)

        SubSectionName = 'param_sample_ran'
        call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
        call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                              Mandatory=.true.)
        call ExportArray(Input=InputSection, Array=This%ParamSampleRan)
        nullify(InputSection)

        call GetInput%AddParameter(Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                &
                                                                                                        SectionName=SectionName)
      end if

    end if

    call GetInput%AddParameter(Name='stage', Value=ConvertToString(Value=This%iStage), SectionName=SectionName)
    call GetInput%AddParameter(Name='model_run_counter', Value=ConvertToString(Value=This%ModelRunCounter),                   &
                                                                                                        SectionName=SectionName)
    call GetInput%AddParameter(Name='samples_obtained', Value=ConvertToString(Value=This%SamplesObtained),                    &
                                                                                                        SectionName=SectionName)
    call GetInput%AddParameter(Name='samples_ran', Value=ConvertToString(Value=This%SamplesRan), SectionName=SectionName)
    call GetInput%AddParameter(Name='samples_processed', Value=ConvertToString(Value=This%SamplesAnalyzed),                  &
                                                                                                        SectionName=SectionName)

  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine BuildModel(This, Basis, SampleSpace, Responses, Model, IndexSetScheme, Coefficients, Indices, CVErrors,              &
                                                                                  OutputDirectory, InputSamples, OutputSamples)

  class(PCERegression_Type), intent(inout)                            ::    This
  type(OrthoMultiVar_Type), intent(inout)                             ::    Basis
  class(SampleSpace_Type), intent(inout)                              ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(Model_Type), intent(inout)                                    ::    Model
  type(IndexSetScheme_Type), intent(in)                               ::    IndexSetScheme
  type(LinkedList0D_Type), allocatable, dimension(:), intent(out)     ::    CVErrors
  type(LinkedList1D_Type), allocatable, dimension(:), intent(out)     ::    Coefficients
  type(LinkedList2D_Type), allocatable, dimension(:), intent(out)     ::    Indices
  character(*), optional, intent(in)                                  ::    OutputDirectory
  real(rkp), optional, dimension(:,:), intent(in)                     ::    InputSamples
  type(List2D_Type), dimension(:), optional, intent(in)               ::    OutputSamples

  character(*), parameter                                             ::    ProcName='BuildModel'
  integer                                                             ::    StatLoc=0
  integer, allocatable, dimension(:,:)                                ::    IndicesLoc
  integer, dimension(:,:), pointer                                    ::    IndicesPointer=>null()
  integer                                                             ::    NbIndices
  real(rkp), allocatable, dimension(:,:)                              ::    DesignSpace
  integer                                                             ::    NbDim
  type(Output_Type), allocatable, dimension(:,:)                      ::    Outputs
  integer                                                             ::    NbOutputs
  type(Input_Type), allocatable, dimension(:)                         ::    Input
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  real(rkp), allocatable, dimension(:)                                ::    Goal
  real(rkp), allocatable, dimension(:)                                ::    CoefficientsLoc
  real(rkp), dimension(:), pointer                                    ::    VarR1DPtr=>null()
  real(rkp), dimension(:,:), pointer                                  ::    VarR2DPtr=>null()
  integer, pointer, dimension(:,:)                                    ::    VarI2DPtr=>null()
  integer                                                             ::    VarI0D
  real(rkp)                                                           ::    CVError
  real(rkp)                                                           ::    CVErrorTrip
  integer                                                             ::    i, iStart, iEnd
  integer                                                             ::    OverfitCounter
  integer                                                             ::    ii, iii, iv
  integer                                                             ::    iRun
  integer                                                             ::    im1
  integer                                                             ::    M, N
  logical                                                             ::    ConvergedFlag
  logical                                                             ::    StepExceededFlag
  logical                                                             ::    SilentLoc
  character(:), allocatable                                           ::    Line
  integer, allocatable, dimension(:)                                  ::    NbCellsOutput
  integer                                                             ::    iMin
  integer                                                             ::    iMax
  type(ModelInterface_Type)                                           ::    ModelInterface
  integer                                                             ::    ParamRecordLength
  integer                                                             ::    NbInputs
  class(IndexSet_Type), pointer                                       ::    IndexSetPointer=>null()
  integer                                                             ::    IndexStartOrder
  integer                                                             ::    IndexMaxOrder
  integer                                                             ::    IndexOrder
  procedure(RestartTarget), pointer                                   ::    RestartInput=>null()
  logical                                                             ::    EarlyExitFlag
  logical                                                             ::    NotOrderExceeded 
  logical                                                             ::    NotConverged
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Labels 

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  RestartInput => GetRestartInput

  call ModelInterface%Construct(Model=Model, Responses=Responses)

  NbOutputs = size(Responses,1)
  NbDim = SampleSpace%GetNbDim()

  allocate(Labels(NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)
  call SampleSpace%GetLabels(Labels=Labels) 

  allocate(NbCellsOutput(NbOutputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc)

  This%NbCells = 0
  i = 1
  do i = 1, NbOutputs
    NbCellsOutput(i) = Responses(i)%GetNbNodes()
    This%NbCells = This%NbCells + NbCellsOutput(i)
  end do

  if (.not. allocated(This%Cells)) then
    allocate(This%Cells(This%NbCells), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
    i = 1
    do i = 1, This%NbCells
      call This%Cells(i)%Construct()
    end do 
  end if

  SilentLoc = This%Silent

  if (This%ModelRunCounter == 0) then
    if ((present(InputSamples) .and. .not. present(OutputSamples)) .or.                                                         &
                                                                  (present(OutputSamples) .and. .not. present(InputSamples)))   &
              call Error%Raise(Line='Need both parameter and output samples to be passed at the same time', ProcName=ProcName)

    if (present(InputSamples)) then
      if (.not. SilentLoc) then
        Line = 'Processing precomputed samples'
        write(*,'(A)') '' 
        write(*,'(A)') Line
        write(*,'(A)') '' 
      end if

      if (size(InputSamples,1) /= NbDim) call Error%Raise(Line='Dimensionality of provided samples does not match ' //          &
                                                                    'the dimensionality of the input space', ProcName=ProcName)

      This%ParamRecord = InputSamples

      i = 1
      do i = 1, NbOutputs
        call OutputSamples(i)%GetPointer(Values=VarR2DPtr)
        if (size(VarR2DPtr,1) /= size(InputSamples,2)) call Error%Raise('Mismatch in number of input and output samples'    &
                                                                                                            , ProcName=ProcName)
        if (size(VarR2DPtr,2) /= NbCellsOutput(i)) call Error%Raise('Mismatch in number of nodes in response and ' //       &
                                                                                    'initial output samples', ProcName=ProcName)

        if (i > 1) then
          iMin = sum(NbCellsOutput(1:i-1)) + 1
        else
          iMin = 1
        end if

        iMax = sum(NbCellsOutput(1:i))
        iii = 0
        ii = iMin
        do ii = iMin, iMax
          iii = iii + 1
          call This%Cells(ii)%AppendRecord(Entries=VarR2DPtr(:,iii))
        end do
        nullify(VarR2DPtr)
      end do
      This%ModelRunCounter = size(This%ParamRecord,2)
      This%SamplesObtained = .true.
      This%SamplesRan = .true.
      This%SamplesAnalyzed = .false.
      This%iStage = 0
      i = 1
      do i = 1,  This%NbCells
        This%Cells(i)%IndexOrder = IndexSetScheme%GetOrder()
      end do
    end if
  end if

  StepExceededFlag = .false.

  IndexStartOrder = IndexSetScheme%GetOrder()
  IndexMaxOrder = IndexSetScheme%GetMaxOrder()
  IndexSetPointer => IndexSetScheme%GetIndexSetPointer()

  ParamRecordLength = 0
  if (allocated(This%ParamRecord)) ParamRecordLength = size(This%ParamRecord,2)

  do

    ! Checks if all cells converged during last iteration
    ConvergedFlag = .true.
    EarlyExitFlag = .true.
    i = 1
    do i = 1, This%NbCells
      NotConverged = This%Cells(i)%GetCVError() > This%StopError
      NotOrderExceeded = This%Cells(i)%GetTruncationOrder() < IndexMaxOrder
      if (NotConverged .and. ConvergedFlag) ConvergedFlag = .false.
      if (NotConverged .and. NotOrderExceeded) EarlyExitFlag = .false.
      if (.not. EarlyExitFlag) exit
    end do

    if (ConvergedFlag) then
      if (.not. SilentLoc) then
        Line = 'All nodes converged'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if   
      exit
    end if

    if (EarlyExitFlag) then
      if (.not. SilentLoc) then
        Line = 'MMaximum truncation order reached for non-converged nodes'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if   
      exit
    end if

    !***************************************************************************************************************************
    ! Obtaining samples
    if (.not. This%SamplesObtained) then
      if (.not. allocated(This%ParamRecord)) then
        if (.not. SilentLoc) then
          Line = 'Initial population of the linear system'
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if
        call SampleSpace%Draw(Sampler=This%Sampler, NbSamples=This%NbSamples, Samples=This%ParamSample)
        This%iStage = 0
      else
        if (.not. SilentLoc) then
          Line = 'Performing enrichment'
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if
        This%iStage = This%iStage + 1
        VarI0D = This%SampleEnrichScheme%GetNbEnrichSamples(NbSamples=size(This%ParamRecord,2), Stage=This%iStage)
        if (size(This%ParamRecord,2) + VarI0D > This%SampleEnrichScheme%GetMaxNbSamples()) then
          StepExceededFlag = .true.
          exit
        end if
        call SampleSpace%Enrich(Sampler=This%Sampler, NbEnrichmentSamples=VarI0D, Samples=This%ParamRecord,                    &
                                                                                            EnrichmentSamples=This%ParamSample)
      end if
      This%SamplesRan = .false.
      This%SamplesObtained = .true.
      allocate(This%ParamSampleRan(size(This%ParamSample,2)), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc)
      This%ParamSampleRan = 1
    end if

    !***************************************************************************************************************************
    ! Running samples
    if (.not. This%SamplesRan) then
      if (.not. SilentLoc) then
        Line = 'Running Samples'
        write(*,'(A)') Line
      end if

      iEnd = size(This%ParamSample,2)
      i = This%ParamSampleStep
      do
        if (i >= iEnd) exit

        NbInputs = iEnd - i
        if (This%CheckPointFreq > 0) NbInputs = min(This%CheckPointFreq, iEnd-i)

        allocate(Input(NbInputs), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Input', ProcName=ProcName, stat=StatLoc)

        allocate(Outputs(NbOutputs,NbInputs), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='Outputs', ProcName=ProcName, stat=StatLoc)

        ii = 1
        do ii = 1, NbInputs
          call Input(ii)%Construct(Input=This%ParamSample(:,i+ii), Labels=Labels)
        end do

        if (.not. SilentLoc) then
          Line = '  Model run # ' // ConvertToString(Value=This%ModelRunCounter+1) 
          if(NbInputs > 1) Line = Line // '-' // ConvertToString(Value=This%ModelRunCounter+NbInputs)
          write(*,'(A)') Line
        end if
        
        call ModelInterface%Run(Input=Input, Output=Outputs, Stat=This%ParamSampleRan(i+1:i+NbInputs))

        iRun = 1
        do iRun = 1, NbInputs
          if (This%ParamSampleRan(i+iRun) /= 0) then
            ii = 1
            do ii = 1, NbOutputs
              call Outputs(ii,iRun)%Reset()
            end do
            cycle
          end if

          im1 = 0
          ii = 1
          do ii = 1, NbOutputs
            VarR2DPtr => Outputs(ii,iRun)%GetValuesPointer()
            if (Outputs(ii,iRun)%GetNbDegen() > 1) call Error%Raise('PCE procedure cant deal with stochastic ' //      &
                                                                                                'responses', ProcName=ProcName)
            iv = 0
            iii = im1 + 1
            do iii = im1+1, im1+size(VarR2DPtr,1)
              iv = iv + 1
              call This%Cells(iii)%AppendRecord(Entry=VarR2DPtr(iv,1))
            end do
            im1 = im1 + size(VarR2DPtr,1)
            nullify(VarR2DPtr)
            call Outputs(ii,iRun)%Reset()
          end do
        end do

        deallocate(Outputs, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Outputs', ProcName=ProcName, stat=StatLoc)

        deallocate(Input, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Input', ProcName=ProcName, stat=StatLoc)

        This%ModelRunCounter = This%ModelRunCounter + NbInputs
        i = i + NbInputs
        This%ParamSampleStep = i

        if (i /= iEnd) call RestartUtility%Update(Input=RestartInput, SectionChain=This%SectionChain)
  
      end do

      This%SamplesRan = .true.

      iStart = ParamRecordLength
      allocate(VarR2D(NbDim,count(This%ParamSampleRan==0)+ParamRecordLength), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      if (iStart > 0) VarR2D(:,1:ParamRecordLength) = This%ParamRecord

      i = iStart+1
      ii = 0
      do i = iStart+1, size(VarR2D,2)
        ii = ii + 1
        if (This%ParamSampleRan(ii) == 0) VarR2D(:,i) = This%ParamSample(:,ii)
      end do
      call move_alloc(VarR2D, This%ParamRecord)
      ParamRecordLength = size(This%ParamRecord,2)

      deallocate(This%ParamSample, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)
      deallocate(This%ParamSampleRan, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc)

      This%ParamSampleStep = 0

      if (.not. SilentLoc) write(*,*)

    end if
    
    iEnd = size(This%ParamRecord,2)

    call RestartUtility%Update(Input=RestartInput, SectionChain=This%SectionChain)

    !***************************************************************************************************************************
    ! Updating coefficients
    if (.not. This%SamplesAnalyzed) then

      if (.not. SilentLoc) then
        Line = 'Computing PCE coefficients for each node'
        write(*,'(A)') Line
      end if

      i = 1
      do i = 1, This%NbCells

        if (This%Cells(i)%GetCVError() <= This%StopError) cycle

        OverfitCounter = 0

        if (iStart == 0) then
          IndexOrder = IndexStartOrder
        else
          IndexOrder = This%Cells(i)%GetTruncationOrder()
          if (IndexOrder >= IndexMaxOrder) cycle
        end if

        CVErrorTrip = huge(VarR0D)

        do
          ! Generating Indices
          if (IndexOrder > IndexMaxOrder) exit
          call IndexSetPointer%GenerateIndices(Order=IndexOrder, TupleSize=NbDim, Indices=IndicesLoc)

          NbIndices = size(IndicesLoc,2)
          call This%Cells(i)%GetRecord(Values=Goal)

          ! Constructing design space
          allocate(DesignSpace(iEnd,NbIndices), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='DesignSpace', ProcName=ProcName, stat=StatLoc)
          M = size(DesignSpace,1)
          N = size(DesignSpace,2)

          ii = 1
          do ii = 1, M
              call Basis%Eval(X=This%ParamRecord(:,ii), Indices=IndicesLoc, Values=DesignSpace(ii,:))
          end do

          allocate(CoefficientsLoc(N), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

          call This%Solver%Solve(System=DesignSpace, Goal=Goal, Coefficients=CoefficientsLoc, CVError=CVError)  

          deallocate(DesignSpace, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='DesignSpace', ProcName=ProcName, stat=StatLoc)

          if (CVError < This%Cells(i)%GetCVError()) then
            call This%Cells(i)%SetModel(Coefficients=CoefficientsLoc, Indices=IndicesLoc, CVError=CVError, IndexOrder=IndexOrder)
          end if

          deallocate(CoefficientsLoc, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc)

          if (.not. SilentLoc) then
            Line = '  Node ' // ConvertToString(Value=i) // ' -- CVError = ' // ConvertToString(Value=CVError)
            if (This%Cells(i)%GetCVError() <= This%StopError) Line = Line // ' -- Converged'
            write(*,'(A)') Line
          end if

          if (CVError >= CVErrorTrip) then
            OverfitCounter = OverfitCounter + 1
            if (OverfitCounter >= This%MaxNumOverfit) exit
          else
            OverfitCounter = 0
            CVErrorTrip = CVError
          end if

          if (This%Cells(i)%GetCVError() <= This%StopError) exit

          IndexOrder = IndexOrder + 1

        end do

      end do

      This%SamplesAnalyzed = .true.

    end if

    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%SamplesAnalyzed = .false.

    call RestartUtility%Update(Input=RestartInput, SectionChain=This%SectionChain)

  end do

  This%SamplesObtained = .true.
  This%SamplesRan = .true.
  This%SamplesAnalyzed = .true.

  if (StepExceededFlag) then
    Line = 'Maximum sampling step exceeded'
    if (This%ModelRunCounter == 0) call Error%Raise(Line='Maximum sampling step exceeded prior to any samples being taken',  &
                                                                                                              ProcName=ProcName)
    write(*,'(A)') Line
  end if

  if (.not. ConvergedFlag) then
    if (.not. SilentLoc) then
      Line = 'Some nodes did not converge'
      write(*,'(A)') '' 
      write(*,'(A)') Line
    end if   
  end if

  if (allocated(Goal)) deallocate(Goal, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Goal', ProcName=ProcName, stat=StatLoc)

  if (present(OutputDirectory)) call This%WriteOutput(Directory=OutputDirectory, Responses=Responses)

  ! Collecting results to construct polynomial chaos model object
  allocate(CVErrors(NbOutputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='CVErrors', ProcName=ProcName, stat=StatLoc)
  allocate(Coefficients(NbOutputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
  allocate(Indices(NbOutputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Indices', ProcName=ProcName, stat=StatLoc)  

  i = 1
  do i = 1, NbOutputs
    iStart = 1
    if (i > 1) iStart = sum(NbCellsOutput(1:(i-1)))+1
    iEnd = sum(NbCellsOutput(1:i))
    ii = 1
    do ii = iStart, iEnd
      call CVErrors(i)%Append(Value=This%Cells(ii)%GetCVError())
      VarR1DPtr => This%Cells(ii)%GetCoefficientsPointer()
      call Coefficients(i)%Append(Values=VarR1DPtr)
      VarI2DPtr => This%Cells(ii)%GetIndicesPointer()
      call Indices(i)%Append(Values=VarI2DPtr)
      call This%Cells(ii)%Reset()
      nullify(VarI2DPtr)
      nullify(VarR1DPtr)
    end do
  end do

  This%ModelRunCounter = 0

  deallocate(This%ParamRecord, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc)

  deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)
  This%NbCells = 0

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
subroutine WriteOutput(This, Directory, Responses)

  class(PCERegression_Type), intent(inout)                            ::    This
  character(*), intent(in)                                            ::    Directory
  type(Response_Type), dimension(:), intent(in)                       ::    Responses

  character(*), parameter                                             ::    ProcName='WriteOutput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type)                                             ::    Input
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  logical                                                             ::    SilentLoc
  integer                                                             ::    NbOutputs
  type(SMUQFile_Type)                                                 ::    File
  integer                                                             ::    i, ii, iii
  character(:), allocatable                                           ::    Line
  integer                                                             ::    iStart
  integer                                                             ::    iEnd
  integer                                                             ::    NbCells
  real(rkp), allocatable, dimension(:)                                ::    VarR1D 
  integer, allocatable, dimension(:)                                  ::    VarI1D
  real(rkp), allocatable, dimension(:)                                ::    SobolIndices
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  real(rkp), pointer, dimension(:)                                    ::    VarR1DPtr=>null()
  integer, pointer, dimension(:,:)                                    ::    VarI2DPtr=>null()

  if (len_trim(Directory) /= 0) then

    call MakeDirectory(Path=Directory, Options='-p')

    SilentLoc = This%Silent

    if (.not. SilentLoc) then
      Line = 'Writing solver data to the output folder'
      write(*,'(A)') ''
      write(*,'(A)') Line
    end if

    PrefixLoc = Directory

    NbOutputs = size(Responses,1)

    FileName = 'nbresponses.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call File%Export(String=ConvertToString(Value=NbOutputs))

    FileName = 'sampled_parameters.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=This%ParamRecord, File=File)

    allocate(SobolIndices(size(This%ParamRecord,1)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='SobolIndices', ProcName=ProcName, stat=StatLoc)
    SobolIndices = Zero 

    iStart = 0

    i = 1
    do i = 1, NbOutputs

      call MakeDirectory(Path=Directory // '/' // Responses(i)%GetLabel() , Options='-p')

      NbCells = Responses(i)%GetNbNodes()
      iEnd = iStart + NbCells

      iii = 0
      ii = iStart+1
      do ii = iStart+1, iEnd
        iii = iii + 1
        DirectoryLoc = '/' // Responses(i)%GetLabel() // '/cell' // ConvertToString(Value=iii) // '/'
        call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

        FileName = DirectoryLoc // 'cverror.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call File%Export(String=ConvertToString(Value=This%Cells(ii)%GetCVError()))

        VarR1DPtr => This%Cells(ii)%GetCoefficientsPointer()
        FileName = DirectoryLoc // 'coefficients.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarR1DPtr, File=File)
        nullify(VarR1DPtr)

        VarI2DPtr => This%Cells(ii)%GetIndicesPointer()
        FileName = DirectoryLoc // 'indices.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarI2DPtr, File=File)
        nullify(VarI2DPtr)

        call This%Cells(ii)%GetRecord(Values=VarR1D)
        FileName = DirectoryLoc // 'sampled_output.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarR1D, File=File)

        call This%Cells(ii)%GetCVErrorHistory(Values=VarR1D)
        FileName = DirectoryLoc // 'cverror_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarR1D, File=File)

        call This%Cells(ii)%GetOrderHistory(Values=VarI1D)
        FileName = DirectoryLoc // 'order_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarI1D, File=File)

        call This%Cells(ii)%GetNbRunsHistory(Values=VarI1D)
        FileName = DirectoryLoc // 'nb_runs_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarI1D, File=File)

        call This%Cells(ii)%GetCardinalityHistory(Values=VarI1D)
        FileName = DirectoryLoc // 'cardinality_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarI1D, File=File)

        call This%Cells(ii)%GetSobolIndicesHistory(Values=VarR2D)
        FileName = DirectoryLoc // 'sobol_indices_history.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=VarR2D, File=File)

        call This%Cells(ii)%GetSobolIndices(Values=SobolIndices)
        FileName = DirectoryLoc // 'sobol_indices.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Array=SobolIndices, File=File)

      end do

      iStart = iEnd

    end do

    deallocate(SobolIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='SobolIndices', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(PCERegression_Type), intent(out)                              ::    LHS
  class(PCEMethod_Type), intent(in)                                   ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (PCERegression_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%Sampler, source=RHS%Sampler, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Sampler', ProcName=ProcName, stat=StatLoc)
        LHS%SampleEnrichScheme = RHS%SampleEnrichScheme
        LHS%NbSamples = RHS%NbSamples
        LHS%Solver = RHS%Solver
        LHS%Silent = RHS%Silent
        LHS%StopError = RHS%StopError
        LHS%ModelRunCounter = RHS%ModelRunCounter
        LHS%MaxNumOverfit = RHS%MaxNumOverfit
        LHS%CheckpointFreq = RHS%CheckpointFreq
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(PCERegression_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamRecord)) deallocate(This%ParamRecord, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc)
  
  if (allocated(This%ParamSample)) deallocate(This%ParamSample, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSample', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamSampleRan)) deallocate(This%ParamSampleRan, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sampler)) deallocate(This%Sampler, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)

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

  if (allocated(This%Coefficients)) deallocate(This%Coefficients, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Coefficients', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Indices)) deallocate(This%Indices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%SobolIndices)) deallocate(This%SobolIndices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SobolIndices', ProcName=ProcName, stat=StatLoc)

  call This%OutputRecord%Purge()
  call This%NbRunsHistory%Purge()
  call This%OrderHistory%Purge()
  call This%CardinalityHistory%Purge()
  call This%CVErrorHistory%Purge()
  call This%SobolIndicesHistory%Purge()

  This%CVError = huge(One)
  This%IndexOrder = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_Cell(This, Input, Prefix)

  use ArrayRoutines_Module

  class(Cell_Type), intent(inout)                                     ::    This
  class(InputSection_Type), intent(in)                                ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_Cell'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    VarC0D
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer                                                             ::    VarI0D
  integer, allocatable, dimension(:)                                  ::    VarI1D
  integer, allocatable, dimension(:,:)                                ::    VarI2D
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

  ParameterName = 'cv_error'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call input%GetValue(Value=VarR0D, ParameterName=Parametername, Mandatory=.true.)
  This%CVError = VarR0D

  ParameterName = 'index_order'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call input%GetValue(Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
  if (Found) This%IndexOrder = VarI0D

  SectionName = 'sobol_indices'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
  nullify(InputSection)
  allocate(This%SobolIndices, source=VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%', ProcName=ProcName, stat=StatLoc)
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'output'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%OutputRecord%Append(Values=VarR1D)
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'coefficients'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
  nullify(InputSection)
  allocate(This%Coefficients, source=VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%', ProcName=ProcName, stat=StatLoc)
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'indices'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarI2D, Prefix=PrefixLoc)
  nullify(InputSection)
  allocate(This%Indices, source=VarI2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)
  deallocate(VarI2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI2D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'cverror_history'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR1D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%CVErrorHistory%Append(Values=VarR1D)
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'order_history'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarI1D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%CVErrorHistory%Append(Values=VarI1D)
  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'nb_runs_history'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarI1D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%NbRunsHistory%Append(Values=VarI1D)
  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'cardinality_history'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarI1D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%CardinalityHistory%Append(Values=VarI1D)
  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'sobol_indices_history'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
  nullify(InputSection)
  call This%SobolIndicesHistory%Append(Values=VarR2D)
  deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1_Cell(This)

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='ConstructCase1_Cell'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call This%Reset()

  allocate(This%Coefficients(1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Coefficients', ProcName=ProcName, stat=StatLoc)
  This%Coefficients = 0

  allocate(This%Indices(1,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

  allocate(This%SobolIndices(1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%SobolIndices', ProcName=ProcName, stat=StatLoc)

  This%Indices = 0
  This%SobolIndices = Zero
  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput_Cell(This, Name, Prefix, Directory)

  use CommandRoutines_Module
  use ArrayRoutines_Module
  use StringConversion_Module
  use SMUQFile_Class                                            ,only:    SMUQFile_Type

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

  call GetInput_Cell%AddParameter(Name='cv_error', Value=ConvertToString(Value=This%CVError))

  call GetInput_Cell%AddParameter(Name='index_order', Value=ConvertToString(Value=This%IndexOrder))

  if (ExternalFlag) then
    SectionName = 'sobol_indices'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'sobol_indices.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%SobolIndices, File=File)
    nullify(InputSection)

    SectionName = 'output'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'output.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call This%OutputRecord%Get(Values=VarR1D)
    call ExportArray(Input=InputSection, Array=VarR1D, File=File)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)

    SectionName = 'coefficients'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'coefficients.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%Coefficients, File=File)
    nullify(InputSection)

    SectionName = 'indices'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // 'indices.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%Indices, File=File)
    nullify(InputSection)

    if (This%CVError < huge(One)) then
      SectionName = 'cverror_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'cverror_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%CVErrorHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D, File=File)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'order_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'order_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%OrderHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D, File=File)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'nb_runs_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'nb_runs_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%NbRunsHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D, File=File)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'cardinality_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'cardinality_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%CardinalityHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D, File=File)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'sobol_indices_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      FileName = DirectoryLoc // 'sobol_indices_history.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call This%SobolIndicesHistory%Get(Values=VarR2D)
      call ExportArray(Input=InputSection, Array=VarR2D, File=File)
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

  else
    SectionName = 'sobol_indices'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    allocate(VarR1D, source=This%SobolIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    call ExportArray(Input=InputSection, Array=VarR1D)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)

    SectionName = 'output'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%OutputRecord%Get(Values=VarR1D)
    call ExportArray(Input=InputSection, Array=VarR1D)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)

    SectionName = 'coefficients'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ExportArray(Input=InputSection, Array=This%Coefficients)
    nullify(InputSection)

    SectionName = 'indices'
    call GetInput_Cell%AddSection(SectionName=SectionName)
    call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ExportArray(Input=InputSection, Array=This%Indices)
    nullify(InputSection)

    if (This%CVError < huge(One)) then
      SectionName = 'cverror_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%CVErrorHistory%Get(Values=VarR1D)
      call ExportArray(Input=InputSection, Array=VarR1D)
      deallocate(VarR1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'order_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%OrderHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'nb_runs_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%NbRunsHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'cardinality_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%CardinalityHistory%Get(Values=VarI1D)
      call ExportArray(Input=InputSection, Array=VarI1D)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)

      SectionName = 'sobol_indices_history'
      call GetInput_Cell%AddSection(SectionName=SectionName)
      call GetInput_Cell%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%SobolIndicesHistory%Get(Values=VarR2D)
      call ExportArray(Input=InputSection, Array=VarR2D)
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      nullify(InputSection)
    end if

  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine AppendRecordR0D_Cell(This, Entry)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), intent(in)                                               ::    Entry

  character(*), parameter                                             ::    ProcName='AppendRecordR0D_Cell'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%OutputRecord%Append(Value=Entry)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine AppendRecordR1D_Cell(This, Entries)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Entries

  character(*), parameter                                             ::    ProcName='AppendRecordR1D_Cell'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%OutputRecord%Append(Values=Entries)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SetModel_Cell(This, Coefficients, Indices, CVError, IndexOrder)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Coefficients
  integer, dimension(:,:), intent(in)                                 ::    Indices
  real(rkp), intent(in)                                               ::    CVError
  integer, intent(in)                                                 ::    IndexOrder

  character(*), parameter                                             ::    ProcName='SetModel_Cell'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    VarI0D  
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    NbNonZero

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Coefficients,1) /= size(Indices,2)) call Error%Raise('Incompatible indices and coefficients arrays', &
                                                                ProcName=ProcName)

  This%IndexOrder = IndexOrder
  This%CVError = CVError

  if (allocated(This%Coefficients)) deallocate(This%Coefficients, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Coefficients', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Indices)) deallocate(This%Indices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

  NbNonZero = count(dabs(Coefficients) > Zero)

  allocate(This%Coefficients(NbNonZero), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Coefficients', ProcName=ProcName, stat=StatLoc)
  This%Coefficients = Zero

  allocate(This%Indices(size(Indices,1), NbNonZero), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)
  This%Indices = 0

  if (allocated(This%SobolIndices)) then
    if (size(This%SobolIndices,1) /= size(This%Indices,1)) then
      deallocate(This%SobolIndices, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='This%SobolIndices', ProcName=ProcName, stat=StatLoc)
    end if
  end if

  if (.not. allocated(This%SobolIndices)) then
    allocate(This%SobolIndices(size(This%Indices,1)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%SobolIndices', ProcName=ProcName, stat=StatLoc)
    This%SobolIndices = Zero
  end if

  i = 1
  ii = 0
  do i = 1, size(Coefficients,1)
    if (.not. dabs(Coefficients(i)) > Zero) cycle
    ii = ii + 1
    This%Coefficients(ii) = Coefficients(i)
    This%Indices(:,ii) = Indices(:,i)
  end do

  if (ii /= NbNonZero) call Error%Raise('Something went wrong when filtering out zero coefficients', ProcName=ProcName)

  VarI0D = This%OutputRecord%GetLength()
  call This%NbRunsHistory%Append(Value=VarI0D)

  call This%OrderHistory%Append(Value=IndexOrder)

  VarI0D = size(This%Indices,2)
  call This%CardinalityHistory%Append(Value=VarI0D)

  call This%CVErrorHistory%Append(Value=CVError)

  call This%OutputRecord%Get(Values=VarR1D)
  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  call ComputeSobolIndices(Coefficients=This%Coefficients, Indices=This%Indices, SobolIndices=This%SobolIndices)

  call This%SobolIndicesHistory%Append(Values=This%SobolIndices)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetRecord_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values 

  character(*), parameter                                             ::    ProcName='GetRecord_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
  if (This%OutputRecord%GetLength() < 1) call Error%Raise(Line='Outputs not yet supplied', ProcName=ProcName)

  call This%OutputRecord%Get(Values=Values)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetCoeffsPointer_Cell(This)

  real(rkp), dimension(:), pointer                                    ::    GetCoeffsPointer_Cell

  class(Cell_Type), target, intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='GetCoeffsPointer_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
  if (.not. allocated(This%Coefficients)) call Error%Raise(Line='Coefficients not yet supplied', ProcName=ProcName)

  GetCoeffsPointer_Cell => This%Coefficients

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetIndicesPointer_Cell(This)

  integer, dimension(:,:), pointer                                    ::    GetIndicesPointer_Cell

  class(Cell_Type), target, intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='GetIndicesPointer_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
  if (.not. allocated(This%Indices)) call Error%Raise(Line='Indices not yet supplied', ProcName=ProcName)

  GetIndicesPointer_Cell => This%Indices

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetCVError_Cell(This)

  real(rkp)                                                           ::    GetCVError_Cell

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='GetCVError_Cell'
  integer                                                             ::    StatLoc=0  

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetCVError_Cell = This%CVError

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetTruncationOrder_Cell(This)

  integer                                                             ::    GetTruncationOrder_Cell

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='GetTruncationOrder_Cell'
  integer                                                             ::    StatLoc=0  

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetTruncationOrder_Cell = This%IndexOrder

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetSobolIndices_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), dimension(:), intent(inout)                              ::    Values 

  character(*), parameter                                             ::    ProcName='GetSobolIndices_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(values,1) /= size(This%SobolIndices)) call Error%Raise('Incompatible values array', ProcName=ProcName)

  Values = This%SobolIndices

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCVErrorHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Values

  character(*), parameter                                             ::    ProcName='GetCVErrorHistory_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%CVErrorHistory%Get(Values=Values)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetNbRunsHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  integer, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetNbRunsHistory_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%NbRunsHistory%Get(Values=Values)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetOrderHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  integer, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetOrderHistory_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%OrderHistory%Get(Values=Values)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCardinalityHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  integer, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='GetCardinalityHistory_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%CardinalityHistory%Get(Values=Values)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetSobolIndicesHistory_Cell(This, Values)

  class(Cell_Type), intent(inout)                                     ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Values

  character(*), parameter                                             ::    ProcName='GetSobolIndicesHistory_Cell'
  integer                                                             ::    StatLoc=0    

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  call This%SobolIndicesHistory%Get(Values=Values)

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
    LHS%IndexOrder = RHS%IndexOrder
    LHS%OutputRecord = RHS%OutputRecord
    LHS%CVError = RHS%CVError
    if (allocated(RHS%Coefficients)) allocate(LHS%Coefficients, source=RHS%Coefficients, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Coefficients', ProcName=ProcName, stat=StatLoc)
    if (allocated(RHS%Indices)) allocate(LHS%Indices, source=RHS%Indices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Indices', ProcName=ProcName, stat=StatLoc)
    LHS%CVErrorHistory = RHS%CVErrorHistory
    LHS%OrderHistory = RHS%OrderHistory
    LHS%NbRunsHistory = RHS%NbRunsHistory
    LHS%CardinalityHistory = RHS%CardinalityHistory
    LHS%SobolIndicesHistory = RHS%SobolIndicesHistory
    allocate(LHS%SobolIndices, source=RHS%SobolIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%SobolIndices', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_Cell(This)

  type(Cell_Type), intent(inout)                                      ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_Cell'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Coefficients)) deallocate(This%Coefficients, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Coefficients', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Indices)) deallocate(This%Indices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Indices', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%SobolIndices)) deallocate(This%SobolIndices, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SobolIndices', ProcName=ProcName, stat=StatLoc)

  call This%OutputRecord%Purge()
  call This%NbRunsHistory%Purge()
  call This%OrderHistory%Purge()
  call This%CardinalityHistory%Purge()
  call This%CVErrorHistory%Purge()
  call This%SobolIndicesHistory%Purge()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------
  
  end module
  