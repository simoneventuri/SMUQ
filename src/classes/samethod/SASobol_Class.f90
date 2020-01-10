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
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SAMethod_Class                                                ,only:    SAMethod_Type
use InputDet_Class                                                ,only:    InputDet_Type
use Output_Class                                                  ,only:    Output_Type
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Model_Class                                                   ,only:    Model_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use OnlineVarEstimator_Class                                      ,only:    OnlineVarEstimator_Type

implicit none

private

public                                                                ::    SASobol_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized
  logical                                                             ::    Constructed
  type(OnlineVarEstimator_Type)                                       ::    MomentEstimator
  real(rkp), allocatable, dimension(:)                                ::    StEstimator
  integer, allocatable, dimension(:)                                  ::    StEstimatorNbSamples
  type(LinkedList1D_Type)                                             ::    StHistory
  type(LinkedList0D_Type)                                             ::    MeanHistory
  type(LinkedList0D_Type)                                             ::    VarianceHistory
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
  type(SpaceSampler_Type)                                             ::    Sampler
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  integer                                                             ::    ModelRunCounter
  real(rkp), allocatable, dimension(:,:)                              ::    ParamRecord
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  logical, allocatable, dimension(:)                                  ::    ParamSampleRan
  integer                                                             ::    ParamSampleStep
  real(rkp)                                                           ::    AbsTolerance
  real(rkp)                                                           ::    RelTolerance
  integer                                                             ::    HistoryFreq
  type(LinkedList0D_Type)                                             ::    HistoryStep
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
  subroutine Initialize( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'SASobol'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%ParamRecord) ) deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    This%NbCells = 0

    deallocate(This%ParamSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

    call This%HistoryStep%Purge()

    call This%Sampler%Reset()

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%CheckpointFreq = -1
    This%HistoryFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%ParamSampleStep = 0
    This%RelTolerance = 1.d-3
    This%AbsTolerance = 1.d-5
    This%ModelRunCounter = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix )

    class(SASobol_Type), intent(inout)                                ::    This
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
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    logical, allocatable, dimension(:)                                ::    VarL1D

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName= 'silent'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Silent=VarL0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    ParameterName = "history_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%HistoryFreq = VarI0D

    ParameterName = "absolute_tolerance"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%AbsTolerance = VarR0D

    ParameterName = "relative_tolerance"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%RelTolerance = VarR0D

    SectionName = 'sampler'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Sampler%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'restart'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'model_run_counter'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%ModelRunCounter = VarI0D

      ParameterName = 'param_sample_step'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%ParamSampleStep = VarI0D

      ParameterName = 'samples_obtained'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesObtained= VarL0D

      ParameterName = 'samples_ran'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesRan = VarL0D

      SubSectionName = SectionName // '>param_record'
      if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
        nullify( InputSection )
        This%ParamRecord = VarR2D
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      end if

      SubSectionName = SectionName // '>param_sample'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
      nullify( InputSection )
      This%ParamSample = VarR2D
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      SubSectionName = SectionName // '>param_sample_ran'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarL1D, Prefix=PrefixLoc )
      nullify( InputSection )
      This%ParamSampleRan = VarL1D
      deallocate(VarL1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      This%NbCells = 0
      SubSectionName = SectionName // '>cells'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      This%NbCells = InputSection%GetNumberofSubSections()
      nullify(InputSection)
      allocate(This%Cells(This%NbCells), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, This%NbCells
        SubSectionName = SectionName // '>cells>cell' // ConvertToString(Value=i)
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>mean', Mandatory=.true. )
        call This%Cells(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      end do

      SubSectionName = SectionName // '>history_step'
      if ( Input%HasSection(SubSectionName=SubSectionName) )then
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
        nullify( InputSection )
        call This%HistoryStep%Append(Values=VarR1D)
        deallocate(VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(SASobol_Type), intent(inout)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
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

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='silent', Value=ConvertToString(Value=This%Silent ) )
    call GetInput%AddParameter( Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq ) )
    call GetInput%AddParameter( Name='history_frequency', Value=ConvertToString(Value=This%HistoryFreq ) )
    call GetInput%AddParameter( Name='absolute_tolerance', Value=ConvertToString(Value=This%AbsTolerance ) )
    call GetInput%AddParameter( Name='relative_tolerance', Value=ConvertToString(Value=This%RelTolerance ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
    SectionName = 'sampler'
    call GetInput%AddSection( Section=This%Sampler%GetInput( MainSectionName=SectionName,                                         &
                                                                                        Prefix=PrefixLoc,Directory=DirectorySub) )


    if ( This%ModelRunCounter > 0 ) then
      SectionName = 'restart'
      call GetInput%AddSection( SectionName=SectionName )

      if ( ExternalFlag ) then
        if ( allocated(This%ParamRecord) ) then
          SubSectionName = 'param_record'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/param_record.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%ParamRecord, File=File )
          nullify(InputSection)
        end if
        
        if ( allocated(This%ParamSample) ) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/param_sample.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%ParamSample, File=File )
          nullify(InputSection)

          SubSectionName = 'param_sample_ran'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/param_sample_ran.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%ParamSample, File=File )
          nullify(InputSection)
        end if

        if ( This%HistoryFreq > 0 ) then
          call This%HistoryStep%Get( Values=VarI1D )
          SubSectionName = 'history_step'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/' // SubSectionName // '.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=VarI1D, File=File )
          nullify(InputSection)
          deallocate(VarI1D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
        end if

      else
        if ( allocated(This%ParamSample) ) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=This%ParamSample )
          nullify(InputSection)

          SubSectionName = 'param_sample_ran'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=This%ParamSampleRan )
          nullify(InputSection)
        end if

        if ( This%HistoryFreq > 0 ) then
          call This%HistoryStep%Get( Values=VarI1D )
          SubSectionName = 'history_step'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=VarI1D )
          nullify(InputSection)
          deallocate(VarI1D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
        end if

      end if

      call GetInput%AddParameter( Name='samples_obtained', Value=ConvertToString(Value=This%SamplesObtained ),                    &
                                                                                                         SectionName=SectionName )
      call GetInput%AddParameter( Name='samples_ran', Value=ConvertToString(Value=This%SamplesRan ), SectionName=SectionName )
      call GetInput%AddParameter( Name='model_run_counter', Value=ConvertToString(Value=This%ModelRunCounter ),                   &
                                                                                                         SectionName=SectionName )
      call GetInput%AddParameter( Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                    &
                                                                                                         SectionName=SectionName )

      SubSectionName = 'cells'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      i = 1
      do i = 1, This%NbCells
        if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cell' // ConvertToString(Value=i)
        call GetInput%AddSection( Section=This%Cells(i)%GetInput( MainSectionName='cell' // ConvertToString(Value=i),             &
                                  Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName // '>' // SubSectionName )
      end do

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, SampleSpace, Responses, Model, OutputDirectory )

    class(SASobol_Type), intent(inout)                                ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory

    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    type(ModelInterface_Type)                                         ::    ModelInterface
    logical                                                           ::    StepExceededFlag=.false.
    logical                                                           ::    SilentLoc
    integer                                                           ::    NbDim
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), pointer, dimension(:,:)                                ::    VarR2DPtr=>null()
    type(ParamSpace_Type)                                             ::    ExtendedSampleSpace
    type(InputDet_Type)                                               ::    Input
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    v
    integer                                                           ::    vi
    integer                                                           ::    vii
    real(rkp), allocatable, dimension(:)                              ::    ParamSubSample
    type(Output_Type), allocatable, dimension(:)                      ::    Outputs
    real(rkp), allocatable, dimension(:,:)                            ::    SubSampleOutput
    logical, allocatable, dimension(:)                                ::    SubSampleRan
    integer                                                           ::    NbResponses
    integer, allocatable, dimension(:)                                ::    NbCellsOutput
    integer                                                           ::    iStart
    integer                                                           ::    iEnd
    integer                                                           ::    ParamRecordLength
    real(rkp), allocatable, dimension(:)                              ::    Delta
    character(:), allocatable                                         ::    Line

    call ModelInterface%Construct( Model=Model, Responses=Responses )

    call ExtendedSampleSpace%Construct( SampleSpace1=SampleSpace, SampleSpace2=SampleSpace )

    NbResponses = size(Responses,1)
    NbDim = SampleSpace%GetNbDim()
    SilentLoc = This%Silent

    allocate(Delta(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Delta', ProcName=ProcName, stat=StatLoc )
    Delta = Zero

    allocate(NbCellsOutput(NbResponses), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0
    i = 1
    do i = 1, NbResponses
      NbCellsOutput(i) = Responses(i)%GetNbNodes()
      This%NbCells = This%NbCells + NbCellsOutput(i)
    end do

    if ( .not. allocated(This%Cells) ) then
      allocate(This%Cells(This%NbCells), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, This%NbCells
        call This%Cells(i)%Construct( Dimensionality=NbDim )
      end do 
    end if

    SilentLoc = This%Silent

    allocate(ParamSubSample(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ParamSubSampleStep', ProcName=ProcName, stat=StatLoc )
    ParamSubSample = Zero

    allocate(SubSampleOutput(NbDim+1,This%NbCells), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SubSampleOutputA', ProcName=ProcName, stat=StatLoc )
    SubSampleOutput = Zero

    allocate(SubSampleRan(NbDim+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SubSampleStatLoc', ProcName=ProcName, stat=StatLoc )
    SubSampleRan = .false.

    StepExceededFlag = .false.

    ParamRecordLength = 0
    if ( allocated(This%ParamRecord) ) ParamRecordLength = size(This%ParamRecord,2)

    if ( This%HistoryFreq > 0 .and. This%ModelRunCounter == 0 ) then
      call This%HistoryStep%Append( Value=0 )
      ii = 1
      do ii = 1, This%NbCells
        call This%Cells(ii)%UpdateHistory()
      end do
    end if

    do

      !***************************************************************************************************************************
      ! Obtaining samples
      if ( .not. This%SamplesObtained ) then

        This%ParamSampleStep = 0

        if ( .not. SilentLoc ) then
          if ( This%ModelRunCounter /= 0 ) then
            Line = 'Performing enrichment'
          else
            Line = 'Initial population of the linear system'
          end if
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if

        if ( This%ModelRunCounter == 0 ) then
          This%ParamSample = This%Sampler%Draw(SampleSpace=ExtendedSampleSpace)
        else
          call This%Sampler%Enrich( SampleSpace=ExtendedSampleSpace, Samples=This%ParamRecord, EnrichmentSamples=This%ParamSample,&
                                                                                                        Exceeded=StepExceededFlag)
          if ( StepExceededFlag ) exit
        end if

        allocate(This%ParamSampleRan(size(This%ParamSample,2)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )
        This%ParamSampleRan = .false.
        This%SamplesObtained = .true.
      end if

      iEnd = size(This%ParamSample,2)

      !***************************************************************************************************************************
      ! Running samples
      if ( .not. This%SamplesRan ) then
        i = This%ParamSampleStep
        do
          i = i + 1
          if ( i > iEnd ) exit
          This%ParamSampleStep = i
          SubSampleRan = .false.

          if ( .not. SilentLoc ) then
            Line = 'Running SubSamples of Sample #' // ConvertToString(Value=ParamRecordLength+i)
            write(*,'(A)') Line
          end if

          ii = 1
          do ii = 1, NbDim+1
            ParamSubSample = This%ParamSample(1:NbDim,i)
            if ( ii > 1 ) ParamSubSample(ii-1) = This%ParamSample(NbDim+ii-1,i)

            This%ModelRunCounter = This%ModelRunCounter + 1
            if ( .not. SilentLoc ) then
              Line = '  Model Run #' // ConvertToString(Value=This%ModelRunCounter) // ', SubSample #' //ConvertToString(Value=ii)
              write(*,'(A)') Line
            end if

            call Input%Construct( Input=ParamSubSample, Labels=SampleSpace%GetLabel() )
            call ModelInterface%Run( Input=Input, Output=Outputs, Stat=StatLoc )

            if ( StatLoc /= 0 ) then
              if ( .not. SilentLoc ) then
                Line = '  Model Run #' // ConvertToString(Value=This%ModelRunCounter) // ' -- Failed'
                write(*,'(A)') Line
              end if
              StatLoc = 0
              if ( allocated(Outputs) ) deallocate(Outputs, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )
              if ( ii == 1 ) exit
            else
              SubSampleRan(ii) = .true.
              iv = 1
              v = 0
              do iii = 1, NbResponses
                v = v + NbCellsOutput(iii)
                VarR2DPtr => Outputs(iii)%GetValuesPointer()
                SubSampleOutput(ii,iv:v) = VarR2DPtr(:,1)
                iv = v + 1
                nullify(VarR2DPtr)
              end do
            end if
          end do

          if ( SubSampleRan(1) ) then
            This%ParamSampleRan(i) = .true.
            ii = 1
            do ii = 1, This%NbCells
              call This%Cells(ii)%UpdateEstimators( OutputA=SubSampleOutput(1,ii) , OutputAB=SubSampleOutput(2:,ii),              &
                                                                                             ABRan=SubSampleRan(2:), Delta=Delta )
            end do
          end if

          if ( This%CheckpointFreq > 0 .and. (mod(ParamRecordLength+i, abs(This%CheckpointFreq)) == 0 .and. i /= iEnd)  ) then
            call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),     &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
          end if

          if ( This%HistoryFreq > 0 .and. (mod(ParamRecordLength+count(This%ParamSampleRan(1:i)), abs(This%HistoryFreq)) == 0     &
                                                                                                          .and. i /= iEnd)  ) then
            call This%HistoryStep%Append( Value=count(This%ParamSampleRan(1:i))+ParamRecordLength )
            ii = 1
            do ii = 1, This%NbCells
              call This%Cells(ii)%UpdateHistory()
            end do
          end if
    
        end do

        This%SamplesRan = .true.

        iStart = ParamRecordLength
        allocate(VarR2D(2*NbDim,count(This%ParamSampleRan)+ParamRecordLength), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        if ( iStart > 0 ) VarR2D(:,1:ParamRecordLength) = This%ParamRecord

        i = iStart+1
        ii = 0
        do i = iStart+1, size(VarR2D,2)
          ii = ii + 1
          if ( This%ParamSampleRan(ii) ) VarR2D(:,i) = This%ParamSample(:,ii)
        end do

        call move_alloc(VarR2D, This%ParamRecord)

        ParamRecordLength = size(This%ParamRecord,2)

        deallocate(This%ParamSampleRan, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

        deallocate(This%ParamSample, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

        This%ParamSampleStep = 0

      end if

      This%SamplesObtained = .false.
      This%SamplesRan = .false.

      if ( This%CheckpointFreq > 0 ) then
        call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),         &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
      end if

      if ( This%HistoryFreq > 0 ) then
        call This%HistoryStep%Append( Value=ParamRecordLength )
        ii = 1
        do ii = 1, This%NbCells
          call This%Cells(ii)%UpdateHistory()
        end do
      end if

    end do

    if ( StepExceededFlag ) then
      Line = 'Maximum sampling step exceeded'
      if ( This%ModelRunCounter == 0 ) call Error%Raise( Line='Maximum sampling step exceeded prior to any samples being taken',  &
                                                                                                               ProcName=ProcName )
      write(*,'(A)') ''  
      write(*,'(A)') Line
    end if

    if ( present(OutputDirectory) ) call This%WriteOutput( Directory=OutputDirectory, Responses=Responses )

    deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )

    This%ModelRunCounter = 0

    deallocate(ParamSubSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamSubSample', ProcName=ProcName, stat=StatLoc )

    deallocate(Outputs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )

    deallocate(SubSampleOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SubSampleOutput', ProcName=ProcName, stat=StatLoc )

    deallocate(SubSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SubSampleRan', ProcName=ProcName, stat=StatLoc )

    deallocate(NbCellsOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, Responses )

    class(SASobol_Type), intent(inout)                                ::    This
    character(*), intent(in)                                          ::    Directory
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
    character(:), allocatable                                         ::    ResponseLabel

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      if ( .not. SilentLoc ) then
        Line = 'Writing solver data to the output folder'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

      PrefixLoc = Directory

      NbResponses = size(Responses)

      FileName = '/sampled_parameters.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%ParamRecord, File=File )

      i = 1
      ii = 1
      iii = 0
      do i = 1, NbResponses

        ResponseLabel = Responses(i)%GetLabel()

        call MakeDirectory( Path=Directory // '/' // ResponseLabel, Options='-p' )

        FileName = '/' // ResponseLabel // '/coordinates.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=Responses(i)%GetCoordinatesPointer(), File=File, RowMajor=.true. )

        iii = iii + Responses(i)%GetNbNodes()
        v = 0
        do iv = ii, iii
          v = v + 1

          call MakeDirectory( Path=Directory // '/' // ResponseLabel // '/cell' // ConvertToString(Value=v), Options='-p' )

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mean.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export(String=ConvertToString(Value=This%Cells(iv)%GetMean()))

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/variance.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export(String=ConvertToString(Value=This%Cells(iv)%GetVariance()))

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sobol_total.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=This%Cells(iv)%GetSt(), File=File )

          if ( This%HistoryFreq > 0 ) then
            call This%HistoryStep%Get( Values=VarI1D )
            FileName = '/' // ResponseLabel // '/history_step.dat'
            call This%HistoryStep%Get( Values=VarI1D )
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
            call ExportArray( Array=VarI1D, File=File, RowMajor=.true. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mean_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' ) 
            call ExportArray( Array=This%Cells(iv)%GetMeanHistory(), File=File, RowMajor=.true. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/variance_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
            call ExportArray( Array=This%Cells(iv)%GetVarianceHistory(), File=File, RowMajor=.true. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sobol_total_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
            call ExportArray( Array=This%Cells(iv)%GetStHistory(), File=File, RowMajor=.false. )

          end if

        end do

      end do

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SASobol_Type), intent(out)                                  ::    LHS
    class(SAMethod_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (SASobol_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Silent = RHS%Silent
          LHS%Sampler = RHS%Sampler
          LHS%CheckPointFreq = RHS%CheckPointFreq
          LHS%HistoryFreq = RHS%HistoryFreq
          LHS%RelTolerance = RHS%RelTolerance
          LHS%AbsTolerance = RHS%AbsTolerance
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SASobol_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamRecord) ) deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
    
    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamSampleRan) ) deallocate(This%ParamSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Initialize_Cell'

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Reset_Cell'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%StEstimator) ) deallocate(This%StEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StEstimatorNbSamples) ) deallocate(This%StEstimatorNbSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc )

    call This%StHistory%Purge()
    call This%MeanHistory%Purge()
    call This%VarianceHistory%Purge
    
    call This%MomentEstimator%Reset()

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_Cell'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell( This, Input, Prefix )

    class(Cell_Type), intent(inout)                                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:)                                ::    VarI1D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'moment_estimator'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%MomentEstimator%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'sobol_total_estimator'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
    nullify( InputSection )
    call move_alloc( VarR1D, This%StEstimator )

    SectionName = 'sobol_estimator_nb_samples'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarI1D, Prefix=PrefixLoc )
    nullify( InputSection )
    call move_alloc( VarI1D, This%StEstimatorNbSamples )

    SectionName = 'sobol_total_history'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
    nullify( InputSection )
    call This%StHistory%Append( Values=VarR2D )
    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'mean_history'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
    nullify( InputSection )
    call This%MeanHistory%Append( Values=VarR1D )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'variance_history'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
    nullify( InputSection )
    call This%VarianceHistory%Append( Values=VarR1D )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell( This, Dimensionality )

    class(Cell_Type), intent(inout)                                   ::    This
    integer, intent(in)                                               ::    Dimensionality

    character(*), parameter                                           ::    ProcName='ConstructCase1_Cell'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%StEstimator(Dimensionality), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%StEstimator', ProcName=ProcName, stat=StatLoc )
    This%StEstimator = Zero

    allocate(This%StEstimatorNbSamples(Dimensionality), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc )
    This%StEstimatorNbSamples = Zero

    call This%MomentEstimator%Construct( SampleVariance=.true. )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory )

    use CommandRoutines_Module
    use ArrayRoutines_Module
    use StringRoutines_Module
    use SMUQFile_Class                                            ,only:    SMUQFile_Type

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    character(*), intent(in)                                          ::    MainSectionName
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
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(SMUQFile_Type)                                               ::    File

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/moment_estimator'
    SectionName = 'moment_estimator'
    call GetInput_Cell%AddSection( Section=This%MomentEstimator%GetInput( MainSectionName=SectionName,                            &
                                                                                       Prefix=PrefixLoc, Directory=DirectorySub) )

    if ( ExternalFlag ) then
      SectionName = 'sobol_total_estimator'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/sobol_total_estimator.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%StEstimator, File=File )
      nullify(InputSection)

      SectionName = 'sobol_estimator_nb_samples'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/sobol_estimator_nb_samples.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%StEstimatorNbSamples, File=File )
      nullify(InputSection)

      SectionName = 'sobol_total_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/sobol_total_history.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call This%StHistory%Get( Values=VarR2D )
      call ExportArray( Input=InputSection, Array=VarR2D, File=File )
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'mean_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/mean_history.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call This%MeanHistory%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D, File=File )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'variance_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/variance_history.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call This%VarianceHistory%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D, File=File )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)
    else
      SectionName = 'sobol_total_estimator'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      allocate(VarR1D, source=This%StEstimator, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      call ExportArray( Input=InputSection, Array=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'sobol_estimator_nb_samples'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      allocate(VarI1D, source=This%StEstimatorNbSamples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
      call ExportArray( Input=InputSection, Array=VarI1D )
      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'sobol_total_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%StHistory%Get( Values=VarR2D )
      call ExportArray( Input=InputSection, Array=VarR2D )
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'mean_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%MeanHistory%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'variance_history'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%VarianceHistory%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateEstimators_Cell( This, OutputA, OutputAB, ABRan, Delta )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), intent(in)                                             ::    OutputA
    real(rkp), dimension(:), intent(in)                               ::    OutputAB
    logical, dimension(:), intent(in)                                 ::    ABRan
    real(rkp), dimension(:), intent(inout)                            ::    Delta

    character(*), parameter                                           ::    ProcName='UpdateEstimators_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    real(rkp)                                                         ::    Variance
    integer                                                           ::    i

    Delta = This%GetSt()

    NbDim = size(This%StEstimator,1)

    call This%MomentEstimator%Update( Value=OutputA )
    Variance = This%MomentEstimator%GetVariance()

    i = 1
    do i = 1, NbDIm
      if ( ABRan(i) ) then
        This%StEstimator(i) = This%StEstimator(i) + (OutputA - OutputAB(i))**2
        This%StEstimatorNbSamples(i) = This%StEstimatorNbSamples(i) + 1
      end if
    end do

    Delta = Delta - This%GetSt()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateHistory_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='UpdateHistory_Cell'
    integer                                                           ::    StatLoc=0

    call This%MeanHistory%Append( Value=This%MomentEstimator%GetMean() )
    call This%VarianceHistory%Append( Value=This%MomentEstimator%GetVariance() )
    call This%StHistory%Append( Values=This%GetSt() )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSt_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetSt_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetSt_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    allocate(GetSt_Cell(size(This%StEstimator,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='(GetSt_Cell', ProcName=ProcName, stat=StatLoc )
    GetSt_Cell = Zero

    i = 1
    do i = 1, size(GetSt_Cell,1)
      if ( This%StEstimatorNbSamples(i) < 2 ) cycle
      GetSt_Cell(i) = This%StEstimator(i) / ( Two*real(This%StEstimatorNbSamples(i),rkp) * This%MomentEstimator%GetVariance() )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean_Cell( This )

    real(rkp)                                                         ::    GetMean_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetMean_Cell'
    integer                                                           ::    StatLoc=0

    GetMean_Cell = This%MomentEstimator%GetMean()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance_Cell( This )

    real(rkp)                                                         ::    GetVariance_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetVariance_Cell'
    integer                                                           ::    StatLoc=0

    GetVariance_Cell = This%MomentEstimator%GetVariance()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMeanHistory_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetMeanHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetMean_Cell'
    integer                                                           ::    StatLoc=0

    call This%MeanHistory%Get( Values=GetMeanHistory_Cell )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVarianceHistory_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetVarianceHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetVariance_Cell'
    integer                                                           ::    StatLoc=0

    call This%VarianceHistory%Get( Values=GetVarianceHistory_Cell )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetStHistory_Cell( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetStHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetStHistory_Cell'
    integer                                                           ::    StatLoc=0

    call This%StHistory%Get( Values=GetStHistory_Cell )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_Cell( LHS, RHS )

    class(Cell_Type), intent(out)                                     ::    LHS
    class(Cell_Type), intent(in)                                      ::    RHS

    character(*), parameter                                           ::    ProcName='Copy_Cell'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%MomentEstimator = RHS%MomentEstimator
      allocate(LHS%StEstimator, source=RHS%StEstimator, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%StEstimator', ProcName=ProcName, stat=StatLoc )
      allocate(LHS%StEstimatorNbSamples, source=RHS%StEstimatorNbSamples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc )
      LHS%StHistory = RHS%StHistory
      LHS%MeanHistory = RHS%MeanHistory
      LHS%VarianceHistory = RHS%VarianceHistory
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_Cell( This )

    type(Cell_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%StEstimator) ) deallocate(This%StEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StEstimatorNbSamples) ) deallocate(This%StEstimatorNbSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StEstimatorNbSamples', ProcName=ProcName, stat=StatLoc )

    call This%StHistory%Purge()
    call This%MeanHistory%Purge()
    call This%VarianceHistory%Purge()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
