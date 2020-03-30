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

module SAMorris_Class

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

implicit none

private

public                                                                ::    SAMorris_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized
  logical                                                             ::    Constructed
  type(OnlineMeanEstimator_Type), allocatable, dimension(:)           ::    MuEstimator
  type(OnlineMeanEstimator_Type), allocatable, dimension(:)           ::    MuStarEstimator
  type(OnlineVarEstimator_Type), allocatable, dimension(:)            ::    VarEstimator
  type(LinkedList1D_Type)                                             ::    MuStarHistory
  type(LinkedList1D_Type)                                             ::    MuHistory
  type(LinkedList1D_Type)                                             ::    SigmaHistory
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

type, extends(SAMethod_Type)                                          ::    SAMorris_Type
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
  integer                                                             ::    CheckpointFreq
  logical                                                             ::    Silent
  class(SampleMethod_Type), allocatable                               ::    Sampler
  integer                                                             ::    NbTrajectories
  integer                                                             ::    NbGridLevels
  integer                                                             ::    StepSize
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  integer                                                             ::    ParamSampleStep
  integer                                                             ::    HistoryFreq
  type(LinkedList0D_Type)                                             ::    HistoryStep
  integer, allocatable, dimension(:,:)                                ::    Signs
  integer, allocatable, dimension(:,:)                                ::    PermutationIndices
  type(RandPseudo_Type)                                               ::    RNG

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

    class(SAMorris_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'SAMorris'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SAMorris_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    This%NbCells = 0

    call This%HistoryStep%Purge()

    if ( allocated(This%Sampler) ) deallocate(This%Sampler, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Sampler', ProcName=ProcName, stat=StatLoc )

    call This%RNG%Reset()

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(SAMorris_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%CheckpointFreq = -1
    This%HistoryFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%ParamSampleStep = 0
    This%NbTrajectories = 0
    This%NbGridLevels = 0
    This%StepSize = 3

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix )

    class(SAMorris_Type), intent(inout)                               ::    This
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

    ParameterName = 'nb_trajectories'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%NbTrajectories = VarI0D
    if ( This%NbTrajectories <= 0 ) call Error%Raise( 'Must specify at least 1 for number of trajectories', ProcName=ProcName )

    ParameterName = 'nb_grid_levels'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if ( Found ) then
      This%NbGridLevels = VarI0D
      if ( This%NbGridLevels <= 1 ) call Error%Raise( 'Must specify at least 2 grid levels', ProcName=ProcName )
    end if

    ParameterName = 'step_size'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if ( Found ) This%StepSize = VarI0D
    if ( This%StepSize <= 0 ) call Error%Raise( 'Trahectory step size must be above 0', ProcName=ProcName )

    SectionName = 'sampler'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call SampleMethod_Factory%Construct( Object=This%Sampler, Input=InputSection, Prefix=PrefixLoc )
    else
      allocate( SampleLHS_Type :: This%Sampler )
      select type (Object => This%Sampler)
        type is (SampleLHS_Type)
          call Object%Construct()
        class default
          call Error%Raise( Line='Something went wrong', ProcName=ProcName )
      end select
    end if

    SectionName = 'rng'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%RNG%Construct( Input=InputSection, Prefix=PrefixLoc )
    else
      call This%RNG%Construct()
    end if

    SectionName = 'restart'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then

      ParameterName = 'param_sample_step'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%ParamSampleStep = VarI0D

      ParameterName = 'samples_obtained'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesObtained= VarL0D

      ParameterName = 'samples_ran'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesRan = VarL0D

      SubSectionName = SectionName // '>permutation_indices'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%PermutationIndices, Prefix=PrefixLoc )
      nullify( InputSection )

      SubSectionName = SectionName // '>signs'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Signs, Prefix=PrefixLoc )
      nullify( InputSection )

      SubSectionName = SectionName // '>param_sample'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%ParamSample, Prefix=PrefixLoc )
      nullify( InputSection )

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

    class(SAMorris_Type), intent(inout)                               ::    This
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
    call GetInput%AddParameter( Name='nb_trajectories', Value=ConvertToString(Value=This%NbTrajectories) )
    if ( This%NbGridLevels > 0 ) call GetInput%AddParameter( Name='nb_grid_levels',                                               &
                                                                                  Value=ConvertToString(Value=This%NbGridLevels) )
    call GetInput%AddParameter( Name='step_size', Value=ConvertToString(Value=This%StepSize) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
    call GetInput%AddSection( Section=SampleMethod_Factory%GetObjectInput( Object=This%Sampler, MainSectionName='sampler',        &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection( Section=This%RNG%GetInput( MainSectionName='rng', Prefix=PrefixLoc, Directory=DirectorySub ) )

    if ( This%ParamSampleStep > 0 ) then
      SectionName = 'restart'
      call GetInput%AddSection( SectionName=SectionName )

      if ( ExternalFlag ) then
        
        if ( allocated(This%ParamSample) ) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/param_sample.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%ParamSample, File=File )
          nullify(InputSection)

          SubSectionName = 'signs'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/signs.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%Signs, File=File )
          nullify(InputSection)

          SubSectionName = 'permutation_indices'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/permutation_indices.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%PermutationIndices, File=File )
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

          SubSectionName = 'permutation_indices'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=This%PermutationIndices )
          nullify(InputSection)

          SubSectionName = 'signs'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=This%Signs )
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

    class(SAMorris_Type), intent(inout)                               ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory

    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    type(ModelInterface_Type)                                         ::    ModelInterface
    logical                                                           ::    SilentLoc
    integer                                                           ::    NbDim
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp), pointer, dimension(:,:)                                ::    VarR2DPtr=>null()
    integer, pointer, dimension(:,:)                                  ::    VarI2DPtr=>null()
    type(Input_Type), allocatable, dimension(:)                       ::    Input
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    v
    integer                                                           ::    vi
    integer                                                           ::    vii
    integer                                                           ::    iCellMin
    integer                                                           ::    iCellMax
    integer                                                           ::    iRunMin
    integer                                                           ::    iRunMax
    integer                                                           ::    NbInputs
    type(Output_Type), allocatable, dimension(:,:)                    ::    Outputs
    integer                                                           ::    NbResponses
    integer, allocatable, dimension(:)                                ::    NbCellsOutput
    integer                                                           ::    iStart
    integer                                                           ::    iEnd
    character(:), allocatable                                         ::    Line
    integer, allocatable, dimension(:)                                ::    RunStatLoc
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    GridSize
    real(rkp)                                                         ::    PerturbationSize
    real(rkp), allocatable, dimension(:,:)                            ::    XStar
    real(rkp), allocatable, dimension(:,:)                            ::    B
    real(rkp), allocatable, dimension(:,:)                            ::    P
    integer                                                           ::    NbGridLevelsLoc
    class(DistProb_Type), pointer                                     ::    DistProbPtr=>null()
    integer                                                           ::    NbTrajectories
    logical, allocatable, dimension(:)                                ::    SampleRan
    real(rkp), allocatable, dimension(:)                              ::    TrajectoryOutput
    integer                                                           ::    M
    integer                                                           ::    N

    call ModelInterface%Construct( Model=Model, Responses=Responses )

    NbResponses = size(Responses,1)
    NbDim = SampleSpace%GetNbDim()
    SilentLoc = This%Silent

    NbTrajectories = 0

    NbGridLevelsLoc = NbDim
    if ( This%NbGridLevels > 0 ) NbGridLevelsLoc = This%NbGridLevels

    if ( NbGridLevelsLoc - This%StepSize <= 0 ) call Error%Raise( 'Perturbation step size too large given the number ' //         &
                                                                                             'of grid levels', ProcName=ProcName )

    allocate(TrajectoryOutput(NbDim+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TrajectoryOutput', ProcName=ProcName, stat=StatLoc )
    TrajectoryOutput = Zero

    allocate(SampleRan(NbDim+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SampleRan', ProcName=ProcName, stat=StatLoc )
    SampleRan = .false.

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

    GridSize = One / real(NbGridLevelsLoc-1,rkp)
    PerturbationSize = GridSize*real(This%StepSize,rkp)

    if ( This%HistoryFreq > 0 .and. This%ParamSampleStep == 0 ) then
      call This%HistoryStep%Append( Value=0 )
      ii = 1
      do ii = 1, This%NbCells
        call This%Cells(ii)%UpdateHistory()
      end do
    end if

    !***************************************************************************************************************************
    ! Obtaining samples
    if ( .not. This%SamplesObtained ) then
      if ( .not. SilentLoc ) then
        Line = 'Initial population of trajectory samples'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if

      allocate(This%ParamSample(NbDim,This%NbTrajectories*(NbDim+1)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSamples', ProcName=ProcName, stat=StatLoc )
      This%ParamSample = Zero

      allocate(This%Signs(NbDim,This%NbTrajectories), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Signs', ProcName=ProcName, stat=StatLoc )
      This%Signs = 1
      
      allocate(This%PermutationIndices(NbDim,This%NbTrajectories), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PermutationIndices', ProcName=ProcName, stat=StatLoc )
      This%PermutationIndices = 0

      allocate(P(NbDim,NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='P', ProcName=ProcName, stat=StatLoc )
      P = Zero

      allocate(B(NbDim+1,NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='B', ProcName=ProcName, stat=StatLoc )
      call StrictTriangular( Array=B, UL='L' )

      VarI0D = NbGridLevelsLoc-This%StepSize

      VarR1D = LinSequence( SeqStart=0, SeqEnd=VarI0D )
      VarR1D = VarR1D / real(VarI0D,rkp)

      VarR2D = This%Sampler%Draw( NbSamples=This%NbTrajectories, NbDim=NbDim )

      allocate(VarI2D(NbDim, This%NbTrajectories) , stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )
      VarI2D = 0

      i = 1
      do i = 1, This%NbTrajectories
        ii = 1
        do ii = 1, NbDim
          iv = 0
          iii = 1
          do iii = 1, VarI0D
            if ( iii < VarI0D ) then
              if ( VarR2D(ii,i) >= VarR1D(iii) .and. VarR2D(ii,i) < VarR1D(iii+1) ) iv = iii
            else
              if ( VarR2D(ii,i) >= VarR1D(iii) ) iv = iii
            end if
          end do
          if ( iv == 0 ) call Error%Raise( 'Something went wrong', ProcName=ProcName )
          VarI2D(ii,i) = iv - 1
        end do
      end do 

      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

      XStar = real(VarI2D,rkp) * GridSize

      deallocate(VarI2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )

      M = NbDim
      N = NbDim+1

      i = 1
      do i = 1, This%NbTrajectories
        This%PermutationIndices(:,i) = LinSequence( SeqStart=1, SeqEnd=NbDim )
        call ScrambleArray( Array=This%PermutationIndices(:,i), RNG=This%RNG )
        P = Zero

        ii = 1
        do ii = 1, NbDim
          VarR0D = This%RNG%Draw()
          if ( VarR0D < 0.5 ) This%Signs(ii,i) = - 1
          P(This%PermutationIndices(ii,i),ii) = One
        end do

        VarR2D = B*Two - One

        ! performing loop to carry out multiplication of triangular matrix by a diagonal one and addition to the multiplication 
        ! of J by x' because it is faster
        VarR0D = PerturbationSize / Two
        ii = 1
        do ii = 1, NbDim
          VarR2D(:,ii) = (VarR2D(:,ii)*This%Signs(ii,i) + One)*VarR0D + XStar(ii,i)
        end do

        call DGEMM( 'T', 'T', NbDim, N, NbDim, One, P, NbDim, VarR2D, N, Zero,                                        &
                                                                        This%ParamSample(:,(i-1)*(NbDim+1)+1:i*(NbDim+1)), NbDim )
      end do

      deallocate(P, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='P', ProcName=ProcName, stat=StatLoc )

      deallocate(B, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='B', ProcName=ProcName, stat=StatLoc )

      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      deallocate(XStar, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='XStar', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, NbDim
        DistProbPtr => SampleSpace%GetDistributionPointer( Num=i )
        ii = 1
        do ii = 1, size(This%ParamSample,2)
          This%ParamSample(i,ii) = DistProbPtr%InvCDF( P=This%ParamSample(i,ii) )
        end do
      end do
        
      This%SamplesRan = .false.
      This%SamplesObtained = .true.
    end if

    !***************************************************************************************************************************
    ! Running samples
    if ( .not. This%SamplesRan ) then
      if ( .not. SilentLoc ) then
        Line = 'Running Samples'
        write(*,*)
        write(*,'(A)') Line
      end if

      iEnd = This%NbTrajectories
      i = This%ParamSampleStep/(NbDim+1)
      do
        if ( i >= iEnd ) exit

        NbTrajectories = iEnd - i
        if ( This%CheckPointFreq > 0 ) NbTrajectories = min(This%CheckPointFreq, iEnd-i)

        NbInputs = NbTrajectories*(NbDim+1)

        allocate(Input(NbInputs), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Input', ProcName=ProcName, stat=StatLoc )

        allocate(Outputs(NbResponses,NbInputs), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )

        allocate(RunStatLoc(NbInputs), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='RunStatLoc', ProcName=ProcName, stat=StatLoc )
        RunStatLoc = 1

        iii = This%ParamSampleStep
        ii = 1
        do ii = 1, NbInputs
          iii = iii + 1
          call Input(iii)%Construct( Input=This%ParamSample(:,iii), Labels=SampleSpace%GetLabel() )
        end do

        if ( .not. SilentLoc ) then
          Line = '  Running Trajectories #' // ConvertToString(Value=i+1) 
          if( NbTrajectories > 1 ) Line = Line // '-' // ConvertToString(Value=i+NbTrajectories)
          write(*,'(A)') Line
        end if
        
        call ModelInterface%Run( Input=Input, Output=Outputs, Stat=RunStatLoc )

        ! checking if any responses are stochastic
        ii = 1
        do ii = 1, NbInputs
          iii = 1
          do iii = 1, NbResponses
            if ( RunStatLoc(ii) == 0 ) then
              if ( Outputs(iii,ii)%GetNbDegen() > 1 ) call Error%Raise( 'Morris method does not support stochastic output',       &
                                                                                                               ProcName=ProcName )
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
            if ( RunStatLoc(iii) == 0 ) SampleRan(iii-iRunMin+1) = .true.
          end do

          iii = 1
          do iii = 1, NbResponses
              iCellMin = 1
              if ( iii > 1 ) iCellMin = sum(NbCellsOutput(1:iii-1)) + 1
              iCellMax = iCellMin + NbCellsOutput(iii) - 1

            iv = iCellMin
            do iv = iCellMin, iCellMax
              v = iRunMin
              do v = iRunMin, iRunMax
                if ( .not. SampleRan(v-iRunMin+1) ) cycle
                VarR2DPtr => Outputs(iii,v)%GetValuesPointer()
                TrajectoryOutput(v-iRunMin+1) = VarR2DPtr(iv-iCellMin+1,1)
                nullify(VarR2DPtr)
              end do
              call This%Cells(iv)%UpdateEstimators( TrajectoryOutput=TrajectoryOutput, SampleRan=SampleRan,                       &
                     PerturbationSize=PerturbationSize, Signs=This%Signs(:,ii), PermutationIndices=This%PermutationIndices(:,ii) )
            end do
          end do

          if ( This%HistoryFreq > 0 .and. (mod(i+ii, abs(This%HistoryFreq)) == 0 .and. i + ii /= iEnd)  ) then
            call This%HistoryStep%Append( Value=i+ii )
            iii = 1
            do iii = 1, This%NbCells
              call This%Cells(iii)%UpdateHistory()
            end do
          end if

        end do

        deallocate(RunStatLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='RunStatLoc', ProcName=ProcName, stat=StatLoc )

        deallocate(Outputs, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )

        deallocate(Input, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Input', ProcName=ProcName, stat=StatLoc )

        i = i + NbTrajectories
        This%ParamSampleStep = This%ParamSampleStep + NbInputs

        if ( i /= iEnd  ) then
          call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),     &
                        Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
        end if

      end do

      This%SamplesRan = .true.

    end if

    if ( This%HistoryFreq > 0 ) then
      call This%HistoryStep%Append( Value=i )
      ii = 1
      do ii = 1, This%NbCells
        call This%Cells(ii)%UpdateHistory()
      end do
    end if

    call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),         &
                      Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

    if ( present(OutputDirectory) ) call This%WriteOutput( Directory=OutputDirectory, Responses=Responses )

    deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Signs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Signs', ProcName=ProcName, stat=StatLoc )

    deallocate(This%PermutationIndices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PermutationIndices', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    deallocate(SampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SampleRan', ProcName=ProcName, stat=StatLoc )

    deallocate(TrajectoryOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='TrajectoryOutput', ProcName=ProcName, stat=StatLoc )

    deallocate(NbCellsOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, Responses )

    class(SAMorris_Type), intent(inout)                                ::    This
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

      FileName = '/trajectories.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%ParamSample, File=File )

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

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export(String=ConvertToString(Values=This%Cells(iv)%GetMu()))

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_star.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export(String=ConvertToString(Values=This%Cells(iv)%GetMuStar()))

          FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sigma.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export(String=ConvertToString(Values=This%Cells(iv)%GetSigma()))

          if ( This%HistoryFreq > 0 ) then
            call This%HistoryStep%Get( Values=VarI1D )
            FileName = '/' // ResponseLabel // '/history_step.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
            call ExportArray( Array=VarI1D, File=File, RowMajor=.true. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' ) 
            call ExportArray( Array=This%Cells(iv)%GetMuHistory(), File=File, RowMajor=.false. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/mu_star_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' ) 
            call ExportArray( Array=This%Cells(iv)%GetMuStarHistory(), File=File, RowMajor=.false. )

            FileName = '/' // ResponseLabel // '/cell' // ConvertToString(Value=v) // '/sigma_history.dat'
            call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
            call ExportArray( Array=This%Cells(iv)%GetSigmaHistory(), File=File, RowMajor=.false. )
          end if

          ii = iii + 1

        end do

      end do

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SAMorris_Type), intent(out)                                 ::    LHS
    class(SAMethod_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (SAMorris_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Silent = RHS%Silent
          allocate(LHS%Sampler, source=RHS%Sampler, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Sampler', ProcName=ProcName, stat=StatLoc )
          LHS%RNG = RHS%RNG
          LHS%NbTrajectories = RHS%NbTrajectories
          LHS%NbGridLevels = RHS%NbGridLevels
          LHS%StepSize = RHS%StepSize
          LHS%CheckPointFreq = RHS%CheckPointFreq
          LHS%HistoryFreq = RHS%HistoryFreq
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SAMorris_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    
    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Sampler) ) deallocate(This%Sampler, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Sampler', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Signs) ) deallocate(This%Signs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Signs', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%PermutationIndices) ) deallocate(This%PermutationIndices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PermutationIndices', ProcName=ProcName, stat=StatLoc )

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

    call This%MuHistory%Purge()
    call This%MuStarHistory%Purge()
    call This%SigmaHistory%Purge
    
    if ( allocated(This%MuEstimator) ) deallocate(This%MuEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MuStarEstimator) ) deallocate(This%MuStarEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%VarEstimator) ) deallocate(This%VarEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc )

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
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'nb_dim'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    NbDim = VarI0D

    allocate(This%MuEstimator(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc )

    allocate(This%MuStarEstimator(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc )

    allocate(This%VarEstimator(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc )

    SectionName = 'dimensions'
    i = 1
    do i = 1, NbDim
      SubSectionName = '>dimension' // ConvertToString(Value=i)

      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>mu_estimator',                 &
                                                                                                                Mandatory=.true. )
      call This%MuEstimator(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )

      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>mu_star_estimator',            &
                                                                                                                Mandatory=.true. )
      call This%MuStarEstimator(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )

      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>variance_estimator',           &
                                                                                                                Mandatory=.true. )
      call This%VarEstimator(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    end do

    SectionName = 'mu_history'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%MuHistory%Append( Values=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    SectionName = 'mu_star_history'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%MuStarHistory%Append( Values=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    SectionName = 'sigma_history'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%SigmaHistory%Append( Values=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell( This, Dimensionality )

    class(Cell_Type), intent(inout)                                   ::    This
    integer, intent(in)                                               ::    Dimensionality

    character(*), parameter                                           ::    ProcName='ConstructCase1_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%MuEstimator(Dimensionality), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc )

    allocate(This%MuStarEstimator(Dimensionality), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc )

    allocate(This%VarEstimator(Dimensionality), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, Dimensionality
      call This%MuEstimator(i)%Construct()
      call This%MuStarEstimator(i)%Construct()
      call This%VarEstimator(i)%Construct( SampleVariance=.true. )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory )

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
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    NbDim
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    NbDim = size(This%MuEstimator,1)

    SectionName = 'dimensions'
    call GetInput_Cell%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, NbDim
      SubSectionName = 'dimension' // ConvertToString(Value=i)
      call GetInput_Cell%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )

      call GetInput_Cell%AddSection( Section=This%MuEstimator(i)%GetInput(MainSectionName='mu_estimator', Prefix=PrefixLoc,       &
                                                     Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName )

      call GetInput_Cell%AddSection( Section=This%MuStarEstimator(i)%GetInput(MainSectionName='mu_star_estimator',                &
                                   Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName )

      call GetInput_Cell%AddSection( Section=This%VarEstimator(i)%GetInput(MainSectionName='variance_estimator', Prefix=PrefixLoc,&
                                                     Directory=DirectorySub), To_SubSection=SectionName // '>' // SubSectionName )

    end do

    if ( ExternalFlag ) then

      if ( This%MuHistory%GetLength() > 0 ) then
        SectionName = 'mu_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/mu_history.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call This%MuHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D, File=File )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

      if ( This%MuStarHistory%GetLength() > 0 ) then
        SectionName = 'mu_star_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/mu_star_history.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call This%MuStarHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D, File=File )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

      if ( This%SigmaHistory%GetLength() > 0 ) then
        SectionName = 'sigma_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/sigma_history.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call This%SigmaHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D, File=File )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

    else

      if ( This%MuHistory%GetLength() > 0 ) then
        SectionName = 'mu_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call This%MuHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

      if ( This%MuStarHistory%GetLength() > 0 ) then
        SectionName = 'mu_star_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call This%MuStarHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

      if ( This%SigmaHistory%GetLength() > 0 ) then
        SectionName = 'sigma_history'
        call GetInput_Cell%AddSection( SectionName=SectionName )
        call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call This%SigmaHistory%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      end if

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateEstimators_Cell( This, TrajectoryOutput, SampleRan, PerturbationSize, Signs, PermutationIndices )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    TrajectoryOutput
    logical, dimension(:), intent(in)                                 ::    SampleRan
    real(rkp), intent(in)                                             ::    PerturbationSize
    integer, dimension(:), intent(in)                                 ::    Signs
    integer, dimension(:), intent(in)                                 ::    PermutationIndices

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
      if ( SampleRan(i) .and. SampleRan(i+1) ) then
        DimIndex = PermutationIndices(i)
        EE = Signs(i)*( TrajectoryOutput(i+1)-TrajectoryOutput(i) ) / PerturbationSize
        call This%MuEstimator(DimIndex)%Update( Value=EE )
        call This%MuStarEstimator(DimIndex)%Update( Value=dabs(EE) )
        call This%VarEstimator(DimIndex)%Update( Value=EE )
      end if
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine UpdateHistory_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='UpdateHistory_Cell'
    integer                                                           ::    StatLoc=0

    call This%MuHistory%Append( Values=This%GetMu() )
    call This%MuStarHistory%Append( Values=This%GetMuStar() )
    call This%SigmaHistory%Append( Values=This%GetSigma() )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMu_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetMu_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetMu_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)

    allocate(GetMu_Cell(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetMu_Cell', ProcName=ProcName, stat=StatLoc )
    GetMu_Cell = Zero

    i = 1
    do i = 1, NbDim
      GetMu_Cell(i) = This%MuEstimator(i)%GetMean()
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMuStar_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetMuStar_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetMuStar_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)

    allocate(GetMuStar_Cell(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetMuStar_Cell', ProcName=ProcName, stat=StatLoc )
    GetMuStar_Cell = Zero

    i = 1
    do i = 1, NbDim
      GetMuStar_Cell(i) = This%MuStarEstimator(i)%GetMean()
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSigma_Cell( This )

    real(rkp), allocatable, dimension(:)                              ::    GetSigma_Cell

    class(Cell_Type), intent(in)                                      ::    This

    character(*), parameter                                           ::    ProcName='GetSigma_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDim

    NbDim = size(This%MuEstimator,1)

    allocate(GetSigma_Cell(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetSigma_Cell', ProcName=ProcName, stat=StatLoc )
    GetSigma_Cell = Zero

    i = 1
    do i = 1, NbDim
      GetSigma_Cell(i) = dsqrt(This%VarEstimator(i)%GetVariance())
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMuHistory_Cell( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetMuHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetMuHistory_Cell'
    integer                                                           ::    StatLoc=0

    if ( This%MuHistory%GetLength() > 0 ) then
      call This%MuHistory%Get( Values=GetMuHistory_Cell )
    else
      allocate(GetMuHistory_Cell(size(This%MuEstimator,1),1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='GetMuHistory_Cell', ProcName=ProcName, stat=StatLoc )
      GetMuHistory_Cell(:,1) = This%GetMu()
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMuStarHistory_Cell( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetMuStarHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetMuStarHistory_Cell'
    integer                                                           ::    StatLoc=0

    if ( This%MuStarHistory%GetLength() > 0 ) then
      call This%MuStarHistory%Get( Values=GetMuStarHistory_Cell )
    else
      allocate(GetMuStarHistory_Cell(size(This%MuEstimator,1),1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='GetMuStarHistory_Cell', ProcName=ProcName, stat=StatLoc )
      GetMuStarHistory_Cell(:,1) = This%GetMuStar()
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSigmaHistory_Cell( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetSigmaHistory_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetSigmaHistory_Cell'
    integer                                                           ::    StatLoc=0

    if ( This%SigmaHistory%GetLength() > 0 ) then
      call This%SigmaHistory%Get( Values=GetSigmaHistory_Cell )
    else
      allocate(GetSigmaHistory_Cell(size(This%MuEstimator,1),1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='GetSigmaHistory_Cell', ProcName=ProcName, stat=StatLoc )
      GetSigmaHistory_Cell(:,1) = This%GetSigma()
    end if

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
      allocate(LHS%MuEstimator, source=RHS%MuEstimator, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MuEstimator', ProcName=ProcName, stat=StatLoc )
      allocate(LHS%MuStarEstimator, source=RHS%MuStarEstimator, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MuStarEstimator', ProcName=ProcName, stat=StatLoc )
      allocate(LHS%VarEstimator, source=RHS%VarEstimator, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%varEstimator', ProcName=ProcName, stat=StatLoc )

      LHS%MuHistory = RHS%MuHistory
      LHS%MuStarHistory = RHS%MuStarHistory
      LHS%SigmaHistory = RHS%SigmaHistory
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_Cell( This )

    type(Cell_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%MuEstimator) ) deallocate(This%MuEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MuEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MuStarEstimator) ) deallocate(This%MuStarEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MuStarEstimator', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%VarEstimator) ) deallocate(This%VarEstimator, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%VarEstimator', ProcName=ProcName, stat=StatLoc )

    call This%MuHistory%Purge()
    call This%MuStarHistory%Purge()
    call This%SigmaHistory%Purge()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
