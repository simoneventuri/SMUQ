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

module PolyChaosOLS_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use PolyChaosMethod_Class                                         ,only:    PolyChaosMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use SampleSpace_CLass                                             ,only:    SampleSpace_Type
use InputDet_Class                                                ,only:    InputDet_Type
use Output_Class                                                  ,only:    Output_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use List2D_Class                                                  ,only:    List2D_Type
use Model_Class                                                   ,only:    Model_Type

implicit none

private

public                                                                ::    PolyChaosOLS_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(LinkedList0D_Type)                                             ::    OutputRecord
  real(rkp), dimension(:), pointer                                    ::    Coefficients=>null()
  integer, dimension(:,:), pointer                                    ::    Indices=>null()
  real(rkp)                                                           ::    CVError=huge(1.0)                    
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Cell
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Cell
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
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell  
end type

type, extends(PolyChaosMethod_Type)                                   ::    PolyChaosOLS_Type
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
  integer                                                             ::    Step
  logical                                                             ::    Silent
  real(rkp)                                                           ::    StopError
  real(rkp)                                                           ::    DesignRatio=-1.0
  integer                                                             ::    CheckpointFreq=-1
  type(LinSolverOLS_Type)                                             ::    Solver
  real(rkp), allocatable, dimension(:,:)                              ::    ParamRecord
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  type(SpaceSampler_Type)                                             ::    Sampler
  integer                                                             ::    IndexOrder=0
  logical, allocatable, dimension(:)                                  ::    ParamSampleRan
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  logical                                                             ::    SamplesAnalyzed
  integer                                                             ::    ParamSampleStep
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
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
  subroutine Initialize( This, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'gPC_ols'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    This%NbCells = 0

    This%Step = 0

    if ( allocated(This%ParamRecord) ) deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
    
    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamSampleRan) ) deallocate(This%ParamSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

    call This%Sampler%Reset()

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%DesignRatio = Zero
    This%StopError = Zero
    This%SectionChain = ''
    This%IndexOrder = 0
    This%CheckpointFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%SamplesAnalyzed = .false.
    This%ParamSampleStep = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    logical, allocatable, dimension(:)                                ::    VarL1D
    integer                                                           ::    NbOutputs
    integer                                                           ::    NbCells

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName= 'silent'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Silent=VarL0D

    ParameterName = "design_ratio"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%DesignRatio = VarR0D
      if ( This%DesignRatio < One ) call Error%Raise( Line='Design ratio below minimum of 1', ProcName=ProcName )
    end if

    ParameterName = "stop_error"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%StopError = VarR0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    SectionName = 'sampler'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%Sampler%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    end if

    SectionName = 'solver'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Solver%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'restart'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then

      ParameterName = 'step'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Step = VarI0D

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
      if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
        nullify( InputSection )
        This%ParamSample = VarR2D
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

        SubSectionName = SectionName // '>param_sample_ran'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarL1D, Prefix=PrefixLoc )
        nullify( InputSection )
        This%ParamSampleRan = VarL1D

        ParameterName = 'param_sample_step'
        call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
        This%ParamSampleStep = VarI0D
      end if

      ParameterName = 'samples_obtained'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesObtained= VarL0D

      ParameterName = 'samples_ran'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesRan = VarL0D

      ParameterName = 'samples_analyzed'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%SamplesAnalyzed = VarL0D

      SubSectionName = SectionName // '>cells'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      NbCells = InputSection%GetNumberofSubSections()
      nullify(InputSection)

      allocate(This%Cells(NbCells), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, NbCells
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>cell' //                     &
                                                                                      ConvertToString(Value=i), Mandatory=.true. )
        call This%Cells(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
      end do
    end if

    if ( This%StopError < Zero ) call Error%Raise( Line='Stop error below minimum of zero', ProcName=ProcName )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName

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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='silent', Value=ConvertToString(Value=This%Silent ) )
    call GetInput%AddParameter( Name='design_ratio', Value=ConvertToString(Value=This%DesignRatio ) )
    call GetInput%AddParameter( Name='stop_error', Value=ConvertToString(Value=This%StopError ) )
    call GetInput%AddParameter( Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
    if ( This%Sampler%IsConstructed() ) then
      SectionName = 'sampler'
      call GetInput%AddSection( Section=This%Sampler%GetInput( MainSectionName=SectionName,                                       &
                                                                                        Prefix=PrefixLoc,Directory=DirectorySub) )
    end if

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/ols_solver'
    SectionName = 'solver'
    call GetInput%AddSection( Section=This%Solver%GetInput( MainSectionName=SectionName,Prefix=PrefixLoc, Directory=DirectorySub))

    if ( This%Step > 0 ) then
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
          call ExportArray( Input=InputSection, Array=This%ParamSampleRan, File=File )
          nullify(InputSection)

          call GetInput%AddParameter( Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                &
                                                                                                         SectionName=SectionName )
        end if
      else
        if ( allocated(This%ParamRecord) ) then
          SubSectionName = 'param_record'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call ExportArray( Input=InputSection, Array=This%ParamRecord )
          nullify(InputSection)
        end if

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

          call GetInput%AddParameter( Name='param_sample_step', Value=ConvertToString(Value=This%ParamSampleStep),                &
                                                                                                         SectionName=SectionName )
        end if

      end if

      call GetInput%AddParameter( Name='step', Value=ConvertToString(Value=This%Step ), SectionName=SectionName )
      call GetInput%AddParameter( Name='samples_obtained', Value=ConvertToString(Value=This%SamplesObtained ),                    &
                                                                                                         SectionName=SectionName )
      call GetInput%AddParameter( Name='samples_ran', Value=ConvertToString(Value=This%SamplesRan ), SectionName=SectionName )
      call GetInput%AddParameter( Name='samples_processed', Value=ConvertToString(Value=This%SamplesAnalyzed ),                   &
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

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BuildModel( This, Basis, SampleSpace, Responses, Model, IndexSetScheme, Coefficients, Indices, CVErrors,              &
                                                                             OutputDirectory, InputSamples, OutputSamples, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    type(OrthoMultiVar_Type), intent(inout)                           ::    Basis
    class(SampleSpace_Type), intent(inout)                            ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    class(IndexSetScheme_Type), intent(inout)                         ::    IndexSetScheme
    type(LinkedList0D_Type), allocatable, dimension(:), intent(out)   ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:), intent(out)   ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:), intent(out)   ::    Indices
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), optional, dimension(:,:), intent(in)                   ::    InputSamples
    type(List2D_Type), dimension(:), optional, intent(in)             ::    OutputSamples
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='BuildModel'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:,:)                              ::    IndicesLoc
    integer                                                           ::    NbIndices
    real(rkp), allocatable, dimension(:,:)                            ::    DesignSpace
    integer                                                           ::    NbDim
    integer                                                           ::    NbCells
    type(InputDet_Type)                                               ::    Input
    type(Output_Type), allocatable, dimension(:)                      ::    Outputs
    real(rkp), allocatable, dimension(:,:)                            ::    ParamSampleTemp
    integer                                                           ::    VarI0D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsLoc
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp), allocatable, dimension(:,:)                            ::    ParamSamplesTemp
    real(rkp), dimension(:), pointer                                  ::    VarR1DPointer=>null()
    real(rkp), dimension(:,:), pointer                                ::    VarR2DPointer=>null()
    real(rkp)                                                         ::    CVError
    real(rkp)                                                         ::    CorrFactor
    integer                                                           ::    i, iStart, iEnd
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    im1
    integer                                                           ::    iMax
    integer                                                           ::    iMin
    integer                                                           ::    M, N
    integer                                                           ::    IndexStartOrder
    integer                                                           ::    MaxIndexOrder
    logical                                                           ::    ConvergedFlag=.false.
    logical                                                           ::    OrderExceededFlag=.false.
    logical                                                           ::    StepExceededFlag=.false.
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    Line
    real(rkp), allocatable, dimension(:,:)                            ::    QR
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    NbSamples
    integer, allocatable, dimension(:)                                ::    NbCellsOutput
    integer                                                           ::    NbOutputs
    type(ModelInterface_Type)                                         ::    ModelInterface

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call ModelInterface%Construct( Model=Model, Responses=Responses )

    NbOutputs = size(Responses,1)
    NbDim = SampleSpace%GetNbDim()

    allocate(NbCellsOutput(NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='NbCellsOutput', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0
    i = 1
    do i = 1, NbOutputs
      NbCellsOutput(i) = Responses(i)%GetNbNodes()
      This%NbCells = This%NbCells + NbCellsOutput(i)
    end do


    if ( .not. allocated(This%Cells) ) then
      allocate(This%Cells(This%NbCells), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, This%NbCells
        call This%Cells(i)%Construct()
      end do 
    end if

    IndexStartOrder = IndexSetScheme%GetOrder()

    if ( This%Step == 0 ) then
      This%IndexOrder = IndexStartOrder
      if ( ( present(InputSamples) .and. .not. present(OutputSamples) ) .or.                                                      &
                                                                    ( present(OutputSamples) .and. .not. present(InputSamples) ) )&
                call Error%Raise( Line='Need both parameter and output samples to be passed at the same time', ProcName=ProcName )

      if ( present(InputSamples) ) then
        if ( .not. SilentLoc ) then
          Line = 'Processing precomputed samples'
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if

        if ( size(InputSamples,1) /= NbDim ) call Error%Raise( Line='Dimensionality of provided samples does not match ' //       &
                                                                      'the dimensionality of the input space', ProcName=ProcName )

        This%ParamRecord = InputSamples

        i = 1
        do i = 1, NbOutputs
          call OutputSamples(i)%GetPointer( Values=VarR2DPointer )
          if ( size(VarR2DPointer,1) /= size(InputSamples,2) ) call Error%Raise( 'Mismatch in number of input and output samples' &
                                                                                                             , ProcName=ProcName )
          if ( size(VarR2DPointer,2) /= NbCellsOutput(i) ) call Error%Raise( 'Mismatch in number of nodes in response and ' //    &
                                                                                     'initial output samples', ProcName=ProcName )

          if ( i > 1 ) then
            iMin = sum(NbCellsOutput(1:i-1)) + 1
          else
            iMin = 1
          end if

          iMax = sum(NbCellsOutput(1:i))
          iii = 0
          ii = iMin
          do ii = iMin, iMax
            iii = iii + 1
            call This%Cells(ii)%AppendRecord( Entries=VarR2DPointer(:,iii) )
          end do
          nullify(VarR2DPointer)
        end do
        This%Step = size(This%ParamRecord,2)
        This%SamplesObtained = .true.
        This%SamplesRan = .true.
        This%SamplesAnalyzed = .false.
      end if
    end if

    SilentLoc = This%Silent
    StepExceededFlag = .false.
    MaxIndexOrder = IndexSetScheme%GetMaxOrder()

    call IndexSetScheme%GenerateIndices( Order=This%IndexOrder, TupleSize=NbDim, Indices=IndicesLoc )

    do

       ! Checks if all cells converged during last iteration
      ConvergedFlag = .true.
      i = 1
      do i = 1, This%NbCells
        if ( This%Cells(i)%GetCVError() > This%StopError ) then
          ConvergedFlag = .false.
          exit
        end if
      end do

      if ( ConvergedFlag ) then
        if ( .not. SilentLoc ) then
          Line = 'All nodes converged'
          write(*,'(A)') '' 
          write(*,'(A)') Line
        end if   
        exit
      end if

      ! Checks if all cells reached maximum truncation order
      OrderExceededFlag = .true.
      i = 1
      do i = 1, This%NbCells
        if ( This%IndexOrder <= MaxIndexOrder ) then
          OrderExceededFlag = .false.
          exit
        end if
      end do

      if ( OrderExceededFlag ) then
        if ( .not. SilentLoc ) then
          Line = 'Index order max reached across all non-converged nodes'
          write(*,'(A)') '' 
          write(*,'(A)') Line
        end if   
        exit
      end if

      NbIndices = size(IndicesLoc,2)

      !***************************************************************************************************************************
      ! Obtaining samples
      if ( .not. This%SamplesObtained ) then

        if ( This%Sampler%IsConstructed() ) then
          if ( This%Step == 0 ) then
            if ( .not. SilentLoc ) then
              Line = 'Initial population of the linear system'
              write(*,'(A)') '' 
              write(*,'(A)') Line
              write(*,'(A)') '' 
            end if
            This%ParamSample = This%Sampler%Draw(SampleSpace=SampleSpace)
            if ( This%DesignRatio > Zero ) then
              do
                VarI0D = ceiling(real(size(IndicesLoc,2),rkp)*This%DesignRatio - real(size(This%ParamSample,2),rkp))
                if ( VarI0D <= 0 ) exit
                VarR2D = This%ParamSample
                call This%Sampler%Enrich( SampleSpace=SampleSpace, Samples=VarR2D, EnrichmentSamples=ParamSampleTemp,             &
                                                                                                      Exceeded=StepExceededFlag)
                if ( StepExceededFlag ) then
                  deallocate(VarR2D, stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
                  exit
                end if
                deallocate(This%ParamSample, stat=StatLoc)
                if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
                allocate(This%ParamSample(NbDim,size(VarR2D,2)+size(ParamSampleTemp,2)), stat=StatLoc)
                if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
                This%ParamSample(:,1:size(VarR2D,2)) = VarR2D
                THis%ParamSample(:,size(VarR2D,2)+1:) = ParamSampleTemp
                deallocate(VarR2D, stat=StatLoc)
                if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc ) 
                deallocate(ParamSampleTemp, stat=StatLoc)
                if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamSampleTemp', ProcName=ProcName, stat=StatLoc )
              end do

              if ( StepExceededFlag ) exit

            end if
          else
            if ( This%DesignRatio > Zero ) then
              VarI0D = ceiling(real(size(IndicesLoc,2),rkp)*This%DesignRatio-real(size(This%ParamRecord,2),rkp))
              if ( VarI0D > 0 ) then
                call This%Sampler%Enrich( SampleSpace=SampleSpace, Samples=This%ParamRecord,                                      &
                                                                    EnrichmentSamples=This%ParamSample, Exceeded=StepExceededFlag)
                if ( StepExceededFlag ) exit
                if ( .not. SilentLoc ) then
                  Line = 'Performing enrichment'
                  write(*,'(A)') '' 
                  write(*,'(A)') Line
                  write(*,'(A)') '' 
                end if
                do
                  VarI0D = ceiling(real(size(IndicesLoc,2),rkp)*This%DesignRatio -                                                &
                                                            (real(size(This%ParamRecord,2)+real(size(This%ParamSample,2),rkp))))
                  if ( VarI0D <= 0 ) exit
                  allocate(VarR2D(NbDim,size(This%ParamSample,2)+size(This%ParamRecord,2)), stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
                  VarR2D(:,1:size(This%ParamRecord,2)) = This%ParamRecord
                  VarR2D(:,size(This%ParamRecord,2)+1:) = This%ParamSample
                  deallocate(This%ParamSample, stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
                  call This%Sampler%Enrich( SampleSpace=SampleSpace, Samples=VarR2D, EnrichmentSamples=ParamSampleTemp,           &
                                                                                                        Exceeded=StepExceededFlag)
                  if ( StepExceededFlag ) then
                    deallocate(VarR2D, stat=StatLoc)
                    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
                    exit
                  end if
                  VarI0D = size(VarR2D,2)-size(This%ParamRecord,2)
                  allocate(This%ParamSample(NbDim,VarI0D+size(ParamSampleTemp,2)), stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
                  This%ParamSample(:,1:VarI0D) = VarR2D(:,size(This%ParamRecord,2)+1:)
                  This%ParamSample(:,VarI0D+1:) = ParamSampleTemp
                  deallocate(ParamSampleTemp, stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamSampleTemp', ProcName=ProcName, stat=StatLoc )
                  deallocate(VarR2D, stat=StatLoc)
                  if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
                end do
              else
                if ( .not. SilentLoc ) then
                  Line = 'Reusing samples with increased truncation order'
                  write(*,'(A)') '' 
                  write(*,'(A)') Line
                  write(*,'(A)') '' 
                end if
                This%SamplesRan = .true.
              end if
            else
              call This%Sampler%Enrich( SampleSpace=SampleSpace, Samples=This%ParamRecord, EnrichmentSamples=This%ParamSample,    &
                                                                                                        Exceeded=StepExceededFlag)
            end if
            if ( StepExceededFlag ) exit
          end if
          if ( allocated(This%ParamSample) ) then
            allocate(This%ParamSampleRan(size(This%ParamSample,2)), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )
            This%ParamSampleRan = .false.
            iEnd = size(This%ParamSample,2)
          end if
          This%SamplesObtained = .true.
        else
          VarI0D = ceiling(real(size(IndicesLoc,2),rkp)*This%DesignRatio-real(size(This%ParamRecord,2),rkp))
          if ( VarI0D > 0 ) then
            if ( This%Step == 0 ) call Error%Raise( 'More samples were required but sampler was never defined',                 &
                                                                                                             ProcName=ProcName )
            StepExceededFlag = .true.
            exit
          else
            if ( This%Step /= 0 ) then               
              if ( .not. SilentLoc ) then
                Line = 'Reusing samples with increased truncation order'
                write(*,'(A)') '' 
                write(*,'(A)') Line
                write(*,'(A)') '' 
              end if
            else
              if ( .not. SilentLoc ) then
                Line = 'Processing precomputed samples'
                write(*,'(A)') '' 
                write(*,'(A)') Line
                write(*,'(A)') '' 
              end if
            end if
            This%SamplesObtained = .true.
            This%SamplesRan = .true.
            This%SamplesAnalyzed = .false.
          end if
        end if
      else
        iEnd = size(This%ParamSample,2)
      end if

      !***************************************************************************************************************************
      ! Running samples
      if ( .not. This%SamplesRan ) then
        i = This%ParamSampleStep
        do
          i = i + 1
          if ( i > iEnd ) exit
          This%Step = This%Step + 1

          if ( .not. SilentLoc ) then
            Line = 'Model run #' // ConvertToString(Value=This%Step)
            write(*,'(A)') Line
          end if

          This%ParamSampleStep = i
          call Input%Construct( Input=This%ParamSample(:,This%ParamSampleStep), Labels=SampleSpace%GetLabel() )
          call ModelInterface%Run( Input=Input, Output=Outputs, Stat=StatLoc )

          if ( StatLoc /= 0 ) then
            if ( .not. SilentLoc ) then
              Line = 'Model run #' // ConvertToString(Value=This%Step) // ' -- Failed'
              write(*,'(A)') Line
            end if
            StatLoc = 0
            if ( allocated(Outputs) ) deallocate(Outputs, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )
            This%Step = This%Step - 1
          else
            This%ParamSampleRan(i) = .true.

            im1 = 0
            ii = 1
            do ii = 1, NbOutputs
              if ( Outputs(ii)%GetNbDegen() > 1 ) call Error%Raise( 'Polychaos procedure cant deal with stochastic responses',    &
                                                                                                               ProcName=ProcName )
              VarR2DPointer => Outputs(ii)%GetValuesPointer()
              iv = 0
              iii = im1 + 1
              do iii = im1+1, im1+size(VarR2DPointer,1)
                iv = iv + 1
                call This%Cells(iii)%AppendRecord( Entry=VarR2DPointer(iv,1) )
              end do
              im1 = im1 + size(VarR2DPointer,1)
              nullify(VarR2DPointer)
            end do
          end if

          if ( This%CheckpointFreq > 0 .and. mod(i, abs(This%CheckpointFreq)) == 0 ) then
            call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),       &
                            Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
          end if
    
        end do

        allocate(VarR2D(NbDim,This%Step), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        if ( allocated(This%ParamRecord) ) then
          iStart = size(This%ParamRecord,2)
          VarR2D(:,1:size(This%ParamRecord,2)) = This%ParamRecord
          deallocate(This%ParamRecord, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
        else
          iStart = 0
        end if

        i = iStart+1
        ii = 0
        do i = iStart+1, size(VarR2D,2)
          ii = ii + 1
          if ( This%ParamSampleRan(ii) ) then
            VarR2D(:,i) = This%ParamSample(:,ii)
          end if
        end do
        call move_alloc(VarR2D, This%ParamRecord)

        deallocate(This%ParamSample, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
        deallocate(This%ParamSampleRan, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

        This%ParamSampleStep = 0

      end if
     
      iEnd = size(This%ParamRecord,2)
      This%Step = iEnd

      call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),          &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )


      !***************************************************************************************************************************
      ! Updating coefficients
      if ( .not. This%SamplesAnalyzed ) then

        if ( iEnd < NbIndices ) call Error%Raise( 'Initial number of samples must be greater than the number of indices',         &
                                                                                                               ProcName=ProcName )

        ! Constructing design space
        allocate( DesignSpace(iEnd,NbIndices), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )
        M = size(DesignSpace,1)
        N = size(DesignSpace,2)

        i = 1
        do i = 1, M
          DesignSpace(i,:) = Basis%Eval( X=This%ParamRecord(:,i), Indices=IndicesLoc )
        end do

        ! Computing qr decomposition of the design space seperately since itll be the same for each cell

        allocate(QR, source=DesignSpace, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='QR', ProcName=ProcName, stat=StatLoc )
    
        allocate( TAU(min(M,N)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='TAU', ProcName=ProcName, stat=StatLoc )

        call DGEQRF( M, N, QR, M, TAU, WORKSIZE, -1, StatLoc  )
        if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

        LWORK = nint(WORKSIZE(1))

        allocate(WORK(LWORK), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

        call DGEQRF( M, N, QR, M, TAU, WORK, LWORK, StatLoc  )
        if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

        deallocate(WORK, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

        NbCells = 0

        allocate(VarR1D(M), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

        ii = 1
        do ii = 1, This%NbCells

          if ( This%Cells(ii)%GetCVError() <= This%StopError ) cycle

          VarR1D = This%Cells(ii)%GetRecord()

          call This%Solver%SolveSystemQR( System=DesignSpace, Goal=VarR1D, Coefficients=CoefficientsLoc, QR=QR, TAU=TAU,          &
                                                                                                                 CVError=CVError )

          if ( This%Cells(ii)%GetCVError() > CVError ) call This%Cells(ii)%SetModel( Coefficients=CoefficientsLoc,                &
                                                                                             Indices=IndicesLoc, CVError=CVError )

          if ( .not. SilentLoc ) then
            Line = ' Node ' // ConvertToString(Value=ii) // ' -- Error = ' // ConvertToString(Value=CVError)
            if ( CVError <= This%StopError ) Line = Line // ' -- Converged'
            write(*,'(A)') Line
          end if

        end do

        ! Clean up and preperation for next loop

        deallocate(CoefficientsLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )

        deallocate(TAU, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='TAU', ProcName=ProcName, stat=StatLoc )

        deallocate(VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

        deallocate(QR, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='QR', ProcName=ProcName, stat=StatLoc )

        deallocate( DesignSpace, stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )

        This%SamplesAnalyzed = .true.

      end if

      This%IndexOrder = This%IndexOrder + 1

      This%SamplesObtained = .false.
      This%SamplesRan = .false.
      This%SamplesAnalyzed = .false.

      call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),          &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

      call IndexSetScheme%GenerateIndices( Order=This%IndexOrder, TupleSize=NbDim, Indices=IndicesLoc, OrderError=.false.,        &
                                                                                                 OrderExceeded=OrderExceededFlag )

    end do

    This%SamplesObtained = .true.
    This%SamplesRan = .true.
    This%SamplesAnalyzed = .true.

    if ( StepExceededFlag ) then
      Line = 'Maximum sampling step exceeded'
      if ( This%Step == 0 ) call Error%Raise( Line='Maximum sampling step exceeded prior to any samples being taken',             &
                                                                                                               ProcName=ProcName )
      write(*,'(A)') ''  
      write(*,'(A)') Line
    end if

    if ( .not. ConvergedFlag ) then
      if ( .not. SilentLoc ) then
        Line = 'Some nodes did not converge.'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if   
    end if

    call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),             &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

    if ( present(OutputDirectory) ) call This%WriteOutput( Directory=OutputDirectory, Responses=Responses )

    ! Collecting results to construct polynomial chaos model object
    allocate( CVErrors(NbOutputs), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CVErrors', ProcName=ProcName, stat=StatLoc )
    allocate( Coefficients(NbOutputs), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
    allocate( Indices(NbOutputs), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Indices', ProcName=ProcName, stat=StatLoc )  

    i = 1
    do i = 1, NbOutputs
      iStart = 1
      if ( i > 1 ) iStart = sum(NbCellsOutput(1:(i-1)))+1
      iEnd = sum(NbCellsOutput(1:i))
      ii = 1
      do ii = iStart, iEnd
        call CVErrors(i)%Append( Value=This%Cells(ii)%GetCVError() )
        call Coefficients(i)%Append( Values=This%Cells(ii)%GetCoefficientsPointer() )
        call Indices(i)%Append( Values=This%Cells(ii)%GetIndicesPointer() )
        call This%Cells(ii)%Reset()
      end do
    end do

    This%Step = 0

    deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    This%NbCells = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, Responses, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    character(*), intent(in)                                          ::    Directory
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='WriteOutput'
    type(InputSection_Type)                                           ::    Input
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    logical                                                           ::    SilentLoc
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    NbOutputs
    integer                                                           ::    NbCells
    type(SMUQFile_Type)                                               ::    File
    class(Cell_Type), pointer                                         ::    CellPointer=>null()
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    Line
    integer                                                           ::    iStart
    integer                                                           ::    iEnd

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      if ( .not. SilentLoc ) then
        Line = 'Writing solver data to the output folder'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

      PrefixLoc = Directory

      NbOutputs = size(Responses,1)

      FileName = '/nbresponses.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call File%Export( String=ConvertToString(Value=NbOutputs) )

      FileName = '/sampled_parameters.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%ParamRecord, File=File )

      iStart = 0

      i = 1
      do i = 1, NbOutputs

        call MakeDirectory( Path=Directory // '/' // Responses(i)%GetLabel(), Options='-p' )

        NbCells = Responses(i)%GetNbNodes()
        iEnd = iStart + NbCells

        FileName = '/' // Responses(i)%GetLabel() // '/nbcells.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call File%Export( String=ConvertToString(Value=NbCells) )

        FileName = '/' // Responses(i)%GetLabel() // '/coordinates.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=Responses(i)%GetCoordinatesPointer(), File=File, RowMajor=.true. )

        iii = 0
        ii = iStart+1
        do ii = iStart+1, iEnd
          iii = iii + 1
          DirectoryLoc = '/' // Responses(i)%GetLabel() // '/cell' // ConvertToString(Value=iii)
          call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

          FileName = DirectoryLoc // '/cverror.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export( String=ConvertToString(Value=This%Cells(ii)%GetCVError()) )

          FileName = DirectoryLoc // '/coefficients.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=This%Cells(ii)%GetCoefficientsPointer(), File=File )

          FileName = DirectoryLoc // '/indices.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=This%Cells(ii)%GetIndicesPointer(), File=File )

          FileName = DirectoryLoc // '/sampled_output.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=This%Cells(ii)%GetRecord(), File=File )

        end do

        iStart = iEnd

      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(PolyChaosOLS_Type), intent(out)                             ::    LHS
    class(PolyChaosMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (PolyChaosOLS_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%DesignRatio = RHS%DesignRatio
          LHS%Silent = RHS%Silent
          LHS%StopError = RHS%StopError
          LHS%Step = RHS%Step
          LHS%CheckpointFreq = RHS%CheckpointFreq
          if ( RHS%Sampler%IsConstructed() ) LHS%Sampler = RHS%Sampler
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(PolyChaosOLS_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamRecord) ) deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
    
    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamSampleRan) ) deallocate(This%ParamSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize_Cell'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset_Cell'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    call This%OutputRecord%Purge()

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults_Cell'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%CVError = huge(1)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell( This, Input, Prefix, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    mtuplesize=0
    integer                                                           ::    nbindices=0
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'cv_error'
    call input%GetValue( Value=VarR0D, ParameterName=Parametername, Mandatory=.true. )
    This%CVError = VarR0D

    SectionName = 'output'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
    nullify( InputSection )
    call This%OutputRecord%Append( Values=VarR1D )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'coefficients'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
    nullify( InputSection )
    allocate(This%Coefficients, source=VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%', ProcName=ProcName, stat=StatLoc )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'indices'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarI2D, Prefix=PrefixLoc )
    nullify( InputSection )
    allocate(This%Indices, source=VarI2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )
    deallocate(VarI2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1_Cell'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%Coefficients(1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )
    This%Coefficients = 0

    allocate(This%Indices(1,1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )
    This%Indices = 0

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory, Debug )

    use CommandRoutines_Module
    use ArrayRoutines_Module
    use StringRoutines_Module
    use SMUQFile_Class                                            ,only:    SMUQFile_Type

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput_Cell'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    mtuplesize=0
    integer                                                           ::    nbindices=0
    integer                                                           ::    StatLoc=0

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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Cell%AddParameter( Name='cv_error', Value=ConvertToString(Value=This%CVError ) )

    if ( ExternalFlag ) then

      SectionName = 'output'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/output.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call This%OutputRecord%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D, File=File )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'coefficients'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/coefficients.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%Coefficients, File=File )
      nullify(InputSection)

      SectionName = 'indices'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      FileName = DirectoryLoc // '/indices.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%Indices, File=File )
      nullify(InputSection)

    else
      SectionName = 'output'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%OutputRecord%Get( Values=VarR1D )
      call ExportArray( Input=InputSection, Array=VarR1D )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)

      SectionName = 'coefficients'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ExportArray( Input=InputSection, Array=This%Coefficients )
      nullify(InputSection)

      SectionName = 'indices'
      call GetInput_Cell%AddSection( SectionName=SectionName )
      call GetInput_Cell%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ExportArray( Input=InputSection, Array=This%Indices )
      nullify(InputSection)
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendRecordR0D_Cell( This, Entry, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), intent(in)                                             ::    Entry
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='AppendRecordR0D_Cell'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call This%OutputRecord%Append( Value=Entry )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendRecordR1D_Cell( This, Entries, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    Entries
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='AppendRecordR1D_Cell'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call This%OutputRecord%Append( Values=Entries )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetModel_Cell( This, Coefficients, Indices, CVError, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    integer, dimension(:,:), intent(in)                               ::    Indices
    real(rkp), intent(in)                                             ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetModel_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    This%CVError = CVError

    if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    allocate( This%Coefficients, source=Coefficients, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    allocate( This%Indices, source=Indices, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetRecord_Cell( This, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetRecord_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetRecord_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%OutputRecord%GetLength() < 1 ) call Error%Raise( Line='Outputs not yet supplied', ProcName=ProcName )

    call This%OutputRecord%Get( Values=GetRecord_Cell )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoeffsPointer_Cell( This, Debug )

    real(rkp), dimension(:), pointer                                  ::    GetCoeffsPointer_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCoeffsPointer_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( .not. associated(This%Coefficients) ) call Error%Raise( Line='Coefficients not yet supplied', ProcName=ProcName )

    GetCoeffsPointer_Cell => This%Coefficients

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndicesPointer_Cell( This, Debug )

    integer, dimension(:,:), pointer                                  ::    GetIndicesPointer_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetIndicesPointer_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( .not. associated(This%Indices) ) call Error%Raise( Line='Indices not yet supplied', ProcName=ProcName )
    GetIndicesPointer_Cell => This%Indices

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCVError_Cell( This, Debug )

    real(rkp)                                                         ::    GetCVError_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCVError_Cell'
    integer                                                           ::    StatLoc=0  

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCVError_Cell = This%CVError

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_Cell( LHS, RHS )

    class(Cell_Type), intent(out)                                     ::    LHS
    class(Cell_Type), intent(in)                                      ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy_Cell'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%OutputRecord = RHS%OutputRecord
      LHS%CVError = RHS%CVError
      if ( associated(RHS%Coefficients) ) allocate( LHS%Coefficients, source=RHS%Coefficients, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Coefficients', ProcName=ProcName, stat=StatLoc )
      if ( associated(RHS%Indices) ) allocate( LHS%Indices, source=RHS%Indices, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Indices', ProcName=ProcName, stat=StatLoc )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer_Cell( This )

    type(Cell_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    call This%OutputRecord%Purge()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
