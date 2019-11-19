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

module PolyChaosSparse_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use PolyChaosMethod_Class                                         ,only:    PolyChaosMethod_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransf_Factory_Class                                     ,only:    SpaceTransf_Factory
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use InputDet_Class                                                ,only:    InputDet_Type
use Output_Class                                                  ,only:    Output_Type
use LinSolverSparse_Class                                         ,only:    LinSolverSparse_Type
use LinSolverSparse_Factory_Class                                 ,only:    LinSolverSparse_Factory
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    PolyChaosSparse_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(LinkedList0D_Type)                                             ::    OutputRecord
  real(rkp), dimension(:), pointer                                    ::    Coefficients=>null()
  integer, dimension(:,:), pointer                                    ::    Indices=>null()
  real(rkp)                                                           ::    CVError=huge(One)
  integer                                                             ::    IndexOrder=0
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
  procedure, public                                                   ::    GetNbRecords            =>    GetNbRecords_Cell
  procedure, public                                                   ::    GetRecord               =>    GetRecord_Cell
  procedure, public                                                   ::    SetModel                =>    SetModel_Cell
  procedure, public                                                   ::    GetIndicesPointer       =>    GetIndicesPointer_Cell
  procedure, public                                                   ::    GetCoefficientsPointer  =>    GetCoeffsPointer_Cell
  procedure, public                                                   ::    GetCVError              =>    GetCVError_Cell
  procedure, public                                                   ::    GetTruncationOrder      =>    GetTruncationOrder_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell  
end type

type, extends(PolyChaosMethod_Type)                                   ::    PolyChaosSparse_Type
  logical                                                             ::    Silent
  real(rkp)                                                           ::    StopError
  integer                                                             ::    Step
  integer                                                             ::    MaxNumOverfit
  integer                                                             ::    CheckpointFreq
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells  
  integer                                                             ::    NbCells
  class(LinSolverSparse_Type), allocatable                            ::    Solver
  real(rkp), allocatable, dimension(:,:)                              ::    ParamRecord
  real(rkp), allocatable, dimension(:,:)                              ::    ParamSample
  type(SpaceSampler_Type)                                             ::    Sampler


  integer, allocatable, dimension(:)                                  ::    NbCellsOutput
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
  procedure, private                                                  ::    UpdateCellCoefficients
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'gPC_sparse'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

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

    if ( allocated(This%NbCellsOutput) ) deallocate(This%NbCellsOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%NbCellsOutput', ProcName=ProcName, stat=StatLoc )

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

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%MaxNumOverfit = 3
    This%StopError = Zero
    This%SectionChain = ''
    This%CheckpointFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%SamplesProcessed = .false.
    This%ParamSampleStep = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix, Debug )

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    NbOutputs

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

    ParameterName = "max_num_overfit"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MaxNumOverfit = VarI0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    ParameterName = "stop_error"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%StopError = VarR0D

    SectionName = 'sampler'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%Sampler%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    end if

    SectionName = "sparse_solver"
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call LinSolverSparse_Factory%Construct( Object=This%Solver, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'preload'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      This%Preload = .true.

      SectionName = 'preload'
      ParameterName = 'step'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Step = VarI0D

      ParameterName = 'nodes_analyzed'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%NodesAnalyzed = VarL0D

      SubSectionName = SectionName // '>param_record'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%ParamRecord%Append( Values=VarR2D )
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

      SubSectionName = SectionName // '>param_sample'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.false.,                 &
                                                                                                              FoundSection=Found )
      if ( Found ) then
        call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
        nullify( InputSection )
        call This%ParamSample%Append( Values=VarR2D )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      end if

      SubSectionName = SectionName // '>managers'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      This%NbManagers = InputSection%GetNumberofSubSections()
      allocate(This%Manager(This%NbManagers), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, This%NbManagers
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName // '>manager' //                  &
                                                                                      ConvertToString(Value=i), Mandatory=.true. )
        call This%Manager(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
      end do
    end if

    if ( This%MaxNumOverfit < 2 ) call Error%Raise( Line='Number of allowable overfits below minimum off 2', ProcName=ProcName )

    if ( This%StopError < Zero ) call Error%Raise( Line='Stop error below minimum of zero', ProcName=ProcName )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
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
    call GetInput%AddParameter( Name='max_num_overfit', Value=ConvertToString(Value=This%MaxNumOverfit ) )
    call GetInput%AddParameter( Name='stop_error', Value=ConvertToString(Value=This%StopError ) )
    call GetInput%AddParameter( Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
    SectionName = 'sampler'
    call GetInput%AddSection( Section=This%Sampler%GetInput( MainSectionName=SectionName,Prefix=PrefixLoc,Directory=DirectorySub))

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sparse_solver'
    if ( This%BeingSampled ) then
      SectionName='sparse_solver'
      call GetInput%AddSection( Section=LinSolverSparse_Factory%GetObjectInput( Object=This%Solver, MainSectionName=SectionName,  &
                                                                                       Prefix=PrefixLoc, Directory=DirectorySub) )
    end if

    if ( This%Step > 0 ) then
      SectionName = 'preload'
      call GetInput%AddSection( SectionName=SectionName )

      if ( ExternalFlag ) then
        SubSectionName = 'param_record'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,         &
                                                                                                                Mandatory=.true. )
        FileName = DirectoryLoc // '/param_record.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call This%ParamRecord%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D, File=File )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)

        if ( This%ParamSample%GetLength() /= 0 ) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          FileName = DirectoryLoc // '/param_sample.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call This%ParamSample%Get( Values=VarR2D )
          call ExportArray( Input=InputSection, Array=VarR2D, File=File )
          deallocate(VarR2D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
          nullify(InputSection)
        end if
      else
        SubSectionName = 'param_record'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,         &
                                                                                                                Mandatory=.true. )
        call This%ParamRecord%Get( Values=VarR2D )
        call ExportArray( Input=InputSection, Array=VarR2D )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)

        if ( This%ParamSample%GetLength() /= 0 ) then
          SubSectionName = 'param_sample'
          call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
          call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,       &
                                                                                                                Mandatory=.true. )
          call This%ParamSample%Get( Values=VarR2D )
          call ExportArray( Input=InputSection, Array=VarR2D )
          deallocate(VarR2D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
          nullify(InputSection)
        end if

      end if

      call GetInput%AddParameter( Name='step', Value=ConvertToString(Value=This%Step ), SectionName=SectionName )
      call GetInput%AddParameter( Name='nodes_analyzed', Value=ConvertToString(Value=ConvertToString(Value=This%NodesAnalyzed)),  &
                                                                                                         SectionName=SectionName )
      SubSectionName = 'managers'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      i = 1
      do i = 1, This%NbManagers
        if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/manager' // ConvertToString(Value=i)
        call GetInput%AddSection( Section=This%Manager(i)%GetInput( MainSectionName='manager' // ConvertToString(Value=i),        &
                                  Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName // '>' // SubSectionName )
      end do
    end if
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BuildModel( This, ModelInterface, Basis, SpaceInput, IndexSetScheme, Coefficients, Indices, CVErrors,&
                                                                        OutputDirectory, SpaceInputSamples, OutputSamples, Debug )

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    type(ModelInterface_Type), intent(inout)                          ::    ModelInterface
    type(OrthoMultiVar_Type), intent(inout)                           ::    Basis
    class(SpaceInput_Type), intent(inout)                             ::    SpaceInput
    class(IndexSetScheme_Type), intent(inout)                         ::    IndexSetScheme
    type(LinkedList0D_Type), allocatable, dimension(:), intent(out)   ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:), intent(out)   ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:), intent(out)   ::    Indices
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), optional, dimension(:,:), intent(in)                   ::    SpaceInputSamples
    real(rkp), optional, dimension(:,:), intent(in)                   ::    OutputSamples
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='BuildModel'
    integer, allocatable, dimension(:,:)                              ::    IndicesLoc
    integer, dimension(:,:), pointer                                  ::    IndicesPointer=>null()
    integer                                                           ::    NbIndices
    real(rkp), allocatable, dimension(:,:)                            ::    DesignSpace
    integer                                                           ::    NbDim
    integer                                                           ::    NbCells
    type(Output_Type), allocatable, dimension(:)                      ::    Outputs
    integer                                                           ::    NbOutputs
    type(InputDet_Type)                                               ::    Input
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1, VarR1D_2
    real(rkp), dimension(:), pointer                                  ::    VarR1DPointer=>null()
    integer                                                           ::    VarI0D
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp)                                                         ::    CVError
    real(rkp)                                                         ::    CVErrorTrip
    type(Cell_Type), pointer                                          ::    CellPointer=>null()
    integer                                                           ::    i, iStart, iEnd
    integer                                                           ::    IndexStartOrder
    integer                                                           ::    IndexOrder
    integer                                                           ::    OverfitCounter=0
    integer                                                           ::    ii, iii, iv
    integer                                                           ::    im1
    integer                                                           ::    M, N
    logical                                                           ::    ConvergedFlag=.false.
    logical                                                           ::    OrderExceededFlag=.false.
    logical                                                           ::    StepExceededFlag=.false.
    integer                                                           ::    StatLoc=0
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    Line
    type(Response_Type), pointer                                      ::    ResponsePointer=>null()
    integer                                                           ::    MaxTruncationOrder

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbOutputs = ModelInterface%GetNbResponses()
    NbDim = SpaceInput%GetNbDim()

    allocate(This%NbCellsOutput(NbOutputs)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%NbCellsOutput', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0
    i = 1
    do i = 1, NbOutputs
      ResponsePointer => ModelInterface%GetResponsePointer(Num=i)
      NbCellsOutput(i) = ResponsePointer%GetNbNodes()
      This%NbCells = This%NbCells + NbCellsOutput(i)
    end do

    allocate(This%Cells(This%NbCells), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbCells
      This%Cells(i)%Construct()
    end do

    if ( This%Step == 0 ) then
      if ( ( present(SpaceInputSamples) .and. .not present(OutputSamples) ) .or.                                                  &
                                                                       ( present(OutputSamples) .and. .not present(InputSamples) )&
                call Error%Raise( Line='Need both parameter and output samples to be passed at the same time', ProcName=ProcName )

      if ( present(SpaceInputSamples) ) then
        if ( .not. SilentLoc ) then
          Line = 'Processing precomputed samples'
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if
        if ( size(OutputSamples,2) /= This%NbCells ) call Error%Raise( Line='Number of nodes in output samples does not match ' //& 
                                                                          'number of nodes for all responses', ProcName=ProcName )
        if ( size(SpaceInputSamples,2) /= size(OutputSamples,1) ) call Error%Raise( Line='Number of samples from input space ' // &
                                                                  'and number of output samples do not match', ProcName=ProcName )
        if ( size(SpaceInputSamples,1) /= NbDim ) call Error%Raise( Line='Dimensionality of provided samples does not match ' //  &
                                                                      'the dimensionality of the input space', ProcName=ProcName )                                        
        This%ParamRecord = SpaceInputSamples
        i = 1
        do i = 1, This%NbCells
          This%Cells(i)%AppendRecord( Entries=OutputSamples(:,i) )
        end do
        This%SamplesObtained = .true.
        This%SamplesRan = .true.
        This%SamplesProcessed = .false.
        This%Step = size(This%ParamRecord,2)
      end if
    end if

    SilentLoc = This%Silent
    StepExceededFlag = .false.
    MaxTruncationOrder = IndexSetScheme%GetMaxOrder()

    do

      ! Checks if all cells converged during last iteration
      ConvergedFlag = .true.
      i = 1
      do i = 1, This%NbCells
        if ( Cells(i)%GetCVError() > This%StopError ) then
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
        if ( Cells(i)%GetTruncationOrder() < MaxTruncationOrder ) then
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

      iStart = This%Step

      !***************************************************************************************************************************
      ! Obtaining samples
      if ( .not. This%SamplesObtained ) then

        if ( .not. SilentLoc ) then
          if ( This%Step /= 0 ) then
            Line = 'Performing enrichment'
          else
            Line = 'Initial population of the linear system'
          end if
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if

        if ( allocated(This%Sampler) ) then
          if ( This%SamplesRan ) then
            if ( This%Step == 0 ) then
              call This%ParamSample = This%Sampler%Draw(SpaceInput=SpaceInput)
              allocate(This%ParamSampleRan(size(ParamSample,2)), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )
              This%ParamSample = .false.
            else
              call This%Sampler%Enrich( SpaceInput=SpaceInput, Samples=This%ParamRecord, EnrichmentSamples=This%ParamSample,      &
                                                                                                        Exceeded=StepExceededFlag)
              if ( StepExceededFlag ) exit
            end if
            This%SamplesRan = .false.
          end if

          iEnd = size(This%ParamSample,2)
          This%SamplesObtained = .true.
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
          call Input%Construct( Input=ParamSample(:,This%ParamSampleStep), Labels=SpaceInput%GetLabel() )
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
            do ii = 1, ModelInterface%GetNbResponses()
              VarR1DPointer => Output(ii)%GetOrdinatePointer()
              iv = 0
              iii = im1 + 1
              do iii = im1+1, im1+size(VarR1DPointer)
                iv = iv + 1
                call This%Cells(iii)%AppendRecord( Entry=VarR1DPointer(iv) )
              end do
              im1 = im1 + size(VarR1DPointer)
              nullify(VarR1DPointer)
            end do
          end if

          if ( This%CheckpointFreq > 0 .and. mod(i, abs(This%CheckpointFreq)) == 0 ) then
            call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),       &
                            Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
          end if
    
        end do

        allocate(NbDim,This%Step), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        if ( allocated(This%ParamRecord) ) then
          VarR2D(:,1:size(This%ParamRecord,2)) = This%ParamRecord
          deallocate(This%ParamRecord, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
        end if

        i = iStart+1
        ii = 0
        do i = iStart+1, size(VarR2D,2)
          ii = ii + 1
          if ( This%ParamSampleRan(ii) ) then
            VarR2D(:,i) = This%ParamSample(ii)
          end if
        end do
        call move_alloc(VarR2D, This%ParamRecord)

        deallocate(This%ParamSample, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
        deallocate(This%ParamSampleRan, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

        This%ParamSampleStep = 0
        This%SamplesRan = .true.

      end if
     
      iEnd = size(This%ParamRecord,2)

      call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),          &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

      !***************************************************************************************************************************
      ! Updating coefficients
      if ( .not. This%SamplesProcessed ) then
        i = 1
        do i = 1, NbCells

          if ( This%Cell(i)%IsConverged(Goal=This%StopError) ) cycle

          OrderExceededFlag = .false.
          OverfitCounter = 0

          if ( iStart == 0 ) then
            IndexOrder = IndexSetScheme%GetOrder()
          else
            IndexOrder = CellPointer%GetTruncationOrder()
          end if

          CVErrorTrip = huge(VarR0D)

          do
            ! Generating Indices
            call IndexSetScheme%GenerateIndices( Order=IndexOrder, TupleSize=NbDim, Indices=IndicesLoc, OrderError=.false.,       &
                                                                                                 OrderExceeded=OrderExceededFlag )

            if ( OrderExceededFlag ) exit

            NbIndices = size(IndicesLoc,2)
            VarR1D_1 = This%Cells(i)%GetRecord( UpTo=iEnd )

            ! Constructing design space
            allocate( DesignSpace(iEnd,NbIndices), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )
            M = size(DesignSpace,1)
            N = size(DesignSpace,2)

            ii = 1
            do ii = 1, M
              DesignSpace(ii,:) = Basis%Eval( X=This%ParamRecord(:,ii), Indices=IndicesLoc )
            end do

            call This%Solver%Solve( System=DesignSpace, Goal=VarR1D_1, ModelSet=VarI1D, CoefficientsSet=VarR1D_2, CVError=CVError)  

            deallocate(DesignSpace, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )

            if ( CVError < CellPointer%GetCVError() ) then
              call CellPointer%SetModel( Coefficients=VarR1D_2, Indices=IndicesLoc(:,VarI1D), CVError=CVError,                    &
                                                                                                           IndexOrder=IndexOrder )
            end if
            
            if ( .not. SilentLoc ) then
              Line = 'Output ' // ConvertToString(Value=i) // ' Node ' // ConvertToString(Value=i) //                             &
                                                                                ' -- CVError = ' // ConvertToString(Value=CVError)
              if ( CellPointer%IsConverged(Goal=This%StopError) ) Line = Line // ' -- Converged'
              write(*,'(A)') Line
            end if

            if ( CVError >= CVErrorTrip ) then
              OverfitCounter = OverfitCounter + 1
              if ( OverfitCounter >= This%MaxNumOverfit ) exit
            else
              OverfitCounter = 0
            end if

            CVErrorTrip = CVError

            if ( CellPointer%IsConverged(Goal=This%StopError) ) exit

            IndexOrder = IndexOrder + 1

          end do

        end do

        This%SamplesProcessed = .true.
      end if

      call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),          &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )


      This%SamplesObtained = .false.
      This%SamplesRan = .false.
      This%SamplesProcessed = .false.

    end do

    if ( StepExceededFlag ) then
      Line = 'Maximum sampling step exceeded'
      if ( This%Step == 0 ) call Error%Raise( Line='Maximum sampling step exceeded prior to any samples being taken',             &
                                                                                                               ProcName=ProcName )
      write(*,'(A)') ''  
      write(*,'(A)') Line
      exit
    end if

    if ( .not. ConvergedFlag ) then
      if ( .not. SilentLoc ) then
        Line = 'Some nodes did not converge.'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if   
    end if

    if ( allocated(VarR1D_1) ) deallocate(VarR1D_2, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_1', ProcName=ProcName, stat=StatLoc )

    if ( allocated(VarR1D_2) ) deallocate(VarR1D_1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D_2', ProcName=ProcName, stat=StatLoc )


    call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),             &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

    if ( present(OutputDirectory) ) call This%WriteOutput( Directory=OutputDirectory, ModelInterface=ModelInterface )

    ! Collecting results to construct polynomial chaos model object
    allocate( CVErrors(This%NbManagers), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CVErrors', ProcName=ProcName, stat=StatLoc )
    allocate( Coefficients(This%NbManagers), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
    allocate( Indices(This%NbManagers), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Indices', ProcName=ProcName, stat=StatLoc )  

    i = 1
    do i = 1, This%NbManagers
      NbCells = This%Manager(i)%GetNbCells()
      ii = 1
      do ii = 1, NbCells
        CellPointer => This%Manager(i)%GetCellPointer( Num=ii )
        call CVErrors(i)%Append( Value=CellPointer%GetCVError() )
        call Coefficients(i)%Append( Values=CellPointer%GetCoefficientsPointer() )
        call Indices(i)%Append( Values=CellPointer%GetIndicesPointer() )
      end do
    end do

    This%Step = 0
    call This%ParamRecord%Purge()
    deallocate(This%Manager, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )
    This%NbManagers = 0
    This%Preload = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, ModelInterface, Debug )

    class(PolyChaosSparse_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    Directory
    type(ModelInterface_Type), intent(in)                             ::    ModelInterface
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='WriteOutput'
    type(InputSection_Type)                                           ::    Input
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    logical                                                           ::    SilentLoc
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    NbManagers
    integer                                                           ::    NbCells
    type(SMUQFile_Type)                                               ::    File
    class(Cell_Type), pointer                                         ::    CellPointer=>null()
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    Line
    type(Response_Type), pointer                                      ::    ResponsePointer=>null()

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

      FileName = '/nbmanagers.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call File%Export( String=ConvertToString(Value=This%NbManagers) )

      FileName = '/sampled_parameters.dat'
      call This%ParamRecord%Get( Values=VarR2D )
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=VarR2D, File=File )

      i = 1
      do i = 1, This%NbManagers
        call MakeDirectory( Path=Directory // '/manager_' // ConvertToString(Value=i) , Options='-p' )

        NbCells = This%Manager(i)%GetNbCells()

        FileName = '/manager_' // ConvertToString(Value=i) // '/nbcells.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call File%Export( String=ConvertToString(Value=NbCells) )


        ResponsePointer => ModelInterface%GetResponsePointer(Num=i)
        FileName = '/manager_' // ConvertToString(Value=i) // '/abscissa.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=ResponsePointer%GetAbscissaPointer(), File=File )

        ii = 1
        do ii = 1, NbCells
          DirectoryLoc = '/manager_' // ConvertToString(Value=i) // '/cell' // ConvertToString(Value=ii)
          call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

          CellPointer => This%Manager(i)%GetCellPointer( Num=ii )

          FileName = DirectoryLoc // '/cverror.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call File%Export( String=ConvertToString(Value=CellPointer%GetCVError()) )

          FileName = DirectoryLoc // '/coefficients.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=CellPointer%GetCoefficientsPointer(), File=File )

          FileName = DirectoryLoc // '/indices.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=CellPointer%GetIndicesPointer(), File=File )

          FileName = DirectoryLoc // '/sampled_output.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Array=CellPointer%GetRecord(), File=File )

          nullify(CellPointer)
        end do

      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(PolyChaosSparse_Type), intent(out)                          ::    LHS
    class(PolyChaosMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (PolyChaosSparse_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Solver = RHS%Solver
          LHS%Silent = RHS%Silent
          LHS%StopError = RHS%StopError
          LHS%Step = RHS%Step
          LHS%NbManagers = RHS%NbManagers
          LHS%Sampler = RHS%Sampler
          LHS%BeingSampled = RHS%BeingSampled
          allocate( LHS%Manager(RHS%NbManagers), stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Manager', ProcName=ProcName, stat=StatLoc )
          i = 1
          do i = 1, RHS%NbManagers
            LHS%Manager(i) = RHS%Manager(i)
          end do
          LHS%ParamRecord = RHS%ParamRecord
          LHS%ParamSample = RHS%ParamSample
          LHS%NodesAnalyzed = RHS%NodesAnalyzed
          LHS%CheckpointFreq = RHS%CheckpointFreq
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(PolyChaosSparse_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%Manager) ) deallocate(This%Manager, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )

    call This%ParamRecord%Purge()

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
    This%IndexOrder = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell( This, Input, Prefix, Debug )

    use String_Library
    use ArrayRoutines_Module

    class(Cell_Type), intent(inout)                                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
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

    ParameterName = 'index_order'
    call input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%IndexOrder = VarI0D

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

    use String_Library

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
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    type(SMUQFile_Type)                                               ::    File

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

    call GetInput_Cell%AddParameter( Name='index_order', Value=ConvertToString(Value=This%IndexOrder ) )

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
  subroutine SetModel_Cell( This, Coefficients, Indices, CVError, IndexOrder, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    integer, dimension(:,:), intent(in)                               ::    Indices
    real(rkp), intent(in)                                             ::    CVError
    integer, intent(in)                                               ::    IndexOrder
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetModel_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    This%IndexOrder = IndexOrder
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
  function GetNbRecords_Cell( This, Debug )

    integer(8)                                                        ::    GetNbRecords_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbRecords_Cell'
    integer                                                           ::    StatLoc=0  

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbRecords_Cell = This%OutputRecord%GetLength()

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetRecord_Cell( This, UpTo, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetRecord_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    integer, optional, intent(in)                                     ::    UpTo
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetRecord_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( This%OutputRecord%GetLength() < 1 ) call Error%Raise( Line='Outputs not yet supplied', ProcName=ProcName )

    if ( present(UpTo) ) then
      call This%OutputRecord%Get( Values=GetRecord_Cell, NodeMax=int(UpTo,8) )
    else
      call This%OutputRecord%Get( Values=GetRecord_Cell )
    end if

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
  function GetTruncationOrder_Cell( This, Debug )

    integer                                                           ::    GetTruncationOrder_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetTruncationOrder_Cell'
    integer                                                           ::    StatLoc=0  

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetTruncationOrder_Cell = This%IndexOrder

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
      LHS%IndexOrder = RHS%IndexOrder
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
