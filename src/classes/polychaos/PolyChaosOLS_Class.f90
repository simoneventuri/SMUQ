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
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransf_Factory_Class                                     ,only:    SpaceTransf_Factory
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use SpaceInput_CLass                                              ,only:    SpaceInput_Type
use InputDet_Class                                                ,only:    InputDet_Type
use Output_Class                                                  ,only:    Output_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type

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
  procedure, public                                                   ::    AppendRecord            =>    AppendRecord_Cell
  procedure, public                                                   ::    GetNbRecords            =>    GetNbRecords_Cell
  procedure, public                                                   ::    GetRecord               =>    GetRecord_Cell
  procedure, public                                                   ::    SetModel                =>    SetModel_Cell
  procedure, public                                                   ::    IsConverged             =>    IsConverged_Cell
  procedure, public                                                   ::    GetIndicesPointer       =>    GetIndicesPointer_Cell
  procedure, public                                                   ::    GetCoefficientsPointer  =>    GetCoeffsPointer_Cell
  procedure, public                                                   ::    GetCVError              =>    GetCVError_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell  
end type

type                                                                  ::    Manager_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbCells=0
  type(Cell_Type), dimension(:), pointer                              ::    Cell=>null()
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Manager
  procedure, public                                                   ::    Reset                   =>    Reset_Manager
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Manager
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Manager
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Manager
  procedure, public                                                   ::    GetInput                =>    GetInput_Manager
  procedure, public                                                   ::    ProcessOutput           =>    ProcessOutput_Manager
  procedure, public                                                   ::    GetCellPointer          =>    GetCellPointer_Manager
  procedure, public                                                   ::    GetNbCells              =>    GetNbCells_Manager
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Manager
  final                                                               ::    Finalizer_Manager  
end type

type, extends(PolyChaosMethod_Type)                                   ::    PolyChaosOLS_Type
  type(LinSolverOLS_Type)                                             ::    Solver
  logical                                                             ::    Silent=.false.
  real(rkp)                                                           ::    StopError=Zero
  real(rkp)                                                           ::    DesignRatio=-1.0
  integer                                                             ::    Step=0
  type(LinkedList1D_Type)                                             ::    ParamRecord
  type(LinkedList1D_Type)                                             ::    ParamSample
  type(Manager_Type), allocatable, dimension(:)                       ::    Manager  
  integer                                                             ::    NbManagers=0
  type(SpaceSampler_Type)                                             ::    Sampler
  integer                                                             ::    IndexOrder=0
  logical                                                             ::    Preload=.false.
  integer                                                             ::    CheckpointFreqModel=0
  integer                                                             ::    CheckpointFreqNode=0
  logical                                                             ::    NodesAnalyzed=.true.
  integer                                                             ::    CheckpointFreq=-1
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    CheckInput
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
      This%Name = 'gPCols'
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

    if ( allocated(This%Manager) ) deallocate(This%Manager, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )
    This%NbManagers = 0

    This%Step = 0

    call This%ParamRecord%Purge()

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

    This%DesignRatio = -1.0
    This%StopError = Zero
    This%Preload = .false.
    This%SectionChain = ''
    This%IndexOrder = 0
    This%CheckpointFreq = -1

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
    if ( Found ) This%DesignRatio = VarR0D

    ParameterName = "stop_error"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%StopError = VarR0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    SectionName = 'sampler'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Sampler%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'solver'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Solver%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'preload'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      This%Preload = .true.

      ParameterName = 'index_order'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%IndexOrder = VarI0D

      ParameterName = 'step'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Step = VarI0D

      ParameterName = 'nodes_analyzed'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%NodesAnalyzed = VarL0D


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

      SubSectionName = SectionName // '>param_record'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%ParamRecord%Append( Values=VarR2D )
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )


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

    call This%CheckInput()

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
    SectionName = 'sampler'
    call GetInput%AddSection( Section=This%Sampler%GetInput( MainSectionName=SectionName,Prefix=PrefixLoc,Directory=DirectorySub))

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/ols_solver'
    SectionName = 'solver'
    call GetInput%AddSection( Section=This%Solver%GetInput( MainSectionName=SectionName,Prefix=PrefixLoc, Directory=DirectorySub))

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

      call GetInput%AddParameter( Name='nodes_analyzed', Value=ConvertToString(Value=ConvertToString(Value=This%NodesAnalyzed)),  &
                                                                                                         SectionName=SectionName )
      call GetInput%AddParameter( Name='step', Value=ConvertToString(Value=This%Step ), SectionName=SectionName )
      call GetInput%AddParameter( Name='index_order', Value=ConvertToString(Value=This%IndexOrder ), SectionName=SectionName)

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
  subroutine CheckInput( This, Debug )
    
    class(PolyChaosOLS_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CheckInput'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DesignRatio < One ) call Error%Raise( Line='Design ratio below minimum of 1', ProcName=ProcName )

    if ( This%StopError < Zero ) call Error%Raise( Line='Stop error below minimum of zero', ProcName=ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BuildModel( This, ModelInterface, Basis, SpaceInput, IndexSetScheme, Coefficients, Indices, CVErrors,                &
                                                                                                          OutputDirectory, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
    type(ModelInterface_Type), intent(inout)                          ::    ModelInterface
    type(OrthoMultiVar_Type), intent(inout)                           ::    Basis
    class(SpaceInput_Type), intent(inout)                             ::    SpaceInput
    class(IndexSetScheme_Type), intent(inout)                         ::    IndexSetScheme
    type(LinkedList0D_Type), allocatable, dimension(:), intent(out)   ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:), intent(out)   ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:), intent(out)   ::    Indices
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='BuildModel'
    integer, allocatable, dimension(:,:)                              ::    IndicesLoc
    integer                                                           ::    NbIndices
    real(rkp), allocatable, dimension(:,:)                            ::    DesignSpace
    integer                                                           ::    NbDim
    integer                                                           ::    NbCells
    type(InputDet_Type)                                               ::    Input
    type(Output_Type), allocatable, dimension(:)                      ::    Outputs
    real(rkp), allocatable, dimension(:,:)                            ::    ParamSample
    integer                                                           ::    VarI0D
    integer(8)                                                        ::    VarI0D_8_a
    integer(8)                                                        ::    VarI0D_8_b
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsLoc
    real(rkp), allocatable, dimension(:)                              ::    HatDiag
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), dimension(:), pointer                                  ::    VarR1DPointer=>null()
    real(rkp)                                                         ::    CVError
    real(rkp)                                                         ::    CorrFactor
    type(Cell_Type), pointer                                          ::    CellPointer=>null()
    integer                                                           ::    i, iStart, iEnd
    integer(8)                                                        ::    i_8
    integer                                                           ::    ii
    integer                                                           ::    M, N
    integer                                                           ::    IndexStartOrder
    logical                                                           ::    AllConvergedFlag=.false.
    logical                                                           ::    OrderExceededFlag=.false.
    logical                                                           ::    StepExceededFlag=.false.
    integer                                                           ::    StatLoc=0
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    Line
    real(rkp), allocatable, dimension(:,:)                            ::    QR
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    NbSamples
    type(Response_Type), pointer                                      ::    ResponsePointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Preload ) then
      i = 1
      do i = 1, This%NbManagers
        ResponsePointer => ModelInterface%GetResponsePointer(Num=i)
        VarR1DPointer => ResponsePointer%GetAbscissaPointer()
        if ( size(VarR1DPointer,1) /= This%Manager(i)%GetNbCells() ) call Error%Raise(                                            &
                           Line='Mismatch detected between preloaded and passed response abscissa total size', ProcName=ProcName )
        nullify(VarR1DPointer)
        nullify(ResponsePointer)
      end do
      This%Preload = .false.
    else
      This%NbManagers = ModelInterface%GetNbResponses()
      allocate( This%Manager(This%NbManagers), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, This%NbManagers
        ResponsePointer => ModelInterface%GetResponsePointer(Num=i)
        call This%Manager(i)%Construct( Response=ResponsePointer )
        nullify(ResponsePointer)
      end do
    end if

    SilentLoc = This%Silent

    NbDim = SpaceInput%GetNbDim()

    IndexStartOrder = IndexSetScheme%GetOrder()

    if ( This%Step == 0 ) This%IndexOrder = INdexStartOrder

    do

      AllConvergedFlag = .true.
      i = 1
      do i = 1, This%NbManagers
        if ( .not. AllConvergedFlag ) exit
        NbCells = This%Manager(i)%GetNbCells()
        ii = 1
        do ii = 1, NbCells
          CellPointer => This%Manager(i)%GetCellPointer( Num=ii )
          if ( .not. CellPointer%IsConverged(Goal=This%StopError) .and. AllConvergedFlag ) then
            AllConvergedFlag = .false.
            exit
          end if
        end do
      end do

      if ( AllConvergedFlag ) then
        if ( .not. SilentLoc ) then
          Line = 'All nodes converged'
          write(*,'(A)') '' 
          write(*,'(A)') Line
        end if   
        exit
      end if

      if ( .not. SilentLoc ) then
        if ( This%Step /= 0 ) then
          Line = 'Performing enrichment'
        else
          Line = 'Initial population of the linear system'
        end if
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if

      ! Generating Indices

      call IndexSetScheme%GenerateIndices( Order=This%IndexOrder, TupleSize=NbDim, Indices=IndicesLoc, OrderError=.false.,        &
                                                                                                 OrderExceeded=OrderExceededFlag )

      ! Exit if max order exceeded
      if ( OrderExceededFlag ) then
        if ( .not. SilentLoc ) then
          Line = 'Maximum index order exceeded -- exiting'
          write(*,'(A)') ''
          write(*,'(A)') Line
        end if   
        exit
      end if

      NbIndices = size(IndicesLoc,2)

      iStart = This%Step

      if ( This%NodesAnalyzed ) then
        if ( This%Step == 0 ) then
          call This%ParamSample%Append( Values=This%Sampler%Draw(SpaceInput=SpaceInput) )
        else
          if ( This%ParamSample%GetLength() == 0 ) then
            call This%ParamRecord%Get( Values=VarR2D )
            if ( This%DesignRatio > Zero ) then
              VarI0D = ceiling(real(size(IndicesLoc,2),rkp)*This%DesignRatio-real(size(VarR2D,2),rkp))
              if ( VarI0D > 0 ) call This%Sampler%Enrich( SpaceInput=SpaceInput, Samples=VarR2D, EnrichmentSamples=ParamSample,   &
                                                                            NbEnrichmentSamples=VarI0D, Exceeded=StepExceededFlag)
            else
              call This%Sampler%Enrich( SpaceInput=SpaceInput, Samples=VarR2D, EnrichmentSamples=ParamSample,                     &
                                                                                                        Exceeded=StepExceededFlag)
            end if
            if ( allocated(ParamSample) ) then
              call This%ParamSample%Append( Values=ParamSample )
              deallocate(ParamSample, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamSample', ProcName=ProcName, stat=StatLoc )
            end if
            deallocate(VarR2D, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
          end if
        end if
      end if

      if ( StepExceededFlag ) then
        Line = 'Maximum sampling step exceeded'
        if ( This%Step == 0 ) call Error%Raise( Line='Maximum sampling step exceeded prior to any samples being taken',           &
                                                                                                               ProcName=ProcName )
        write(*,'(A)') '' 
        write(*,'(A)') Line
        exit
      end if

      iEnd = int(This%ParamSample%GetLength(),4) + iStart

      ! Drawing necessary number of model output samples
      i = iStart
      do

        i = i + 1

        if ( i > iEnd ) exit

        This%Step = This%Step + 1

        if ( .not. SilentLoc ) then
          Line = 'Model run #' // ConvertToString(Value=This%Step)
          write(*,'(A)') Line
        end if

        call This%ParamSample%GetPointer( Node=1_8, Values=VarR1DPointer )
        call Input%Construct( Input=VarR1DPointer, Labels=SpaceInput%GetLabel() )
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
          if ( size(Outputs,1) /= This%NbManagers ) call Error%Raise( Line='Unexpected output size', ProcName=ProcName )

          call This%ParamRecord%Append( Values=VarR1DPointer )
          ii = 1
          do ii = 1, This%NbManagers
            call This%Manager(ii)%ProcessOutput( Output=Outputs(ii) )
          end do
        end if

        nullify(VarR1DPointer)
        call This%ParamSample%Remove( Node=1_8 )

        if ( This%CheckpointFreq > 0 .and. mod(i, abs(This%CheckpointFreq)) == 0 ) then
          call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),       &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
        end if

      end do

      iEnd = This%Step

      This%NodesAnalyzed = .false.

      if ( This%CheckpointFreq <= 0 .or. mod(i, abs(This%CheckpointFreq)) /= 0 ) then
        call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),         &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
      end if

      ! Constructing design space
      allocate( DesignSpace(iEnd,NbIndices), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )
      M = size(DesignSpace,1)
      N = size(DesignSpace,2)

      i_8 = 1
      do i_8 = 1, int(M,8)
        call This%ParamRecord%GetPointer( Node=i_8, Values=VarR1DPointer )
        DesignSpace(i_8,:) = Basis%Eval( X=VarR1DPointer, Indices=IndicesLoc )
        nullify( VarR1DPointer )
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

      i = 1
      do i = 1, This%NbManagers

        NbCells = This%Manager(i)%GetNbCells()
        ii = 1
        do ii = 1, NbCells
          CellPointer => This%Manager(i)%GetCellPointer( Num=ii )
          if ( CellPointer%IsConverged(Goal=This%StopError) ) cycle

          VarR1D = CellPointer%GetRecord( UpTo=M )

          call This%Solver%SolveSystemQR( System=DesignSpace, Goal=VarR1D, Coefficients=CoefficientsLoc, QR=QR, TAU=TAU,          &
                                                                                                                 CVError=CVError )

          call CellPointer%SetModel( Coefficients=CoefficientsLoc, Indices=IndicesLoc, CVError=CVError )

          if ( .not. SilentLoc ) then
            Line = 'Output ' // ConvertToString(Value=i) // ' Node ' // ConvertToString(Value=ii) //                          &
                                                                             ' -- Error = ' // ConvertToString(Value=CVError)
            if ( CellPointer%IsConverged(Goal=This%StopError) ) Line = Line // ' -- Converged'
            write(*,'(A)') Line
          end if

          nullify( CellPointer )

        end do

      end do

      ! Clean up and preperation for next loop

      deallocate(CoefficientsLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsLoc', ProcName=ProcName, stat=StatLoc )

      deallocate(TAU, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='TAU', ProcName=ProcName, stat=StatLoc )

      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      deallocate(QR, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='QR', ProcName=ProcName, stat=StatLoc )

      deallocate( DesignSpace, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DesignSpace', ProcName=ProcName, stat=StatLoc )

      This%IndexOrder = This%IndexOrder + 1

      This%NodesAnalyzed = .true.

      call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),          &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

    end do

    if ( .not. AllConvergedFlag ) then
      if ( .not. SilentLoc ) then
        Line = 'Some nodes did not converge.'
        write(*,'(A)') '' 
        write(*,'(A)') Line
      end if   
    end if

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

    call This%Reset()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, ModelInterface, Debug )

    class(PolyChaosOLS_Type), intent(inout)                           ::    This
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
          LHS%Silent = RHS%Silent
          LHS%StopError = RHS%StopError
          LHS%DesignRatio = RHS%DesignRatio
          LHS%Step = RHS%Step
          LHS%Sampler = RHS%Sampler
          LHS%NbManagers = RHS%NbManagers
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

    type(PolyChaosOLS_Type), intent(inout)                            ::    This

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
  subroutine Initialize_Manager( This, Debug )

    class(Manager_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize_Manager'

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
  subroutine Reset_Manager( This, Debug )

    class(Manager_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset_Manager'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%Cell) ) deallocate(This%Cell, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Manager( This, Debug )

    class(Manager_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Manager( This, Input, Prefix, Debug )

    use StringRoutines_Module

    class(Manager_Type), intent(inout)                                ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput_Manager'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
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

    This%NbCells = Input%GetNumberofSubSections()
    allocate( This%Cell(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbCells
      SectionName = "cell" // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%Cell(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Manager( This, Response, Debug )

    use String_Library

    class(Manager_Type), intent(inout)                                ::    This
    type(Response_Type), intent(in)                                   ::    Response
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1_Manager'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), pointer                                  ::    VarR1DPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    VarR1DPointer => Response%GetAbscissaPointer()

    This%NbCells = size(VarR1DPointer,1)
    allocate( This%Cell(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )
    
    nullify(VarR1DPointer)

    i = 1
    do i = 1, This%NbCells
      call This%Cell(i)%Construct()
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Manager( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput_Manager

    class(Manager_Type), intent(in)                                   ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput_Manager'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
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

    call GetInput_Manager%SetName( SectionName = trim(adjustl(MainSectionName)) )

    i = 1
    do i = 1, This%NbCells
      SectionName = 'cell' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cell' // ConvertToString(Value=i)
      call GetInput_Manager%AddSection( Section=This%Cell(i)%GetInput( MainSectionName=SectionName, Prefix=PrefixLoc,             &
                                                                                                        Directory=DirectorySub ) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ProcessOutput_Manager( This, Output, Debug )

    class(Manager_Type), intent(inout)                                ::    This
    type(Output_Type), intent(in)                                     ::    Output
    logical, optional ,intent(in)                                     ::    Debug
    
    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ProcessOutput_Manager'
    real(rkp), dimension(:,:), pointer                                ::    OrdinatePointer
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Output%GetOrdinateNbDegen() /= 1 ) call Error%Raise(                                                                     &
                                  Line='Current PCE implementation does not support multivalued model output', ProcName=ProcName )

    OrdinatePointer => Output%GetOrdinatePointer()

    i = 1
    do i = 1, This%NbCells
      call This%Cell(i)%AppendRecord( Entry=OrdinatePointer(i,1) )
    end do

    nullify( OrdinatePointer )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCellPointer_Manager( This, Num, Debug )

    type(Cell_Type), pointer                                          ::    GetCellPointer_Manager

    class(Manager_Type), intent(in)                                   ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCellPointer_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCellPointer_Manager => This%Cell(Num)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbCells_Manager( This, Debug )

    integer                                                           ::    GetNbCells_Manager

    class(Manager_Type), intent(in)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCellPointer_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbCells_Manager = This%NbCells

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_Manager( LHS, RHS )

    class(Manager_Type), intent(out)                                  ::    LHS
    class(Manager_Type), intent(in)                                   ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy_Manager'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%NbCells = RHS%NbCells
      allocate( LHS%Cell(RHS%NbCells), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Cell', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, RHS%NbCells
        LHS%Cell(i) = RHS%Cell(i)
      end do
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer_Manager( This )

    type(Manager_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Manager'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Cell) ) deallocate(This%Cell, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

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
  subroutine AppendRecord_Cell( This, Entry, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), intent(in)                                             ::    Entry
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='AppendRecord_Cell'
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

    if ( This%CVError > CVError ) then
      This%CVError = CVError

      if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

      if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

      allocate( This%Coefficients, source=Coefficients, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

      allocate( This%Indices, source=Indices, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConverged_Cell( This, Goal, Debug )

    logical                                                           ::    IsConverged_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), intent(in)                                             ::    Goal
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsConverged_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%CVError <= Goal ) then
      IsConverged_Cell = .true.
    else
      IsConverged_Cell = .false.
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
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
