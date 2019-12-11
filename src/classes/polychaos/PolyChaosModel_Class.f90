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

module PolyChaosModel_Class

use Input_Library
use Parameters_Library
use String_Library
use CommandRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use Response_Class                                                ,only:    Response_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use TransfSampleSpace_Factory_Class                               ,only:    TransfSampleSpace_Factory
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    PolyChaosModel_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  real(rkp), dimension(:), pointer                                    ::    Coefficients=>null()
  integer, dimension(:,:), pointer                                    ::    Indices=>null()
  real(rkp)                                                           ::    CVError=huge(One)
  real(rkp), dimension(:), pointer                                    ::    Coordinate=>null()
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Cell
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    GetIndicesPointer       =>    GetIndicesPointer_Cell
  procedure, public                                                   ::    GetCoefficientsPointer  =>    GetCoeffsPointer_Cell
  procedure, public                                                   ::    GetCoordinatePointer    =>    GetCoordPointer_Cell
  procedure, public                                                   ::    GetCVError              =>    GetCVError_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell  
end type

type, extends(Model_Type)                                             ::    PolyChaosModel_Type
  type(OrthoMultiVar_Type)                                            ::    Basis
  class(TransfSampleSpace_Type), allocatable                          ::    TransformedSpace
  integer                                                             ::    NbDim=0
  type(String_Type), allocatable, dimension(:)                        ::    InputLabel
  integer                                                             ::    NbCells=0
  type(Cell_Type), dimension(:), pointer                              ::    Cells=>null()
  character(:), allocatable                                           ::    OutputLabel
  type(String_Type), allocatable, dimension(:)                        ::    CoordinateLabels
contains
  procedure, public                                                   ::    Initialize              =>    Initialize
  procedure, public                                                   ::    Reset                   =>    Reset
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructInput2,        &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructInput2
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Run                     =>    RunCase2
  procedure, public                                                   ::    RunCase1
  procedure, public                                                   ::    RunCase2
  procedure, public                                                   ::    ReplaceInputLabel
  procedure, public                                                   ::    ReplaceOutputLabel
  procedure, public                                                   ::    GetNbInputs
  procedure, public                                                   ::    GetNbNodes
  procedure, public                                                   ::    GetCoefficientsPointer
  procedure, public                                                   ::    GetIndicesPointer
  procedure, public                                                   ::    GetCVError
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer 
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(PolyChaosModel_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'polychaosmodel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(PolyChaosModel_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )
    This%NbCells = 0

    if ( allocated(This%TransformedSpace) ) deallocate(This%TransformedSpace, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TransformedSpace', ProcName=ProcName, stat=StatLoc )
    This%NbDim = 0

    if ( allocated(This%CoordinateLabels) ) deallocate(This%CoordinateLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CoordinateLabels', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(PolyChaosModel_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%OutputLabel = '<undefined>'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Prefix )

    use StringRoutines_Module

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputReader_Type)                                            ::    Input
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    Strings
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    integer                                                           ::    IOLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()  

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    FileName = PrefixLoc // '/PCModelInput.dat'
    call Input%Read( FileName=FileName )

    SectionName = 'space_transform'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call TransfSampleSpace_Factory%Construct( Object=This%TransformedSpace, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    This%NbDim = This%TransformedSpace%GetNbDim()
    if ( This%NbDim < 1 ) call Error%Raise( Line='Dimensionality of parameter space below minimum of 1', ProcName=ProcName )

    allocate(This%InputLabel(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )
    i = 1
    do i = 1, This%NbDim
      This%InputLabel(i) = This%TransformedSpace%GetLabel(Num=i)
    end do

    SectionName = 'basis'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Basis%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    if ( This%Basis%GetNbDim() /= This%NbDim ) call Error%Raise(                                                                  &
               Line='Dimension of basis polynomials does not match the dimension of original parameter space', ProcName=ProcName )

    ParameterName = 'output_label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%OutputLabel = VarC0D

    SectionName = 'cells'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbCells = Input%GetNumberofSubSections()
    nullify(InputSection)

    allocate( This%Cells(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbCells
      SubSectionName = SectionName // ">cell" // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Cells(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
    end do

    allocate(This%CoordinateLabels(size(This%Cells(1)%GetCoordinatePointer())))
    ParameterName = 'coordinate_labels'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    call Parse( Input=VarC0D, Separator=' ', Output=Strings )
    if ( size(Strings,1) /= size(This%CoordinateLabels) ) call Error%Raise( 'Incorrect number of coordinate labels',              &
                                                                                                               ProcName=ProcName )
    i = 1
    do i = 1, size(This%CoordinateLabels)
      This%CoordinateLabels(i) = trim(adjustl(Strings(i)(:)))
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2( This, Input, Prefix )

    use StringRoutines_Module

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput2'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    Strings
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    integer                                                           ::    IOLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()  

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'space_transform'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call TransfSampleSpace_Factory%Construct( Object=This%TransformedSpace, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    This%NbDim = This%TransformedSpace%GetNbDim()
    if ( This%NbDim < 1 ) call Error%Raise( Line='Dimensionality of parameter space below minimum of 1', ProcName=ProcName )

    allocate(This%InputLabel(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )
    i = 1
    do i = 1, This%NbDim
      This%InputLabel(i) = This%TransformedSpace%GetLabel(Num=i)
    end do

    SectionName = 'basis'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Basis%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    if ( This%Basis%GetNbDim() /= This%NbDim ) call Error%Raise(                                                                  &
               Line='Dimension of basis polynomials does not match the dimension of original parameter space', ProcName=ProcName )

    ParameterName = 'output_label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%OutputLabel = VarC0D

    SectionName = 'cells'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbCells = Input%GetNumberofSubSections()
    nullify(InputSection)

    allocate( This%Cells(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbCells
      SubSectionName = SectionName // ">cell" // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Cells(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
    end do

    allocate(This%CoordinateLabels(size(This%Cells(1)%GetCoordinatePointer())))
    ParameterName = 'coordinate_labels'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    call Parse( Input=VarC0D, Separator=' ', Output=Strings )
    if ( size(Strings,1) /= size(This%CoordinateLabels) ) call Error%Raise( 'Incorrect number of coordinate labels',              &
                                                                                                               ProcName=ProcName )
    i = 1
    do i = 1, size(This%CoordinateLabels)
      This%CoordinateLabels(i) = trim(adjustl(Strings(i)(:)))
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Response, TransformedSpace, Basis, Coefficients, Indices, CVErrors )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    type(Response_Type), intent(in)                                   ::    Response
    class(TransfSampleSpace_Type), intent(in)                         ::    TransformedSpace
    type(OrthoMultiVar_Type), intent(in)                              ::    Basis
    type(LinkedList1D_Type), intent(inout)                            ::    Coefficients
    type(LinkedList2D_Type), intent(inout)                            ::    Indices
    type(LinkedList0D_Type), optional, intent(inout)                  ::    CVErrors

    character(*), parameter                                           ::    ProcName='ConstructCase'
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), pointer                                  ::    VarR1DPointer=>null()
    integer, dimension(:,:), pointer                                  ::    VarI2DPointer=>null()
    real(rkp), dimension(:,:), pointer                                ::    VarR2DPointer=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp)                                                         ::    VarR0D

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%Basis = Basis

    allocate(This%TransformedSpace, source=TransformedSpace, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TransformedSpace', ProcName=ProcName, stat=StatLoc )

    This%NbDim = This%TransformedSpace%GetNbDim()
    
    allocate(This%InputLabel(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      This%InputLabel(i) = TransformedSpace%GetLabel(i)
    end do

    This%OutputLabel = Response%GetLabel()

    if ( This%NbDim < 1 ) call Error%Raise( Line='Dimensionality of parameter space below minimum of 1', ProcName=ProcName )

    This%NbCells = Response%GetNbNodes()

    if ( This%NbCells /= Coefficients%GetLength() ) call Error%Raise( Line='Mismatch between number of coefficient records and '  &
                                                                                         // 'number of nodes', ProcName=ProcName )

    if ( This%NbCells /= Indices%GetLength() ) call Error%Raise( Line='Mismatch between number of indices records and '           &
                                                                                         // 'number of nodes', ProcName=ProcName )

    if ( present(CVErrors) ) then
      if ( This%NbCells /= CVErrors%GetLength() ) call Error%Raise( Line='Mismatch between number of CV error records and '       &
                                                                                         // 'number of nodes', ProcName=ProcName )
    end if

    allocate( This%Cells(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    VarR2DPointer => Response%GetCoordinatesPointer()

    allocate(VarR1D(Response%GetNbIndCoordinates()), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    VarR1D = Zero

    i = 1
    do i = 1, This%NbCells
      call Coefficients%GetPointer( Node=i, Values=VarR1DPointer )
      call Indices%GetPointer( Node=i, Values=VarI2DPointer )
      VarR1D = VarR2DPointer(i,:)
      if ( present(CVErrors) ) then
        call CVErrors%Get(Node=i, Value=VarR0D)
        call This%Cells(i)%Construct( Coefficients=VarR1DPointer, Indices=VarI2DPointer, Coordinate=VarR1D, CVError=VarR0D )
      else
        call This%Cells(i)%Construct( Coefficients=VarR1DPointer, Indices=VarI2DPointer, Coordinate=VarR1D )
      end if
      nullify(VarR1DPointer)
      nullify(VarI2DPointer)
    end do

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    allocate(This%CoordinateLabels, source=Response%GetCoordinateLabels(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CoordinateLabels', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(PolyChaosModel_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'space_transform'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/space_transform'
    call GetInput%AddSection( Section=TransfSampleSpace_Factory%GetObjectInput( Object=This%TransformedSpace,                     &
                                                         MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/basis'
    call GetInput%AddSection( Section=This%Basis%GetInput( MainSectionName='basis', Prefix=PrefixLoc, Directory=DirectorySub ) )

    call GetInput%AddParameter( Name='output_label', Value=This%OutputLabel )
    call GetInput%AddParameter( Name='coordinate_labels', Value=ConvertToString(Values=This%CoordinateLabels) )

    SectionName = 'cells'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbCells
      SubSectionName = 'cell' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/cell' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%Cells(i)%GetInput( MainSectionName=SubSectionName, Prefix=PrefixLoc,                 &
                                                                             Directory=DirectorySub ), To_SubSection=SectionName )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  subroutine RunCase1( This, Input, Output, Stat )
    
    class(PolyChaosModel_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat 

    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0

    if ( allocated(Output) ) then
      if ( size(Output,1) /= 1 ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if
    if ( .not. allocated(Output) ) then
      allocate(Output(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

    if ( present(Stat) ) then
      call This%Run( Input, Output(1), Stat )
    else
      call This%Run( Input, Output(1) )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunCase2( This, Input, Output, Stat )
    
    class(PolyChaosModel_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), intent(inout)                                  ::    Output
    integer, optional, intent(out)                                    ::    Stat 

    character(*), parameter                                           ::    ProcName='RunCase1'
    real(rkp), dimension(:), pointer                                  ::    CoefficientsPointer=>null()
    integer, dimension(:,:), pointer                                  ::    IndicesPointer=>null()
    integer                                                           ::    i, ii
    real(rkp), dimension(:,:), allocatable                            ::    Ordinate
    real(rkp), dimension(:), allocatable                              ::    Basis
    integer                                                           ::    NbCoefficients
    integer                                                           ::    StatLoc=0
    type(InputDet_Type)                                               ::    InputDetLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(8), external                                                 ::    DDOT
    character(:), allocatable                                         ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Input%GetNbInputs() < This%NbDim ) call Error%Raise( Line='Incorrect input dimensionality', ProcName=ProcName )

    select type (Input)

      type is (InputDet_Type)

        call Input%GetValue( Values=VarR1D, Labels=This%InputLabel )

        allocate( Ordinate(This%NbCells,1), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
    
        i = 1
        do i = 1, This%NbCells
          CoefficientsPointer => This%Cells(i)%GetCoefficientsPointer()
          IndicesPointer => This%Cells(i)%GetIndicesPointer()
          NbCoefficients = size(CoefficientsPointer,1)
          Basis = This%Basis%Eval( X=This%TransformedSpace%Transform(X=VarR1D), Indices=IndicesPointer )
          Ordinate(i,1) = dot_product(CoefficientsPointer, Basis )
          nullify( IndicesPointer )
          nullify( CoefficientsPointer )
        end do

        deallocate(Basis, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Basis', ProcName=ProcName, stat=StatLoc )

        call Output%Construct( Values=Ordinate, Label=This%OutputLabel )

        deallocate(Ordinate, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

      type is (InputStoch_Type)

        allocate( Ordinate(This%NbCells, Input%GetNbDegen()), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
    
        i = 1
        do i = 1, This%NbCells
          CoefficientsPointer => This%Cells(i)%GetCoefficientsPointer()
          IndicesPointer => This%Cells(i)%GetIndicesPointer()
          NbCoefficients = size(CoefficientsPointer,1)
          ii=1
          do ii = 1, Input%GetNbDegen()
            InputDetLoc = Input%GetDetInput(Num=ii)
            call InputDetLoc%GetValue( Values=VarR1D, Labels=This%InputLabel )
            Basis = This%Basis%Eval( X=This%TransformedSpace%Transform(X=VarR1D), Indices=IndicesPointer )
            Ordinate(i,ii) = DDOT( NbCoefficients, CoefficientsPointer, 1, Basis, 1 )
          end do
        end do

        deallocate(Basis, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Basis', ProcName=ProcName, stat=StatLoc )

        call Output%Construct( Values=Ordinate, Label=This%OutputLabel )
      
        deallocate(Ordinate, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

      class default
        call Error%Raise( Line='Type not recognized, update definitions', ProcName=ProcName )

    end select

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    if ( present(Stat) ) Stat = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReplaceInputLabel( This, OldLabel, NewLabel )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    OldLabel
    character(*), intent(in)                                          ::    NewLabel

    character(*), parameter                                           ::    ProcName='ReplaceInputLabel'
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbDim
      if ( This%InputLabel(i)%GetValue() == OldLabel ) then
        This%InputLabel(i) = NewLabel
        exit
      end if
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbInputs( This )

    integer                                                           ::    GetNbInputs

    class(PolyChaosModel_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetNbInputs'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbInputs = This%NbDim

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbNodes( This )

    integer                                                           ::    GetNbNodes

    class(PolyChaosModel_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetNbNodes'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbNodes = This%NbCells

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoefficientsPointer( This, Node )

    real(rkp), dimension(:), pointer                                  ::    GetCoefficientsPointer

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='GetCoefficientsPointer'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( Node < 1 .or. Node > This%NbCells) call Error%Raise( Line='Node specifier outside of bounds', ProcName=ProcName )

    GetCoefficientsPointer => This%Cells(Node)%GetCoefficientsPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndicesPointer( This, Node )

    integer, dimension(:,:), pointer                                  ::    GetIndicesPointer

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='GetIndicesPointer'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( Node < 1 .or. Node > This%NbCells) call Error%Raise( Line='Node specifier outside of bounds', ProcName=ProcName )

    GetIndicesPointer => This%Cells(Node)%GetIndicesPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCVError( This, Node )

    real(rkp)                                                         ::    GetCVError

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Node

    character(*), parameter                                           ::    ProcName='GetCVError'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( Node < 1 .or. Node > This%NbCells) call Error%Raise( Line='Node specifier outside of bounds', ProcName=ProcName )

    GetCVError = This%Cells(Node)%GetCVError()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReplaceOutputLabel( This, NewLabel )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    NewLabel

    character(*), parameter                                           ::    ProcName='ReplaceOutputLabel'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    This%OutputLabel = NewLabel

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(PolyChaosModel_Type), intent(out)                           ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (PolyChaosModel_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Basis = RHS%Basis
          LHS%InputLabel = RHS%InputLabel
          LHS%OutputLabel = RHS%OutputLabel
          allocate(LHS%TransformedSpace, source=RHS%TransformedSpace, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TransformedSpace', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Cells, source=RHS%Cells, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Cells', ProcName=ProcName, stat=StatLoc )
          LHS%NbCells = RHS%NbCells
          allocate(LHS%CoordinateLabels, source=RHS%CoordinateLabels, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CoordinateLabels', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(PolyChaosModel_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%TransformedSpace) ) deallocate(This%TransformedSpace, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TransformedSpace', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CoordinateLabels) ) deallocate(This%CoordinateLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CoordinateLabels', ProcName=ProcName, stat=StatLoc )

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

    if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Coordinate) ) deallocate(This%Coordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coordinate', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Cell( This )

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_Cell'

    THis%CVError = huge(One)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell( This, Input, Prefix )

    class(Cell_Type), intent(inout)                                   ::    This

    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
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

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'coordinate'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    VarR1D = ConvertToReals(String=VarC0D)
    allocate(This%Coordinate, source=VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coordinate', ProcName=ProcName, stat=StatLoc )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    ParameterName = 'pred_error'
    call input%GetValue( Value=VarR0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%CVError = VarR0D

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell( This, Coefficients, Indices, Coordinate, CVError )

    use String_Library

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    integer, dimension(:,:), intent(in)                               ::    Indices
    real(rkp), dimension(:), intent(in)                               ::    Coordinate
    real(rkp), optional, intent(in)                                   ::    CVError

    character(*), parameter                                           ::    ProcName='ConstructCase1_Cell'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(CVError) ) This%CVError=CVError

    allocate(This%Coordinate, source=Coordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coordinate', ProcName=ProcName, stat=StatLoc )

    allocate( This%Coefficients, source=Coefficients, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    allocate( This%Indices, source=Indices, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(in)                                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput_Cell'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    integer, allocatable, dimension(:)                                ::    VarI0D
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    mtuplesize=0
    integer                                                           ::    nbindices=0
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( DirectoryLoc /= '<undefined>' ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Cell%AddParameter( Name='coordinate', Value=ConvertToString( Values=This%Coordinate ) )
    call GetInput_Cell%AddParameter( Name='pred_error', Value=ConvertToString( Value=This%CVError ) )

    if ( ExternalFlag ) then
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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordPointer_Cell( This )

    real(rkp), pointer, dimension(:)                                  ::    GetCoordPointer_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetCoordPointer_Cell'
    integer                                                           ::    StatLoc=0    

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCoordPointer_Cell => This%Coordinate

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCVError_Cell( This )

    real(rkp)                                                         ::    GetCVError_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetCVError_Cell'
    integer                                                           ::    StatLoc=0    

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCVError_Cell = This%CVError

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoeffsPointer_Cell( This )

    real(rkp), dimension(:), pointer                                  ::    GetCoeffsPointer_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetCoeffsPointer_Cell'
    integer                                                           ::    StatLoc=0    

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCoeffsPointer_Cell => This%Coefficients

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndicesPointer_Cell( This )

    integer, dimension(:,:), pointer                                  ::    GetIndicesPointer_Cell

    class(Cell_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='GetIndicesPointer_Cell'
    integer                                                           ::    StatLoc=0    

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetIndicesPointer_Cell => This%Indices

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
      allocate(LHS%Coordinate, source=RHS%Coordinate, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Coordinate', ProcName=ProcName, stat=StatLoc )
      allocate( LHS%Coefficients, source=RHS%Coefficients, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Coefficients', ProcName=ProcName, stat=StatLoc )
      allocate( LHS%Indices, source=RHS%Indices, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Indices', ProcName=ProcName, stat=StatLoc )
      LHS%CVError = RHS%CVError
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_Cell( This )

    type(Cell_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    integer                                                           ::    StatLoc=0

    if ( associated(This%Coefficients) ) deallocate(This%Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Indices) ) deallocate(This%Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Coordinate) ) deallocate(This%Coordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coordinate', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
