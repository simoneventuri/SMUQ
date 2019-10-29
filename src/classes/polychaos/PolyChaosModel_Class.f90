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
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransf_Factory_Class                                     ,only:    SpaceTransf_Factory
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use SMUQFile_Class                                                ,only:     SMUQFile_Type

implicit none

private

public                                                                ::    PolyChaosModel_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  real(rkp), dimension(:), pointer                                    ::    Coefficients=>null()
  integer, dimension(:,:), pointer                                    ::    Indices=>null()
  real(rkp)                                                           ::    Abscissa=Zero
  real(rkp)                                                           ::    CVError=One
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
  procedure, public                                                   ::    GetAbscissa             =>    GetAbscissa_Cell
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
  character(:), allocatable                                           ::    OrdinateName
  character(:), allocatable                                           ::    AbscissaName
  character(:), allocatable                                           ::    OutputLabel
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Manager
  procedure, public                                                   ::    Reset                   =>    Reset_Manager
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Manager
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Manager
  procedure, private                                                  ::    ConstructCase1          =>    ConstructCase1_Manager
  procedure, public                                                   ::    GetInput                =>    GetInput_Manager
  procedure, public                                                   ::    GetCellPointer          =>    GetCellPointer_Manager
  procedure, public                                                   ::    GetNbCells              =>    GetNbCells_Manager
  procedure, public                                                   ::    GetAbscissaName         =>    GetAbscissaName_Manager
  procedure, public                                                   ::    GetOrdinateName         =>    GetOrdinateName_Manager
  procedure, public                                                   ::    GetOutputLabel          =>    GetOutputLabel_Manager
  procedure, public                                                   ::    ReplaceOutputLabel      =>    ReplaceLabel_Manager
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Manager
  final                                                               ::    Finalizer_Manager 
end type

type, extends(Model_Type)                                             ::    PolyChaosModel_Type
  type(OrthoMultiVar_Type)                                            ::    Basis
  class(SpaceTransf_Type), allocatable                                ::    SpaceTransf
  type(Manager_Type), allocatable, dimension(:)                       ::    Manager  
  integer                                                             ::    NbManagers=0
  integer                                                             ::    NbDim=0
  type(String_Type), allocatable, dimension(:)                        ::    InputLabel
contains
  procedure, public                                                   ::    Initialize              =>    Initialize
  procedure, public                                                   ::    Reset                   =>    Reset
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    RunCase1
  procedure, public                                                   ::    RunPredet
  procedure, public                                                   ::    ReplaceInputLabel
  procedure, public                                                   ::    ReplaceOutputLabel
  procedure, public                                                   ::    GetNbInputs
  procedure, public                                                   ::    GetNbOutputs
  procedure, public                                                   ::    GetNbManagers
  procedure, public                                                   ::    GetNbCells
  procedure, public                                                   ::    GetCoefficientsPointer
  procedure, public                                                   ::    GetIndicesPointer
  procedure, public                                                   ::    GetCVError
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer 
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'polychaosmodel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Manager) ) deallocate(This%Manager, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )

    This%NbManagers = 0

    if ( allocated(This%SpaceTransf) ) deallocate(This%SpaceTransf, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransf', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Prefix, Debug )

    use StringRoutines_Module

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputReader_Type)                                            ::    Input
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    StatLoc=0

    integer                                                           ::    UnitLoc
    integer                                                           ::    IOLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()  

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    FileName = PrefixLoc // '/PCModelInput.dat'
    call Input%Read( FileName=FileName )

    SectionName = 'space_transform'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call SpaceTransf_Factory%Construct( Object=This%SpaceTransf, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    This%NbDim = This%SpaceTransf%GetNbDim()

    allocate(This%InputLabel(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )
    i = 1
    do i = 1, This%NbDim
      This%InputLabel(i) = This%SpaceTransf%GetLabel(Num=i)
    end do

    SectionName = 'basis'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Basis%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )
    if ( This%Basis%GetNbDim() /= This%NbDim ) call Error%Raise(                                                                  &
               Line='Dimension of basis polynomials does not match the dimension of original parameter space', ProcName=ProcName )

    SectionName = 'outputs'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbManagers = InputSection%GetNumberOfSubSections()
    if ( This%NbManagers < 1 ) call Error%Raise( Line='Number of specified responses below minimum of 1', ProcName=ProcName )
    if ( This%NbDim < 1 ) call Error%Raise( Line='Dimensionality of parameter space below minimum of 1', ProcName=ProcName )
    allocate( This%Manager(This%NbManagers), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbManagers
      SubSectionName = SectionName // ">output" // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Manager(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Response, SpaceTransf, Basis, Coefficients, Indices, CVErrors, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    class(SpaceTransf_Type), intent(in)                               ::    SpaceTransf
    type(OrthoMultiVar_Type), intent(in)                              ::    Basis
    type(LinkedList1D_Type), dimension(:), intent(inout)              ::    Coefficients
    type(LinkedList2D_Type), dimension(:), intent(inout)              ::    Indices
    type(LinkedList0D_Type), dimension(:), intent(inout)              ::    CVErrors
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase'
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%Basis = Basis

    allocate(This%SpaceTransf, source=SpaceTransf, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%SpaceTransf', ProcName=ProcName, stat=StatLoc )

    This%NbDim = This%SpaceTransf%GetNbDim()
    This%NbManagers = size(Response,1)
    
    allocate(This%InputLabel(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      This%InputLabel(i) = SpaceTransf%GetLabel(i)
    end do

    if ( This%NbManagers < 1 ) call Error%Raise( Line='Number of specified responses below minimum of 1', ProcName=ProcName )
    if ( This%NbDim < 1 ) call Error%Raise( Line='Dimensionality of parameter space below minimum of 1', ProcName=ProcName )

    if ( This%NbManagers /= size(Coefficients,1) ) call Error%Raise( Line='Mismatch between number of coefficient records and '   &
                                                                                     // 'number of responses', ProcName=ProcName )

    if ( This%NbManagers /= size(Indices,1) ) call Error%Raise( Line='Mismatch between number of indices records and '            &
                                                                                     // 'number of responses', ProcName=ProcName )

    if ( This%NbManagers /= size(CVErrors,1) ) call Error%Raise( Line='Mismatch between number of CV error records and '          &
                                                                                     // 'number of responses', ProcName=ProcName )

    allocate( This%Manager(This%NbManagers), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbManagers
      call This%Manager(i)%Construct( Response=Response(i), Coefficients=Coefficients(i), Indices=Indices(i),CVErrors=CVErrors(i))
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(PolyChaosModel_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

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

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'space_transform'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/space_transform'
    call GetInput%AddSection( Section=SpaceTransf_Factory%GetObjectInput( Object=This%SpaceTransf, MainSectionName=SectionName,   &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/basis'
    call GetInput%AddSection( Section=This%Basis%GetInput( MainSectionName='basis', Prefix=PrefixLoc, Directory=DirectorySub ) )

    SectionName = 'outputs'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbManagers
      SubSectionName = 'output' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/output' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%Manager(i)%GetInput( MainSectionName=SubSectionName, Prefix=PrefixLoc,               &
                                                                             Directory=DirectorySub ), To_SubSection=SectionName )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunCase1( This, Input, Output, Stat, Debug )
    
    class(PolyChaosModel_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), allocatable, dimension(:), intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Input%GetNbInputs() < This%SpaceTransf%GetNbDim() ) call Error%Raise( Line='Incorrect input dimensionality', ProcName=ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(This%NbManagers), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= This%NbManagers ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(This%NbManagers), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    call This%RunPredet( Input=Input, Output=Output(1:This%NbManagers), Stat=StatLoc )

    if ( present(Stat) ) Stat = StatLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunPredet( This, Input, Output, Stat, Debug )
    
    class(PolyChaosModel_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='RunPredet'
    type(Cell_Type), pointer                                          ::    CellPointer=>null()
    real(rkp), dimension(:), pointer                                  ::    CoefficientsPointer=>null()
    integer, dimension(:,:), pointer                                  ::    IndicesPointer=>null()
    integer                                                           ::    i, ii, iii
    real(rkp), dimension(:,:), allocatable                            ::    Ordinate
    real(rkp), dimension(:), allocatable                              ::    Abscissa
    real(rkp), dimension(:), allocatable                              ::    Basis
    integer                                                           ::    NbCells
    integer                                                           ::    NbCoefficients
    integer                                                           ::    StatLoc=0
    type(InputDet_Type)                                               ::    InputDetLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(8), external                                                 ::    DDOT
    type(Manager_Type), pointer                                       ::    ManagerPointer=>null()
    character(:), allocatable                                         ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Input%GetNbInputs() < This%SpaceTransf%GetNbDim() ) call Error%Raise( Line='Incorrect input dimensionality',             &
                                                                                                               ProcName=ProcName )

    if ( size(Output,1) /= This%NbManagers ) call Error%Raise( Line='Passed incorrect size Output array', ProcName=ProcName )

    select type (Input)

      type is (InputDet_Type)

        call Input%GetValue( Values=VarR1D, Labels=This%InputLabel )

        i = 1
        do i = 1, This%NbManagers
          NbCells = This%Manager(i)%GetNbCells()
          allocate( Ordinate(NbCells,1), stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
          allocate( Abscissa(NbCells), stat=StatLoc )
      
          ii = 1
          do ii = 1, NbCells
            CellPointer => This%Manager(i)%GetCellPointer( Num=ii )
            CoefficientsPointer => CellPointer%GetCoefficientsPointer()
            IndicesPointer => CellPointer%GetIndicesPointer()
            NbCoefficients = size(CoefficientsPointer,1)
            Basis = This%Basis%Eval( X=This%SpaceTransf%Transform(X=VarR1D), Indices=IndicesPointer )
            Abscissa(ii) = CellPointer%GetAbscissa()
            Ordinate(ii,1) = dot_product(CoefficientsPointer, Basis )
            nullify( CellPointer )
            nullify( IndicesPointer )
            nullify( CoefficientsPointer )
          end do

          deallocate(Basis, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Basis', ProcName=ProcName, stat=StatLoc )

          call Output(i)%Construct( Abscissa=Abscissa, Ordinate=Ordinate, AbscissaName=This%Manager(i)%GetAbscissaName(),         &
                                          OrdinateName=This%Manager(i)%GetOrdinateName(), Label=This%Manager(i)%GetOutputLabel() )

          deallocate(Abscissa, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )
          deallocate(Ordinate, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        end do

      type is (InputStoch_Type)

        i = 1
        do i = 1, This%NbManagers
          NbCells = This%Manager(i)%GetNbCells()
          allocate( Ordinate(NbCells, Input%GetNbDegen()), stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
          allocate( Abscissa(NbCells), stat=StatLoc )
      
          ii = 1
          do ii = 1, NbCells
            CellPointer => This%Manager(i)%GetCellPointer( Num=ii )
            CoefficientsPointer => CellPointer%GetCoefficientsPointer()
            IndicesPointer => CellPointer%GetIndicesPointer()
            NbCoefficients = size(CoefficientsPointer,1)
            iii=1
            do iii = 1, Input%GetNbDegen()
              InputDetLoc = Input%GetDetInput(Num=iii)
              call InputDetLoc%GetValue( Values=VarR1D, Labels=This%InputLabel )
              Basis = This%Basis%Eval( X=This%SpaceTransf%Transform(X=VarR1D), Indices=IndicesPointer )
              Abscissa(ii) = CellPointer%GetAbscissa()
              Ordinate(ii,iii) = DDOT( NbCoefficients, CoefficientsPointer, 1, Basis, 1 )
            end do
          end do

          deallocate(Basis, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Basis', ProcName=ProcName, stat=StatLoc )

          call Output(i)%Construct( Abscissa=Abscissa, Ordinate=Ordinate, AbscissaName=This%Manager(i)%GetAbscissaName(),         &
                                          OrdinateName=This%Manager(i)%GetOrdinateName(), Label=This%Manager(i)%GetOutputLabel() )
        
          deallocate(Abscissa, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )
          deallocate(Ordinate, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        end do

      class default
        call Error%Raise( Line='Type not recognized, update definitions', ProcName=ProcName )

    end select

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    if ( present(Stat) ) Stat = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReplaceInputLabel( This, OldLabel, NewLabel, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    OldLabel
    character(*), intent(in)                                          ::    NewLabel
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReplaceInputLabel'
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbDim
      if ( This%InputLabel(i)%GetValue() == OldLabel ) then
        This%InputLabel(i) = NewLabel
        exit
      end if
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReplaceOutputLabel( This, OldLabel, NewLabel, Debug )

    class(PolyChaosModel_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    OldLabel
    character(*), intent(in)                                          ::    NewLabel
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReplaceOutputLabel'
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbManagers
      if ( This%Manager(i)%GetOutputLabel() == OldLabel ) then
        call This%Manager(i)%ReplaceOutputLabel( NewLabel=NewLabel )
        exit
      end if
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbInputs( This, Debug )

    integer                                                           ::    GetNbInputs

    class(PolyChaosModel_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbInputs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbInputs = This%NbDim

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs( This, Debug )

    integer                                                           ::    GetNbOutputs

    class(PolyChaosModel_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbOutputs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbOutputs = This%NbManagers

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbManagers( This, Debug )

    integer                                                           ::    GetNbManagers

    class(PolyChaosModel_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbManagers'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbManagers = This%NbManagers

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbCells( This, ManagerNum, Debug )

    integer                                                           ::    GetNbCells

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    ManagerNum
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbCells'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( ManagerNum < 1 ) call Error%Raise( Line='ManagerNum specifier below minimum of 1', ProcName=ProcName )

    GetNbCells = This%Manager(ManagerNum)%GetNbCells()

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoefficientsPointer( This, ManagerNum, CellNum, Debug )

    real(rkp), dimension(:), pointer                                  ::    GetCoefficientsPointer

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    ManagerNum
    integer, intent(in)                                               ::    CellNum
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCoefficientsPointer'
    type(Cell_Type), pointer                                          ::    CellPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( ManagerNum < 1 ) call Error%Raise( Line='ManagerNum specifier below minimum of 1', ProcName=ProcName )

    CellPointer => This%Manager(ManagerNum)%GetCellPointer( Num=CellNum )
    GetCoefficientsPointer => CellPointer%GetCoefficientsPointer()
    nullify(CellPointer)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndicesPointer( This, ManagerNum, CellNum, Debug )

    integer, dimension(:,:), pointer                                  ::    GetIndicesPointer

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    ManagerNum
    integer, intent(in)                                               ::    CellNum
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetIndicesPointer'
    type(Cell_Type), pointer                                          ::    CellPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( ManagerNum < 1 ) call Error%Raise( Line='ManagerNum specifier below minimum of 1', ProcName=ProcName )

    CellPointer => This%Manager(ManagerNum)%GetCellPointer( Num=CellNum )
    GetIndicesPointer => CellPointer%GetIndicesPointer()
    nullify(CellPointer)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCVError( This, ManagerNum, CellNum, Debug )

    real(rkp)                                                         ::    GetCVError

    class(PolyChaosModel_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    ManagerNum
    integer, intent(in)                                               ::    CellNum
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCVError'
    type(Cell_Type), pointer                                          ::    CellPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    if ( ManagerNum < 1 ) call Error%Raise( Line='ManagerNum specifier below minimum of 1', ProcName=ProcName )

    CellPointer => This%Manager(ManagerNum)%GetCellPointer( Num=CellNum )
    GetCVError = CellPointer%GetCVError()
    nullify(CellPointer)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(PolyChaosModel_Type), intent(out)                           ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (PolyChaosModel_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Basis = RHS%Basis
          LHS%InputLabel = RHS%InputLabel
          allocate(LHS%SpaceTransf, source=RHS%SpaceTransf, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%SpaceTransf', ProcName=ProcName, stat=StatLoc )
          LHS%NbManagers = RHS%NbManagers
          allocate( LHS%Manager(RHS%NbManagers), stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Manager', ProcName=ProcName, stat=StatLoc )
          i = 1
          do i = 1, RHS%NbManagers
            LHS%Manager(i) = RHS%Manager(i)
          end do
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(PolyChaosModel_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%SpaceTransf) ) deallocate(This%SpaceTransf, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransf', ProcName=ProcName, stat=StatLoc )
  
    if ( allocated(This%Manager) ) deallocate(This%Manager, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Manager', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

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

    This%AbscissaName = '<undefined>'
    This%OrdinateName = '<undefined>'
    This%OutputLabel = '<undefined>'

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
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
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

    ParameterName = 'ordinate_name'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%OrdinateName = VarC0D

    ParameterName = 'abscissa_name'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%AbscissaName = VarC0D

    ParameterName = 'output_label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%OutputLabel = VarC0D

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
  subroutine ConstructCase1_Manager( This, Response, Coefficients, Indices, CVErrors, Debug )

    use String_Library

    class(Manager_Type), intent(inout)                                ::    This
    type(Response_Type), intent(in)                                   ::    Response
    type(LinkedList1D_Type), intent(inout)                            ::    Coefficients
    type(LinkedList2D_Type), intent(inout)                            ::    Indices
    type(LinkedList0D_Type), intent(inout)                            ::    CVErrors
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1_Manager'
    integer(8)                                                        ::    i
    real(rkp), dimension(:), pointer                                  ::    CoefficientsPointer=>null()
    integer, dimension(:,:), pointer                                  ::    IndicesPointer=>null()
    real(rkp), pointer                                                ::    CVErrorPointer=>null()
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), pointer                                  ::    AbscissaPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%OrdinateName = Response%GetResponseName()
    This%AbscissaName = Response%GetAbscissaName()
    This%OutputLabel = Response%GetLabel()

    AbscissaPointer => Response%GetAbscissaPointer()

    This%NbCells = size(AbscissaPointer,1)
    allocate( This%Cell(This%NbCells), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, int(This%NbCells,8)
      call Coefficients%GetPointer( Node=i, Values=CoefficientsPointer )
      call Indices%GetPointer( Node=i, Values=IndicesPointer )
      call CVErrors%GetPointer( Node=i, Value=CVErrorPointer )

      call This%Cell(i)%Construct( Abscissa=AbscissaPointer(i), Coefficients=CoefficientsPointer,                                 &
                                                                                  Indices=IndicesPointer, CVError=CVErrorPointer )

      nullify( CoefficientsPointer )
      nullify( IndicesPointer )
      nullify( CVErrorPointer )
    end do

    nullify( AbscissaPointer )

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
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( DirectoryLoc /= '<undefined>' ) ExternalFlag = .true.

    call GetInput_Manager%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Manager%AddParameter( Name='abscissa_name', Value=This%AbscissaName )
    call GetInput_Manager%AddParameter( Name='ordinate_name', Value=This%OrdinateName )
    call GetInput_Manager%AddParameter( Name='output_label', Value=This%OutputLabel )

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
    character(*), parameter                                           ::    ProcName='GetNbCells_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbCells_Manager = This%NbCells

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAbscissaName_Manager( This, Debug )

    character(:), allocatable                                         ::    GetAbscissaName_Manager

    class(Manager_Type), intent(in)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissaName_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetAbscissaName_Manager = This%AbscissaName

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOrdinateName_Manager( This, Debug )

    character(:), allocatable                                         ::    GetOrdinateName_Manager

    class(Manager_Type), intent(in)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOrdinateName_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetOrdinateName_Manager = This%OrdinateName

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOutputLabel_Manager( This, Debug )

    character(:), allocatable                                         ::    GetOutputLabel_Manager

    class(Manager_Type), intent(in)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOutputLabel_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetOutputLabel_Manager = This%OutputLabel

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReplaceLabel_Manager( This, NewLabel, Debug )

    class(Manager_Type), intent(inout)                                ::    This
    character(*), intent(in)                                          ::    NewLabel
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReplaceLabel_Manager'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    This%OutputLabel = NewLabel

    if (DebugLoc) call Logger%Exiting()

  end subroutine
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
      LHS%AbscissaName = RHS%AbscissaName
      LHS%OrdinateName = RHS%OrdinateName
      LHS%NbCells = RHS%NbCells
      LHS%OutputLabel = RHS%OutputLabel
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

    This%Abscissa = Zero

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

    ParameterName = 'abscissa'
    call input%GetValue( Value=VarR0D, ParameterName=Parametername, Mandatory=.true. )
    This%Abscissa = VarR0D

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1_Cell( This, Abscissa, Coefficients, Indices, CVError, Debug )

    use String_Library

    class(Cell_Type), intent(inout)                                   ::    This
    real(rkp), intent(in)                                             ::    Abscissa
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    integer, dimension(:,:), intent(in)                               ::    Indices
    real(rkp), intent(in)                                             ::    CVError
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

    This%Abscissa = Abscissa

    This%CVError=CVError

    allocate( This%Coefficients, source=Coefficients, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coefficients', ProcName=ProcName, stat=StatLoc )

    allocate( This%Indices, source=Indices, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Indices', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(in)                                      ::    This
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
    integer, allocatable, dimension(:)                                ::    VarI0D
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    mtuplesize=0
    integer                                                           ::    nbindices=0
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( DirectoryLoc /= '<undefined>' ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Cell%AddParameter( Name='abscissa', Value=ConvertToString( Value=This%Abscissa ) )
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

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAbscissa_Cell( This, Debug )

    real(rkp)                                                         ::    GetAbscissa_Cell

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissa_Cell'
    integer                                                           ::    StatLoc=0    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetAbscissa_Cell = This%Abscissa

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

    GetIndicesPointer_Cell => This%Indices

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
      LHS%Abscissa = RHS%Abscissa
      allocate( LHS%Coefficients, source=RHS%Coefficients, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Coefficients', ProcName=ProcName, stat=StatLoc )
      allocate( LHS%Indices, source=RHS%Indices, stat=StatLoc )
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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
