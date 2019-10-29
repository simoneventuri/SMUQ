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

module PCESM_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use Output_Class                                                  ,only:    Output_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use List1DAllocChar_Class                                         ,only:    List1DAllocChar_Type

implicit none

private

public                                                                ::    PCESM_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Directory
  type(PolyChaosModel_Type)                                           ::    PCEModel
  integer                                                             ::    NbOutputs=0
  integer                                                             ::    NbInputs=0
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Cell
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput          =>    ConstructInput_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    Run                     =>    Run_Cell
  procedure, public                                                   ::    GetNbOutputs            =>    GetNbOutputs_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_Cell
  final                                                               ::    Finalizer_Cell
end type

type, extends(ModelExtTemplate_Type)                                  ::    PCESM_Type
  type(Cell_Type), allocatable, dimension(:)                          ::    Cell
  integer                                                             ::    NbCells=0
  integer                                                             ::    NbOutputs=0
  integer                                                             ::    NbFixedParams=0
  real(rkp), allocatable, dimension(:)                                ::    FixedParamVals
  type(String_Type), allocatable, dimension(:)                        ::    FixedParamLabels   
  integer                                                             ::    NbTransformParams=0   
  type(List1DAllocChar_Type), allocatable, dimension(:)               ::    ParamTransform      
  type(String_Type), allocatable, dimension(:)                        ::    ParamTransformLabel 
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    RunCase1
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(PCESM_Type), intent(inout)                                  ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'pcesmmodel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(PCESM_Type), intent(inout)                                  ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Cell) ) deallocate(This%Cell, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamLabels) ) deallocate(This%FixedParamLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamVals) ) deallocate(This%FixedParamVals, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransform) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransformLabel) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0
    This%NbOutputs = 0
    This%NbFixedParams = 0
    This%NbTransformParams = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(PCESM_Type), intent(inout)                                  ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use String_Library

    class(PCESM_Type), intent(inout)                                  ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    integer, allocatable, dimension(:)                                ::    OutputMap
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    real(rkp)                                                         ::    VarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'models'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbCells = InputSection%GetNumberOfSubSections()
    nullify(InputSection)

    allocate(This%Cell(This%NbCells), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )
    This%NbOutputs = 0

    i = 1
    do i = 1, This%NbCells
      SubSectionName = SectionName // '>model' // Convert_To_String(i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Cell(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
      This%NbOutputs = This%NbOutputs + This%Cell(i)%GetNbOutputs()
    end do

    SectionName = 'fixed_parameters'
    if ( Input%HasSection(SubSectionName=SectionName) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      This%NbFixedParams = InputSection%GetNumberOfSubSections()
      nullify(InputSection)

      allocate(This%FixedParamLabels(This%NbFixedParams), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

      allocate(This%FixedParamVals(This%NbFixedParams), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )
      This%FixedParamVals = Zero

      i = 1
      do i = 1, This%NbFixedParams
        SubSectionName = SectionName // '>parameter' // ConvertToString(i)
        ParameterName = 'label'
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
        This%FixedParamLabels(i) = VarC0D

        ParameterName = 'value'
        call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
        This%FixedParamVals(i) = VarR0D
      end do

      i = 1
      do i = 1, This%NbFixedParams
        ii = 1
        do ii = i + 1 , This%NbFixedParams
          if ( This%FixedParamLabels(i)%GetValue() == This%FixedParamLabels(ii)%GetValue() ) call Error%Raise(                    &
                             Line='Duplicate fixed parameter labels : ' // This%FixedParamLabels(i)%GetValue(), ProcName=ProcName)
        end do
      end do

    end if

    SectionName = 'transformed_parameters'
    if ( Input%HasSection(SubSectionName=SectionName) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      This%NbTransformParams = InputSection%GetNumberOfSubSections()
      nullify(InputSection)

      allocate(This%ParamTransform(This%NbTransformParams), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

      allocate(This%ParamTransformLabel(This%NbTransformParams), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamTransformLabel', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, This%NbTransformParams
        SubSectionName = SectionName // '>parameter' // ConvertToString(i)
        ParameterName = 'label'
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
        This%ParamTransformLabel(i) = VarC0D

        ParameterName = 'transform'
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        call This%ParamTransform(i)%Set( Values=VarC1D )
      end do

      i = 1
      do i = 1, This%NbTransformParams
        ii = 1
        do ii = i + 1 , This%NbTransformParams
          if ( This%ParamTransformLabel(i)%GetValue() == This%ParamTransformLabel(ii)%GetValue() ) call Error%Raise(              &
                      Line='Duplicate transform parameter labels : ' // This%ParamTransformLabel(i)%GetValue(), ProcName=ProcName)
        end do
      end do

    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase( This, CaseDir, Prefix, Debug )

    use String_Library

    class(PCESM_Type), intent(inout)                                  ::    This
    character(*), intent(in)                                          ::    CaseDir
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FileName
    type(InputReader_Type)                                            ::    Input

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    FileName = PrefixLoc // CaseDir // '/input/input.dat'
    call Input%Read( FileName=FileName )

    call This%Construct( Input=Input, Prefix= PrefixLoc // CaseDir )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(PCESM_Type), intent(in)                                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    DirectorySub = DirectoryLoc
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( DirectoryLoc /= '<undefined>' ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    i = 1
    do i = 1, This%NbCells
      SectionName = 'model' // Convert_To_String(i)
      call GetInput%AddSection( Section=This%Cell(i)%GetInput(MainSectionName=SectionName) )
    end do

    if ( This%NbFixedParams > 0 ) then
      SectionName = 'fixed_parameters'
      call GetInput%AddSection( SectionName=SectionName )

      i = 1
      do i = 1, This%NbFixedParams
        SubSectionName ='parameter' // ConvertToString(i)
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )

        SubSectionName = SectionName // '>parameter' // ConvertToString(i)
        call GetInput%AddParameter(Name='value', Value=ConvertToString(Value=This%FixedParamVals(i)), SectionName=SubSectionName )
        call GetInput%AddParameter(Name='label', Value=This%FixedParamLabels(i)%GetValue(), SectionName=SubSectionName )
      end do

    end if

    if ( This%NbTransformParams > 0 ) then
      SectionName = 'transformed_parameters'
      call GetInput%AddSection( SectionName=SectionName )

      i = 1
      do i = 1, This%NbTransformParams
        SubSectionName ='parameter' // ConvertToString(i)
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )

        SubSectionName = SectionName // '>parameter' // ConvertToString(i)
        call GetInput%AddParameter(Name='transform', Value=ConvertToString(Values=This%ParamTransform(i)%Values, Separator=' ' ),  &
                                                                                                      SectionName=SubSectionName )
        call GetInput%AddParameter(Name='label', Value=This%ParamTransformLabel(i)%GetValue(), SectionName=SubSectionName )
      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunCase1( This, Input, Output, Stat, Debug )

    class(PCESM_Type), intent(inout)                                  ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    integer                                                           ::    NbOutputsLoc
    class(Input_Type), allocatable                                    ::    InputLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(This%NbOutputs), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= This%NbOutputs ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(This%NbOutputs), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if ( This%NbFixedParams > 0 .or. This%NbTransformParams > 0 ) then
      allocate(InputLoc, source=Input, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='InputLoc', ProcName=ProcName, stat=StatLoc )
      select type (InputLoc)
        type is ( InputDet_Type )
          if ( This%NbFixedParams > 0 ) call InputLoc%Append( Values=This%FixedParamVals, Labels=This%FixedParamLabels )
        type is ( InputStoch_Type )
          if ( This%NbFixedParams > 0 ) call InputLoc%Append( Values=This%FixedParamVals, Labels=This%FixedParamLabels )
        class default
          call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
      end select
      if ( This%NbTransformParams > 0 ) then
        i = 1
        do i = 1, This%NbTransformParams
          call InputLoc%Transform( Transformations=This%ParamTransform(i)%Values, Label=This%ParamTransformLabel(i)%GetValue() )
        end do
      end if 
      ii = 0
      i = 1
      do i = 1, This%NbCells
        NbOutputsLoc = This%Cell(i)%GetNbOutputs()
        if ( NbOutputsLoc <= 0 ) cycle
        call This%Cell(i)%Run( Input=InputLoc, Output=Output(ii+1:ii+NbOutputsLoc) )
        ii = ii + NbOutputsLoc
      end do
      deallocate(InputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='InputLoc', ProcName=ProcName, stat=StatLoc )
    else
      ii = 0
      i = 1
      do i = 1, This%NbCells
        NbOutputsLoc = This%Cell(i)%GetNbOutputs()
        if ( NbOutputsLoc <= 0 ) cycle
        call This%Cell(i)%Run( Input=Input, Output=Output(ii+1:ii+NbOutputsLoc) )
        ii = ii + NbOutputsLoc
      end do
    end if

    if ( present(Stat) ) Stat = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(PCESM_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (PCESM_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbOutputs = RHS%NbOutputs
          LHS%NbCells = RHS%NbCells
          LHS%NbTransformParams = RHS%NbTransformParams
          allocate(LHS%Cell(RHS%NbCells), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='Cell', ProcName=ProcName, stat=StatLoc )
          LHS%NbFixedParams = RHS%NbFixedParams
          if ( RHS%NbFixedParams > 0 ) then
            allocate(LHS%FixedParamVals, source=RHS%FixedParamVals, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%FixedParamVals', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%FixedParamLabels(RHS%NbFixedParams), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%FixedParamLabels', ProcName=ProcName, stat=StatLoc )
            i = 1
            do i = 1, RHS%NbFixedParams
              LHS%FixedParamLabels(i) = RHS%FixedParamLabels(i)%GetValue()
            end do
          end if
          if ( RHS%NbTransformParams > 0 ) then
            allocate(LHS%ParamTransform, source=RHS%ParamTransform, stat=StatLoc)
            allocate(LHS%ParamTransformLabel(RHS%NbTransformParams), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamTransformLabel', ProcName=ProcName, stat=StatLoc )
            i = 1
            do i = 1, RHS%NbTransformParams
              LHS%ParamTransformLabel(i) = RHS%ParamTransformLabel(i)%GetValue()
            end do
          end if
          i = 1
          do i = 1, RHS%NbCells
            LHS%Cell(i) = RHS%Cell(i)
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

    type(PCESM_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%Cell) ) deallocate(This%Cell, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamLabels) ) deallocate(This%FixedParamLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamVals) ) deallocate(This%FixedParamVals, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransform) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransformLabel) ) deallocate(This%ParamTransformLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransformLabel', ProcName=ProcName, stat=StatLoc )

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

    call This%PCEModel%Reset()

    This%NbInputs = 0
    This%NbOutputs = 0

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

    This%Directory = ''

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
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    NbOutputs
    integer                                                           ::    NbMaps
    integer                                                           ::    i
    character(:), allocatable, dimension(:)                           ::    LabelMap

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'directory'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Directory = VarC0D

    call This%PCEModel%Construct( Prefix=PrefixLoc // VarC0D )

    This%NbInputs = This%PCEModel%GetNbInputs()
    
    SectionName = 'input_label_map'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      i = 1
      do i = 1, Input%GetNumberofParameters(FromSubSection=SectionName)
        ParameterName = 'map' // ConvertToString(Value=i)
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
        call Parse( Input=VarC0D, Separator=' ', Output=LabelMap )
        if ( size(LabelMap,1) /= 2 ) call Error%Raise( Line='Incorrect input label map format', ProcName=ProcName )
        call This%PCEModel%ReplaceInputLabel( OldLabel=trim(adjustl(LabelMap(1))),  NewLabel=trim(adjustl(LabelMap(2))) )
      end do
    end if

    This%NbOutputs = This%PCEModel%GetNbOutputs()

    SectionName = 'output_label_map'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      i = 1
      do i = 1, Input%GetNumberofParameters(FromSubSection=SectionName)
        ParameterName = 'map' // ConvertToString(Value=i)
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
        call Parse( Input=VarC0D, Separator=' ', Output=LabelMap )
        if ( size(LabelMap,1) /= 2 ) call Error%Raise( Line='Incorrect input label map format', ProcName=ProcName )
        call This%PCEModel%ReplaceOutputLabel( OldLabel=trim(adjustl(LabelMap(1))),  NewLabel=trim(adjustl(LabelMap(2))) )
      end do
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory, Debug )

    use String_Library
    use StringRoutines_Module

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
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    DirectorySub = DirectoryLoc
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( DirectoryLoc /= '<undefined>' ) ExternalFlag = .true.

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Cell%AddParameter( Name='directory', Value=This%Directory )
    
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_Cell( This, Input, Output, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call This%PCEModel%RunPredet( Input=Input, Output=Output )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs_Cell( This, Debug )

    integer                                                           ::    GetNbOutputs_Cell

    class(Cell_Type), intent(in)                                      ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbOutputs_Cell'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbOutputs_Cell = This%NbOutputs

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_Cell( LHS, RHS )

    class(Cell_Type), intent(out)                                     ::    LHS
    class(Cell_Type), intent(in)                                      ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy_Cell'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%Directory = RHS%Directory
      LHS%NbOutputs = RHS%NbOutputs
      LHS%NbInputs = RHS%NbInputs
      LHS%PCEModel = RHS%PCEModel
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
  
    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
