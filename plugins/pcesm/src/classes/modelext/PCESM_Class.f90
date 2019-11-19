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

type, extends(ModelExtTemplate_Type)                                  ::    PCESM_Type
  type(PolyChaosModel_Type), allocatable, dimension(:)                ::    PCEModels
  integer                                                             ::    NbModels=0
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

    if ( allocated(This%PCEModels) ) deallocate(This%PCEModels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PCEModels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamLabels) ) deallocate(This%FixedParamLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamVals) ) deallocate(This%FixedParamVals, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransform) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransformLabel) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    This%NbModels = 0
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
    character(:), allocatable, dimension(:)                           ::    LabelMap

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'models'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbModels = InputSection%GetNumberOfSubSections()
    allocate(This%PCEModels(This%NbModels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cell', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbModels
      SubSectionName = SectionName // '>model' // ConvertToString(Value=i)
      ParameterName = 'directory'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found, SectionName=SubSectionName )
      if ( Found )
        call This%PCEModels(i)%Construct( Prefix=PrefixLoc // VarC0D )

        SubSectionName = SectionName // '>model' // ConvertToString(Value=i) // '>input_label_map'
        if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
          i = 1
          do i = 1, Input%GetNumberofParameters(FromSubSection=SubSectionName)
            ParameterName = 'map' // ConvertToString(Value=i)
            call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
            call Parse( Input=VarC0D, Separator=' ', Output=LabelMap )
            if ( size(LabelMap,1) /= 2 ) call Error%Raise( Line='Incorrect input label map format', ProcName=ProcName )
            call This%PCEModels(i)%ReplaceInputLabel( OldLabel=trim(adjustl(LabelMap(1))),  NewLabel=trim(adjustl(LabelMap(2))) )
          end do
        end if

        SectionName = SectionName // '>model' // ConvertToString(Value=i) // '>output_label_map'
        if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
          i = 1
          do i = 1, Input%GetNumberofParameters(FromSubSection=SubSectionName)
            ParameterName = 'map' // ConvertToString(Value=i)
            call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
            call Parse( Input=VarC0D, Separator=' ', Output=LabelMap )
            if ( size(LabelMap,1) /= 2 ) call Error%Raise( Line='Incorrect input label map format', ProcName=ProcName )
            call This%PCEModels(i)%ReplaceOutputLabel( OldLabel=trim(adjustl(LabelMap(1))),  NewLabel=trim(adjustl(LabelMap(2))) )
          end do
        end if
      else
        SubSectionName = SubSectionName // '>pce_input'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call This%PCEModels(i)%Construct( Input=Input, Prefix=PrefixLoc )
        nullify(InputSection)
      end if
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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    SectionName = 'models'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbModels
      SubSectionName = 'model' // ConvertToString(Value=i)
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      SubSectionName = SectionName // '>' // SubSectionName
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/model' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%PCEModels(i)%GetInput(MainSectionName='pce_input', Prefix=PrefixLoc,                 &
                                                                          Directory=DirectorySub ), To_SubSection=SubSectionName )
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
    class(Input_Type), allocatable                                    ::    InputLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(This%NbModels), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= This%NbModels ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(This%NbModels), stat=StatLoc )
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
      i = 1
      do i = 1, This%NbModels
        call This%PCEModels(i)%Run( Input=InputLoc, Output=Output(i) )
      end do
      deallocate(InputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='InputLoc', ProcName=ProcName, stat=StatLoc )
    else
      i = 1
      do i = 1, This%NbModels
        call This%PCEModels(i)%Run( Input=Input, Output=Output(i) )
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
          LHS%NbModels = RHS%NbModels
          allocate(LHS%PCEModels, source=RHS%PCEModels, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PCEModels', ProcName=ProcName, stat=StatLoc )
          LHS%NbTransformParams = RHS%NbTransformParams
          allocate(LHS%Cell(RHS%NbModels), stat=StatLoc)
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
          do i = 1, RHS%NbModels
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
  
    if ( allocated(This%PCEModels) ) deallocate(This%PCEModels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PCEModels', ProcName=ProcName, stat=StatLoc )

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

end module
