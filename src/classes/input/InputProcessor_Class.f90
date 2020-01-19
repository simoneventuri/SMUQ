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

module InputProcessor_Class

use StringRoutines_Module
use String_Library
use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    InputProcessor_Type

type                                                                  ::    InputProcessor_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Initialized=.false.
  integer                                                             ::    NbFixedParams=0
  real(rkp), allocatable, dimension(:)                                ::    FixedParamVals
  type(String_Type), allocatable, dimension(:)                        ::    FixedParamLabels
  integer                                                             ::    NbTransformParams=0   
  type(String_Type), allocatable, dimension(:)                        ::    ParamTransform 
  type(String_Type), allocatable, dimension(:)                        ::    ParamTransformLabel
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    ProcessInput            =>    ProcessInput_0D,        &
                                                                                                          ProcessInput_1D
  procedure, private                                                  ::    ProcessInput_0D
  procedure, private                                                  ::    ProcessInput_1D
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(InputProcessor_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'InputProcessor'
      call This%SetDefaults()
    end if

   end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(InputProcessor_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%FixedParamLabels) ) deallocate(This%FixedParamLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamVals) ) deallocate(This%FixedParamVals, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransform) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransformLabel) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransform', ProcName=ProcName, stat=StatLoc )

    This%NbFixedParams = 0
    This%NbTransformParams = 0

    call This%Initialize()

   end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(InputProcessor_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

   end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(InputProcessor_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    integer                                                           ::    i
    integer                                                           ::    ii
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix


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
        This%ParamTransform(i) = VarC0D
      end do

      i = 1
      do i = 1, This%NbTransformParams
        ii = 1
        do ii = i + 1 , This%NbTransformParams
          if ( This%ParamTransformLabel(i)%GetValue() == This%ParamTransformLabel(ii)%GetValue() ) call Error%Raise(              &
            Line='Duplicate transform and fixed parameter labels : ' // This%ParamTransformLabel(i)%GetValue(), ProcName=ProcName)
        end do
      end do

    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(InputProcessor_Type), intent(in)                            ::    This
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

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

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
        call GetInput%AddParameter(Name='transform', Value=This%ParamTransform(i)%GetValue(), SectionName=SubSectionName )
        call GetInput%AddParameter(Name='label', Value=This%ParamTransformLabel(i)%GetValue(), SectionName=SubSectionName )
      end do

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ProcessInput_0D( This, Input, ProcessedInput )

    class(InputProcessor_Type), intent(in)                            ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Input_Type), intent(out)                                     ::    ProcessedInput

    character(*), parameter                                           ::    ProcName='ProcessInput_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    real(rkp)                                                         ::    VarR0D

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    ProcessedInput = Input
    if ( This%NbFixedParams > 0 .or. This%NbTransformParams > 0 ) then
      if ( This%NbFixedParams > 0 ) call ProcessedInput%Append( Values=This%FixedParamVals, Labels=This%FixedParamLabels )
      if ( This%NbTransformParams > 0 ) call ProcessedInput%Transform( Transformations=This%ParamTransform,                       &
                                                                                                 Labels=This%ParamTransformLabel )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ProcessInput_1D( This, Input, ProcessedInput )

    class(InputProcessor_Type), intent(in)                            ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Input_Type), allocatable, dimension(:), intent(out)          ::    ProcessedInput

    character(*), parameter                                           ::    ProcName='ProcessInput_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbInputs

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    NbInputs = size(Input,1)

    allocate(ProcessedInput(NbInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ProcessedInput', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbInputs
      call This%ProcessInput( Input=Input(i), ProcessedInput=ProcessedInput(i) )
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This )

    logical                                                           ::    IsConstructed

    class(InputProcessor_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='IsConstructed'

    IsConstructed = This%Constructed

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(InputProcessor_Type), intent(out)                           ::    LHS
    class(InputProcessor_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%NbTransformParams = RHS%NbTransformParams
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
        if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamTransform', ProcName=ProcName, stat=StatLoc )
        allocate(LHS%ParamTransformLabel(RHS%NbTransformParams), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamTransformLabel', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, RHS%NbTransformParams
          LHS%ParamTransformLabel(i) = RHS%ParamTransformLabel(i)%GetValue()
        end do
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(InputProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FixedParamLabels) ) deallocate(This%FixedParamLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%FixedParamVals) ) deallocate(This%FixedParamVals, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FixedParamVals', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransform) ) deallocate(This%ParamTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ParamTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamTransformLabel) ) deallocate(This%ParamTransformLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamTransformLabel', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
