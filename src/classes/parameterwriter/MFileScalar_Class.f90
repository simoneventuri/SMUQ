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

module MFileScalar_Class

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use MFileInput_Class                                              ,only:    MFileInput_Type
use MParamScalar_Class                                            ,only:    MParamScalar_Type
use MParamScalar_Vec_Class                                        ,only:    MParamScalar_Vec_Type
use MParamScalar_Factory_Class                                    ,only:    MParamScalar_Factory
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    MFileScalar_Type

type, extends(MFileInput_Type)                                        ::    MFileScalar_Type
  type(String_Type), allocatable, dimension(:)                        ::    TemplateTranscript
  type(MParamScalar_Vec_Type), allocatable, dimension(:)              ::    MParam
  integer                                                             ::    NbMParams=0
  type(LinkedList0D_Type), allocatable, dimension(:)                  ::    LineLog
  type(String_Type), allocatable, dimension(:)                        ::    ParamIdentifier
  type(String_Type), allocatable, dimension(:)                        ::    ParamFormat
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput2
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructInput2
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WriteInput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(MFileScalar_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'mfilescalar'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(MFileScalar_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    if ( allocated(This%MParam) ) deallocate(This%MParam, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    This%NbMParams = 0

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%LineLog) ) deallocate(This%LineLog, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LineLog', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamIdentifier) ) deallocate(This%ParamIdentifier, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamIdentifier', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamFormat) ) deallocate(This%ParamFormat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(MFileScalar_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use StringRoutines_Module
    use ArrayRoutines_Module

    class(MFileScalar_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamScalar_Type), allocatable                             ::    MParam
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i, ii
    integer                                                           ::    NbLines=0
    type(SMUQFile_Type)                                               ::    MTemplateFile

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'template'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportFile( Strings=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)

    NbLines = size(This%TemplateTranscript,1)

    SectionName = 'params'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbMParams = InputSection%GetNumberofSubSections()

    allocate(This%LineLog(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%LineLog', ProcName=ProcName, stat=StatLoc )

    allocate(This%MParam(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamFormat(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )
    
    allocate(This%ParamIdentifier(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamIdentifier', ProcName=ProcName, stat=StatLoc )

    ParameterName = 'format'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( .not. Found ) VarC0D = 'G0'
    i = 1
    do i = 1, This%NbMParams
      call This%ParamFormat(i)%Set_Value(Value=VarC0D)
    end do

    i = 1
    do i = 1, This%NbMParams
      SubSectionName = SectionName // '>param' // ConvertToString(Value=i)

      ParameterName = 'format'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) call This%ParamFormat(i)%Set_Value(Value=VarC0D)

      ParameterName = 'identifier'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.true. )
      call This%ParamIdentifier(i)%Set_Value(Value=VarC0D)

      VarC0D = '{' // This%ParamIdentifier(i)%Getvalue() // '}'

      ii = 1
      do ii = 1, NbLines
        if ( This%TemplateTranscript(ii)%Is_Substring_Present( SubStr=VarC0D ) ) call This%LineLog(i)%Append( Value=ii )
      end do

      if ( This%LineLog(i)%GetLength() < 1 ) call Error%Raise( Line='Could not find indicator in template:' //                    &
                                                                            This%ParamIdentifier(i)%GetValue(), ProcName=ProcName)

      SubSectionName = SubSectionName // '>param'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MParamScalar_Factory%Construct( Object=MParam, Input=InputSection, Prefix=PrefixLoc )
      call This%MParam(i)%Set( Object=MParam )
      nullify(InputSection)

      deallocate(MParam, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='MParam', ProcName=ProcName, stat=StatLoc )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2( This, Template, Input, Prefix )

    use StringRoutines_Module

    class(MFileScalar_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    type(String_Type), dimension(:), intent(in)                       ::    Template

    character(*), parameter                                           ::    ProcName='ConstructInput2'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamScalar_Type), allocatable                             ::    MParam
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i, ii
    integer                                                           ::    NbLines=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    allocate(This%TemplateTranscript, source=Template, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    NbLines = size(This%TemplateTranscript,1)

    SectionName = 'params'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbMParams = InputSection%GetNumberofSubSections()

    allocate(This%LineLog(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%LineLog', ProcName=ProcName, stat=StatLoc )

    allocate(This%MParam(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamFormat(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )
    
    allocate(This%ParamIdentifier(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamIdentifier', ProcName=ProcName, stat=StatLoc )
    
    ParameterName = 'format'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( .not. Found ) VarC0D = 'G0'
    i = 1
    do i = 1, This%NbMParams
      This%ParamFormat(i) = VarC0D
    end do

    i = 1
    do i = 1, This%NbMParams
      SubSectionName = SectionName // '>param' // ConvertToString(Value=i)

      ParameterName = 'format'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) call This%ParamFormat(i)%Set_Value(Value=VarC0D)

      ParameterName = 'identifier'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.true. )
      call This%ParamIdentifier(i)%Set_Value(Value=VarC0D)

      VarC0D = '{' // This%ParamIdentifier(i)%Getvalue() // '}'

      ii = 1
      do ii = 1, NbLines
        if ( This%TemplateTranscript(i)%Is_Substring_Present( SubStr=VarC0D ) ) call This%LineLog(i)%Append( Value=ii )
      end do

      if ( This%LineLog(i)%GetLength() < 1 ) call Error%Raise( Line='Could not find indicator in template:' //                    &
                                                                            This%ParamIdentifier(i)%GetValue(), ProcName=ProcName)

      SubSectionName = SubSectionName // '>param'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MParamScalar_Factory%Construct( Object=MParam, Input=InputSection, Prefix=PrefixLoc )
      call This%MParam(i)%Set( Object=MParam )
      nullify(InputSection)

      deallocate(MParam, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='MParam', ProcName=ProcName, stat=StatLoc )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(MFileScalar_Type), intent(inout)                            ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamScalar_Type), pointer                                 ::    MParam=>null()
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

    call GetInput%SetName( SectionName=trim(adjustl(MainSectionName)) )

    SectionName = 'template'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ExportFile( Input=InputSection, Strings=This%TemplateTranscript )
    nullify(InputSection)

    SectionName = 'params'
    call GetInput%AddSection( SectionName=SectionName)

    i = 1
    do i = 1, This%NbMParams
      call GetInput%AddSection( SectionName='param' // ConvertToString(Value=i), To_SubSection=SectionName )
      SubSectionName = SectionName // '>param' // ConvertToString(Value=i)

      call GetInput%AddParameter(Name='format', Value=This%ParamFormat(i)%GetValue(), SectionName=SubSectionName )
      call GetInput%AddParameter(Name='identifier', Value=This%ParamIdentifier(i)%GetValue(), SectionName=SubSectionName )

      MParam => This%MParam(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/param' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=MParamScalar_Factory%GetObjectInput(MainSectionName='param', Object=MParam,               &
                                                         Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SubSectionName )
      nullify(MParam)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput( This, Input, Strings )

    class(MFileScalar_Type), intent(inout)                            ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings

    character(*), parameter                                           ::    ProcName='WriteParameters'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    VarC0D
    class(MParamScalar_Type), pointer                                 ::    MParamPointer=>null()
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    IndexLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(Strings, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbMParams
      MParamPointer => This%MParam(i)%GetPointer()       
      ii = 1
      do ii = 1, This%LineLog(i)%GetLength()
        call This%LineLog(i)%Get( Node=ii, Value=IndexLoc )
        VarC0D = ReplaceCharacter( String=Strings(IndexLoc)%GetValue(),                                                           &
                 Old='{' // This%ParamIdentifier(i)%GetValue() // '}' ,                                                           &
                 New=MParamPointer%GetCharValue(Input=Input, Format=This%ParamFormat(i)%GetValue()) )
        call Strings(IndexLoc)%Set_Value( Value=VarC0D )
      end do
      nullify(MParamPointer)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(MFileScalar_Type), intent(out)                              ::    LHS
    class(MFileInput_Type), intent(in)                                ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (MFileScalar_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbMParams = RHS%NbMParams
          allocate(LHS%MParam, source=RHS%MParam, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MParam', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%TemplateTranscript, source=RHS%TemplateTranscript, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TemplateTranscript', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%LineLog, source=RHS%LineLog, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%LineLog', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ParamIdentifier, source=RHS%ParamIdentifier, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamIdentifier', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ParamFormat, source=RHS%ParamFormat, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamFormat', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(MFileScalar_Type),intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%MParam) ) deallocate(This%MParam, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%LineLog) ) deallocate(This%LineLog, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LineLog', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamIdentifier) ) deallocate(This%ParamIdentifier, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamIdentifier', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamFormat) ) deallocate(This%ParamFormat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
