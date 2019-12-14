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

module MFileTable_Class

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use MFileInput_Class                                              ,only:    MFileInput_Type
use MParamTable_Class                                             ,only:    MParamTable_Type
use MParamTableContainer_Class                                    ,only:    MParamTableContainer_Type
use MParamTable_Factory_Class                                     ,only:    MParamTable_Factory
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use InputDet_Class                                                ,only:    InputDet_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type

implicit none

private

public                                                                ::    MFileTable_Type

type, extends(MFileInput_Type)                                        ::    MFileTable_Type
  type(MParamTableContainer_Type), allocatable, dimension(:)          ::    MParam
  integer                                                             ::    NbMParams=0
  character(:), allocatable                                           ::    Separator
  character(:), allocatable                                           ::    Comment
  integer                                                             ::    NbLinesSkip=0
  integer                                                             ::    AbscissaColumn=1
  type(LinkedList0D_Type), allocatable, dimension(:)                  ::    ParamColumn
  type(String_Type), allocatable, dimension(:)                        ::    ParamFormat
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WriteInput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(MFileTable_Type), intent(inout)                             ::    This
    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'mfiletable'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(MFileTable_Type), intent(inout)                             ::    This
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    if ( allocated(This%MParam) ) deallocate(This%MParam, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    This%NbMParams = 0

    if ( allocated(This%ParamColumn) ) deallocate(This%ParamColumn, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamFormat) ) deallocate(This%ParamFormat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(MFileTable_Type), intent(inout)                             ::    This
    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%NbLinesSkip = 0
    This%Separator = ' '
    This%Comment = '#'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(MFileTable_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamTable_Type), allocatable                              ::    MParam
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer, allocatable, dimension(:)                                ::    VarI1D
    logical                                                           ::    Found
    integer                                                           ::    i, ii
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbColumns=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'nb_lines_skip'
    call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%NbLinesSkip = VarI0D
      if ( This%NbLinesSkip > NbLines-1 ) call Error%Raise( Line='Specified too many number of lines to skip', ProcName=ProcName )   
    end if

    ParameterName = 'abscissa_column'
    call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%AbscissaColumn = VarI0D  

    SectionName = 'parameters'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbMParams = InputSection%GetNumberofSubSections()

    allocate(This%MParam(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamColumn(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamFormat(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )
    
    ParameterName = 'format'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( .not. Found ) VarC0D = 'G0'
    i = 1
    do i = 1, THis%NbMParams
      call This%ParamFormat(i)%Set_Value(Value=VarC0D)
    end do

    i = 1
    do i = 1, This%NbMParams
      SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)

      ParameterName = 'format'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) call This%ParamFormat(i)%Set_Value(Value=VarC0D)

      ParameterName = 'column'
      call Input%GetValue( Values=VarI1D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      if ( any(VarI1D <= 0) ) call Error%Raise( Line='Specified column 0 or less', ProcName=ProcName )
      call This%ParamColumn(i)%Append( Values=VarI1D )

      SubSectionName = SubSectionName // '>param'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MParamTable_Factory%Construct( Object=MParam, Input=InputSection, Prefix=PrefixLoc )
      nullify(InputSection)
      call This%MParam(i)%Set( Object=MParam )

      deallocate(MParam, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='MParam', ProcName=ProcName, stat=StatLoc )
    end do

    call ColumnRecord%Get( Values=VarI1D )
    if ( any(VarI1D == This%AbscissaColumn) ) call Error%Raise( Line='One of the parameter column specifications coincides with'  &
                                                                                        // ' abscissa column', ProcName=ProcName )
    VarI0D = size(VarI1D,1)
    i = 1
    do i = 1, VarI0D-1
      if ( any( VarI1D(i+1:VarI0D) == VarI1D(i) ) ) call Error%Raise( Line='Multiple parameters have same column specification',  &
                                                                                                               ProcName=ProcName )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2( This, Template, Comment, Separator, NbLinesSkip, Input, Prefix )

    class(MFileTable_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    type(String_Type), dimension(:), intent(in)                       ::    Template
    character(*), optional, intent(in)                                ::    Comment
    character(*), optional, intent(in)                                ::    Separator
    integer, optional, intent(in)                                     ::    NbLinesSkip
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput2'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamTable_Type), allocatable                              ::    MParam
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer, allocatable, dimension(:)                                ::    VarI1D
    type(LinkedList0D_Type)                                           ::    ColumnRecord
    logical                                                           ::    Found
    integer                                                           ::    i, ii
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbColumns=0
    integer                                                           ::    AbscissaLength=0
    type(String_Type), allocatable, dimension(:)                      ::    VarString1D

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    allocate(This%TemplateTranscript, source=Template, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    NbLines = size(This%TemplateTranscript,1)

    if (present(NbLinesSKip) ) This%NbLinesSKip = NbLinesSkip
    if ( This%NbLinesSkip > NbLines-1 ) call Error%Raise( Line='Specified too many number of lines to skip', ProcName=ProcName )

    AbscissaLength = 0
    i = 1
    do i = 1, NbLines
      if ( i <= This%NbLinesSkip ) cycle
      VarC0D = trim(adjustl(This%TemplateTranscript(i)%GetValue()))  
      if ( VarC0D(1:1) == This%Comment ) cycle
      call This%TemplateTranscript(i)%Parse(Strings=VarString1D, Separator=This%Separator )
      NbColumns = size(VarString1D,1)
      AbscissaLength = AbscissaLength + 1
    end do

    deallocate(VarString1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarString1D', ProcName=ProcName, stat=StatLoc )

    ParameterName = 'abscissa_column'
    call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%AbscissaColumn = VarI0D
      if ( This%AbscissaColumn > NbColumns ) call Error%Raise( Line='Specified abscissa column exceeds number columns',           &
                                                                                                               ProcName=ProcName )   
    end if

    allocate(This%Abscissa(AbscissaLength), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )
    This%Abscissa=Zero

    i = 1
    do i = 1, NbLines
      if ( i <= This%NbLinesSkip ) cycle
      VarC0D = trim(adjustl(This%TemplateTranscript(i)%GetValue()))  
      if ( VarC0D(1:1) == This%Comment ) cycle
      call This%TemplateTranscript(i)%Parse(Strings=VarString1D, Separator=This%Separator )
      This%Abscissa(i) = ConvertToReal( String=VarString1D(This%AbscissaColumn)%GetValue() )
    end do

    SectionName = 'params'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbMParams = InputSection%GetNumberofSubSections()
    if ( This%NbMParams > NbColumns-1 ) call Error%Raise( Line='Specified too many parameters for the file', ProcName=ProcName )

    allocate(This%MParam(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamColumn(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc )

    allocate(This%ParamFormat(This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )
    
    ParameterName = 'format'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( .not. Found ) VarC0D = 'G0'
    i = 1
    do i = 1, THis%NbMParams
      call This%ParamFormat(i)%Set_Value(Value=VarC0D)
    end do

    i = 1
    do i = 1, This%NbMParams
      SubSectionName = SectionName // '>param' // ConvertToString(Value=i)

      ParameterName = 'format'
      call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) call This%ParamFormat(i)%Set_Value(Value=VarC0D)

      ParameterName = 'column'
      call Input%GetValue( Values=VarI1D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      if ( any(VarI1D <= 0) ) call Error%Raise( Line='Specified column 0 or less', ProcName=ProcName )
      call This%ParamColumn(i)%Append( Values=VarI1D )
      call ColumnRecord%Append( Values=VarI1D )
      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

      SubSectionName = SubSectionName // '>param'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MParamTable_Factory%Construct( Object=MParam, Input=InputSection, Prefix=PrefixLoc )
      nullify(InputSection)
      call This%MParam(i)%Set( Object=MParam )

      deallocate(MParam, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='MParam', ProcName=ProcName, stat=StatLoc )
    end do

    call ColumnRecord%Get( Values=VarI1D )
    if ( any(VarI1D == This%AbscissaColumn) ) call Error%Raise( Line='One of the parameter column specifications coincides with'  &
                                                                                        // ' abscissa column', ProcName=ProcName )
    VarI0D = size(VarI1D,1)
    i = 1
    do i = 1, VarI0D-1
      if ( any( VarI1D(i+1:VarI0D) == VarI1D(i) ) ) call Error%Raise( Line='Multiple parameters have same column specification',  &
                                                                                                               ProcName=ProcName )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(MFileTable_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MParamTable_Type), pointer                                  ::    MParam=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    integer, allocatable, dimension(:)                                ::    VarI1D

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

    call GetInput%AddParameter( Name='abscissa_column', Value=ConvertToString(Value=This%AbscissaColumn) )
    
    call GetInput%AddParameter( Name='nb_lines_skip', Value=ConvertToString(Value=This%NbLinesSkip) )

    SectionName = 'params'
    call GetInput%AddSection( SectionName=SectionName)

    i = 1
    do i = 1, This%NbMParams
      SubSectionName = SectionName // '>param' // ConvertToString(Value=i)
      call GetInput%AddParameter(Name='format', Value=This%ParamFormat(i)%GetValue(), SectionName=SubSectionName )
      call This%ParamColumn(i)%Get( Values=VarI1D )
      call GetInput%AddParameter( Name='column', Value=ConvertToString(Values=VarI1D), SectionName=SubSectionName )
      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

      MParam => This%MParam(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/param' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=MParamTable_Factory%GetObjectInput(MainSectionName='param', Object=MParam,                &
                                                         Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SubSectionName )
      nullify(MParam)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput( This, Input, Strings )

    class(MFileTable_Type), intent(inout)                             ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    VarC0D
    class(MParamTable_Type), pointer                                  ::    MParamPointer=>null()
    integer                                                           ::    i, ii, iii, iv
    type(String_Type), allocatable, dimension(:)                      ::    VarString1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    type(String_Type), allocatable, dimension(:,:)                    ::    NewEntry

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(Strings, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

    NbLines = size(Strings,1)

    allocate(NewEntry(size(This%Abscissa,1),This%NbMParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='NewEntry', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbMParams
      MParamPointer => This%MParam(i)%GetPointer()
      NewEntry(:,i) = MParamPointer%GetCharValue(Input=Input, Abscissa=This%Abscissa, Format=This%ParamFormat(i)%GetValue() )
      nullify(MParamPointer)
    end do

    i = 1
    ii = 0
    do i = 1, NbLines
      if ( i <= This%NbLinesSkip ) cycle
      VarC0D = trim(adjustl(This%TemplateTranscript(i)%GetValue()))  
      if ( VarC0D(1:1) == This%Comment ) cycle
      ii = ii + 1
      call This%TemplateTranscript(i)%Parse(Strings=VarString1D, Separator=This%Separator )
      iii = 1
      do iii = 1, This%NbMParams
        call This%ParamColumn(iii)%Get( Values=VarI1D )
        iv = 1
        do iv = 1, size(VarI1D,1)
          VarC0D = ReplaceCharacter( String=Strings(i)%GetValue(), Old=VarString1D(VarI1D(iv))%GetValue(),                        &
                                                                                                 New=NewEntry(ii,iii)%GetValue() )
          call Strings(i)%Set_Value( Value=VarC0D )
        end do
        deallocate(VarI1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
      end do
    end do

    deallocate(NewEntry, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NewEntry', ProcName=ProcName, stat=StatLoc )

    deallocate(VarString1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarString1D', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(MFileTable_Type), intent(out)                               ::    LHS
    class(MFileInput_Type), intent(in)                                ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (MFileTable_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbMParams = RHS%NbMParams
          LHS%Separator = RHS%Separator
          LHS%Comment = RHS%Comment
          LHS%AbscissaColumn = RHS%AbscissaColumn
          LHS%NbLinesSkip = RHS%NbLinesSkip
          allocate(LHS%ParamColumn, source=RHS%ParamColumn, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParamColumn', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%MParam, source=RHS%MParam, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MParam', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%TemplateTranscript, source=RHS%TemplateTranscript, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TemplateTranscript', ProcName=ProcName, stat=StatLoc )
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

    type(MFileTable_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%MParam) ) deallocate(This%MParam, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MParam', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Abscissa) ) deallocate(This%Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )    
  
    if ( allocated(This%ParamColumn) ) deallocate(This%ParamColumn, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamFormat) ) deallocate(This%ParamFormat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
