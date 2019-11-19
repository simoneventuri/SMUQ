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

module MFileMixed_Class

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
use MFileScalar_Class                                             ,only:    MFileScalar_Type
use MFileTable_Class                                              ,only:    MFileTable_Type
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    MFileMixed_Type

type, extends(MFileInput_Type)                                        ::    MFileMixed_Type
  type(String_Type), allocatable, dimension(:)                        ::    TemplateTranscript
  type(MFileScalar_Type)                                              ::    ScalarFile
  type(MFileTable_Type), allocatable, dimension(:)                    ::    TableFile
  logical                                                             ::    Scalar=.false.
  integer                                                             ::    NbTables
  integer, dimension(:), allocatable                                  ::    TableStart
  integer, dimension(:), allocatable                                  ::    TableEnd
  type(String_Type), allocatable, dimension(:)                        ::    TableIdentifier
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
  subroutine Initialize( This, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'mfilemixed'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TableFile) ) deallocate(This%TableFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableFile', ProcName=ProcName, stat=StatLoc )

    This%NbTables = 0

    if ( allocated(This%TableStart) ) deallocate(This%TableStart, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableStart', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TableEnd) ) deallocate(This%TableEnd, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableEnd', ProcName=ProcName, stat=StatLoc )

    call This%ScalarFile%Reset()

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Scalar = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
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
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Identifier
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i, ii, iii
    integer                                                           ::    NbLines
    type(SMUQFile_Type)                                               ::    MTemplateFile

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'template'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportFile( Strings=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)
    NbLines = size(This%TemplateTranscript,1)

    SectionName = 'scalars'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found ) then
      call This%ScalarFile%Construct( Template=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc )
    else
      This%Scalar = .false.
    end if

    SectionName = 'tables'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true., FoundSection=Found)
    This%NbTables = InputSection%GetNumberofSubSections()

    allocate(This%TableFile(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableFile', ProcName=ProcName, stat=StatLoc )

    allocate(This%TableStart(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableStart', ProcName=ProcName, stat=StatLoc )
    This%TableStart = 0

    allocate(This%TableEnd(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableEnd', ProcName=ProcName, stat=StatLoc )
    This%TableStart = 0

    allocate(This%TableIdentifier(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableIdentifier', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbTables
      SubSectionName = SectionName // '>table' // ConvertToString(Value=i)
      ParameterName = 'identifier'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      call This%TableIdentifier(i)%Set_Value(Value=VarC0D)
      SubSectionName =SubSectionName // '>table'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )

      Identifier = '{' // This%TableIdentifier(i)%GetValue() // '}'

      ii = 1
      iii = 0
      do ii = 1, NbLines
        if ( trim(adjustl(This%TemplateTranscript(ii)%GetValue())) == Identifier ) then
          iii = iii + 1
          if ( iii > 2 ) call Error%Raise( Line='Detected more than one of one of the specified tables', ProcName=ProcName )
          if ( iii == 1 ) This%TableStart(i) = ii
          if ( iii == 2 ) This%TableEnd(i) = ii
        end if
      end do
      if ( iii == 0 ) call Error%Raise( Line='Could not find any identifier for one of the tables', ProcName=ProcName )
      if ( iii == 1 ) call Error%Raise( Line='Could not find a start or end identifier for one of the tables', ProcName=ProcName )
      call This%TableFile(i)%Construct(This%TemplateTranscript(This%TableStart(i)+1:This%TableEnd(i)-1), Input=InputSection,      &
                                                                                                                Prefix=PrefixLoc )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2( This, Template, Input, Prefix, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    type(String_Type), dimension(:), intent(in)                       ::    Template
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput2'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Identifier
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i, ii, iii
    integer                                                           ::    NbLines

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    allocate(This%TemplateTranscript, source=Template, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )
    NbLines = size(This%TemplateTranscript,1)

    SectionName = 'scalars'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found ) then
      call This%ScalarFile%Construct( Template=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc )
    else
      This%Scalar = .false.
    end if

    SectionName = 'tables'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true., FoundSection=Found)
    This%NbTables = InputSection%GetNumberofSubSections()

    allocate(This%TableFile(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableFile', ProcName=ProcName, stat=StatLoc )

    allocate(This%TableStart(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableStart', ProcName=ProcName, stat=StatLoc )
    This%TableStart = 0

    allocate(This%TableEnd(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableEnd', ProcName=ProcName, stat=StatLoc )
    This%TableStart = 0

    allocate(This%TableIdentifier(This%NbTables), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TableIdentifier', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbTables
      SubSectionName = SectionName // '>table' // ConvertToString(Value=i)
      ParameterName = 'identifier'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      call This%TableIdentifier(i)%Set_Value(Value=VarC0D)
      SubSectionName =SubSectionName // '>table'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )

      Identifier = '{' // This%TableIdentifier(i)%GetValue() // '}'

      ii = 1
      iii = 0
      do ii = 1, NbLines
        if ( trim(adjustl(This%TemplateTranscript(ii)%GetValue())) == Identifier ) then
          iii = iii + 1
          if ( iii > 2 ) call Error%Raise( Line='Detected more than one of one of the specified tables', ProcName=ProcName )
          if ( iii == 1 ) This%TableStart(i) = ii
          if ( iii == 2 ) This%TableEnd(i) = ii
        end if
      end do
      if ( iii == 0 ) call Error%Raise( Line='Could not find any identifier for one of the tables', ProcName=ProcName )
      if ( iii == 1 ) call Error%Raise( Line='Could not find a start or end identifier for one of the tables', ProcName=ProcName )
      call This%TableFile(i)%Construct(This%TemplateTranscript(This%TableStart(i)+1:This%TableEnd(i)-1), Input=InputSection,      &
                                                                                                                Prefix=PrefixLoc )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(MFileMixed_Type), intent(inout)                             ::    This
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
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SubSubSectionName
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

    call GetInput%SetName( SectionName=trim(adjustl(MainSectionName)) )

    SectionName = 'template'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ExportFile( Input=InputSection, Strings=This%TemplateTranscript )
    nullify(InputSection)

    SectionName = 'scalars'
    if ( This%Scalar ) then
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/scalars'
      call GetInput%AddSection( Section=This%ScalarFile%GetInput( MainSectionName=SectionName, Prefix=PrefixLoc,                  &
                                                                                                          Directory=DirectorySub))
    else
      continue
    end if
    
    SectionName = 'tables'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbTables
      SubSectionName = 'table' // ConvertToString(Value=i)
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      call GetInput%AddParameter( Name='identifier', Value=This%TableIdentifier(i)%GetValue(),                                    &
                                                                              SectionName = SectionName // '>' // SubSectionName )
      SubSectionName = SectionName // '>' // SubSectionName
      SubSubSectionName = 'table'
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/table' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%TableFile(i)%GetInput( MainSectionName=SubSubSectionName, Prefix=PrefixLoc,          &
                                                                            Directory=DirectorySub), To_SubSection=SubSectionName)
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
 subroutine WriteInput( This, Input, Strings, Debug )

    class(MFileMixed_Type), intent(inout)                             ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    type(String_Type), allocatable, dimension(:)                      ::    TemplateLoc
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i, ii, iii
    type(String_Type), allocatable, dimension(:)                      ::    VarString1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(TemplateLoc, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TemplateLoc', ProcName=ProcName, stat=StatLoc )

    NbLines = size(TemplateLoc,1)

    if ( This%Scalar ) then
      call This%ScalarFile%WriteInput( Input=Input, Strings=TemplateLoc )
    else
      continue
    end if

    i = 1
    do i = 1, This%NbTables
      call This%TableFile(i)%WriteInput( Input=Input, Strings=VarString1D )
      ii = 1
      iii = 0
      do ii = This%TableStart(i)+1, This%TableEnd(i)-1
        iii = iii + 1
        TemplateLoc(ii) = VarString1D(iii)
      end do
      deallocate(VarString1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarString1D', ProcName=ProcName, stat=StatLoc )
    end do

    allocate(Strings(NbLines-2*size(This%TableStart,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

    i = 1
    ii = 0
    do i = 1, NbLines
      if ( any(i == This%TableStart .or. i == This%TableEnd) ) cycle
      ii = ii + 1
      Strings(ii) = TemplateLoc(i)
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(MFileMixed_Type), intent(out)                               ::    LHS
    class(MFileInput_Type), intent(in)                                ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (MFileMixed_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Scalar = RHS%Scalar
          LHS%NbTables = RHS%NbTables
          if ( LHS%Scalar ) LHS%ScalarFile = RHS%ScalarFile
          allocate(LHS%TableFile, source=RHS%TableFile, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TableFile', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%TableStart, source=RHS%TableStart, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TableStart', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%TableEnd, source=RHS%TableEnd, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TableEnd', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%TemplateTranscript, source=RHS%TemplateTranscript, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TemplateTranscript', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(MFileMixed_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )   

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TableFile) ) deallocate(This%TableFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableFile', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TableStart) ) deallocate(This%TableStart, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableStart', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TableEnd) ) deallocate(This%TableEnd, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TableEnd', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
