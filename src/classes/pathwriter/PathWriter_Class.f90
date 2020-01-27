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

module PathWriter_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use String_Library
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type

implicit none

private

public                                                                ::    PathWriter_Type

type                                                                  ::    FileProcessor_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(SMUQFile_Type)                                                 ::    PathFile
  integer                                                             ::    NbPaths=0
  character(:), allocatable                                           ::    AbsolutePrefix
  logical, allocatable, dimension(:)                                  ::    PrependAbsolute
  type(String_Type), allocatable, dimension(:)                        ::    Path
  type(String_Type), allocatable, dimension(:)                        ::    Identifier
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_FP
  procedure, public                                                   ::    Reset                   =>    Reset_FP
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_FP
  generic, public                                                     ::    Construct               =>    ConstructInput_FP
  procedure, private                                                  ::    ConstructInput_FP
  procedure, public                                                   ::    GetInput                =>    GetInput_FP
  procedure, public                                                   ::    WritePaths              =>    WritePaths_FP
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_FP
  final                                                               ::    Finalizer_FP
end type

type                                                                  ::    PathWriter_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbFiles
  type(FileProcessor_Type), allocatable, dimension(:)                 ::    FileProcessors
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WritePaths
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(PathWriter_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'PathWriter'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(PathWriter_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessors) ) deallocate(This%FileProcessors, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

    This%NbFiles = 0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(PathWriter_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library
    use StringRoutines_Module
    class(PathWriter_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i
    

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    SectionName = 'files'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbFiles = InputSection%GetNumberofSubSections()

    allocate(This%FileProcessors(This%NbFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = SectionName // '>file' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%FileProcessors(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(PathWriter_Type), intent(in)                           ::    This
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
    
    SectionName = 'files'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = 'file' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%FileProcessors(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,         &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WritePaths( This )

    class(PathWriter_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='WritePaths'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbFiles
      call This%FileProcessors(i)%WritePaths()
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(PathWriter_Type), intent(out)                          ::    LHS
    class(PathWriter_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (PathWriter_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbFiles = RHS%NbFiles
          allocate(LHS%FileProcessors, source=RHS%FileProcessors, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%FileProcessors', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(PathWriter_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessors) ) deallocate(This%FileProcessors, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize_FP'

    if ( .not. This%Initialized ) then
      This%Name = 'FileProcessor'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset_FP'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%PrependAbsolute) ) deallocate(This%PrependAbsolute, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PrependAbsolute', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Path) ) deallocate(This%Path, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Path', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Identifier) ) deallocate(This%Identifier, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Identifier', ProcName=ProcName, stat=StatLoc )

    This%NbPaths = 0

    call This%PathFile%Reset()

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_FP'

    This%AbsolutePrefix = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_FP( This, Input, Prefix )

    class(FileProcessor_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput_FP'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'file'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%PathFile%Construct( Input=InputSection, Prefix=PrefixLoc )

    This%AbsolutePrefix = PrefixLoc

    SectionName = 'paths'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbPaths = InputSection%GetNumberofSubSections()
    nullify(InputSection)

    allocate(This%PrependAbsolute(This%NbPaths), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PrependAbsolute', ProcName=ProcName, stat=StatLoc )
    This%PrependAbsolute = .true.

    allocate(This%Identifier(This%NbPaths), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Identifier', ProcName=ProcName, stat=StatLoc )
    This%Identifier = ''

    allocate(This%Path(This%NbPaths), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Path', ProcName=ProcName, stat=StatLoc )
    This%Path = ''

    i = 1
    do i = 1, This%NbPaths
      SubSectionName = SectionName // '>path' // ConvertToString(Value=i)
  
      ParameterName = 'path'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Path(i) = VarC0D

      ParameterName = 'identifier'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Identifier(i) = VarC0D

      ParameterName = 'prepend_absolute'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%PrependAbsolute(i) = VarL0D
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_FP( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput_FP

    class(FileProcessor_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput_FP'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    type(InputSection_Type), pointer                                  ::    InputSection=>null()


    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput_FP%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    SectionName = 'file'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file'
    call GetInput_FP%AddSection( Section=This%PathFile%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                    &
                                                                                                         Directory=DirectorySub) )

    SectionName = 'paths'
    call GetInput_FP%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbPaths
      SubSectionName = 'path' // ConvertToString(Value=i)
      call GetInput_FP%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      call GetInput_FP%AddParameter( Name='path', Value=This%Path(i)%GetValue(), SectionName=SectionName // '>' //                &
                                                                                                                  SubSectionName )
      call GetInput_FP%AddParameter( Name='identifier', Value=This%Identifier(i)%GetValue(), SectionName=SectionName // '>' //    &
                                                                                                                  SubSectionName )
      call GetInput_FP%AddParameter( Name='prepend_absolute', Value=ConvertToString(Value=This%PrependAbsolute(i)),               &
                                                                                SectionName=SectionName // '>' // SubSectionName )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WritePaths_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    type(String_Type), allocatable, dimension(:)                      ::    Transcript
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbLines
    type(LinkedList0D_Type), allocatable, dimension(:)                ::    LineLog
    integer                                                           ::    IndexLoc
    character(:), allocatable                                         ::    Line

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(LineLog(This%NbPaths), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LineLog', ProcName=ProcName, stat=StatLoc )

    call This%PathFile%Import( Strings=Transcript, Mandatory=.true. )

    NbLines = size(Transcript,1)

    i = 1
    do i = 1, This%NbPaths
      VarC0D = '{' // This%Identifier(i)%GetValue() // '}'

      ii = 1
      do ii = 1, NbLines
        if ( index(Transcript(ii)%GetValue(), VarC0D) /= 0 ) call LineLog(i)%Append( Value=ii )
      end do

      if ( LineLog(i)%GetLength() < 1 ) call Error%Raise( Line='Could not find path indicator : ' //                              &
                                                                                 This%Identifier(i)%GetValue(), ProcName=ProcName)
    end do

    IndexLoc = 0
    i = 1
    do i = 1, This%NbPaths
      ii = 1
      do ii = 1, LineLog(i)%GetLength()
        call LineLog(i)%Get( Node=ii, Value=IndexLoc )
        Line = This%Path(i)%GetValue()
        if ( This%PrependAbsolute(i) ) Line = This%AbsolutePrefix // Line
        VarC0D = ReplaceCharacter( String=Transcript(IndexLoc)%GetValue(), Old='{' // This%Identifier(i)%GetValue() // '}' , &
                 New=Line )
        Transcript(IndexLoc) = VarC0D
      end do
    end do

    call This%PathFile%Export( Strings=Transcript )

    deallocate(Transcript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Transcript', ProcName=ProcName, stat=StatLoc )

    deallocate(LineLog, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LineLog', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_FP( LHS, RHS )

    class(FileProcessor_Type), intent(out)                            ::    LHS
    class(FileProcessor_Type), intent(in)                             ::    RHS

    character(*), parameter                                           ::    ProcName='Copy_FP'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (FileProcessor_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%PathFile = RHS%PathFile
          LHS%NbPaths = RHS%NbPaths
          LHS%AbsolutePrefix = RHS%AbsolutePrefix
          allocate(LHS%Path, source=RHS%Path, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Path', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Identifier, source=RHS%Identifier, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Identifier', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%PrependAbsolute, source=RHS%PrependAbsolute, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PrependAbsolute', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_FP( This )

    type(FileProcessor_Type),intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_FP'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%PrependAbsolute) ) deallocate(This%PrependAbsolute, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PrependAbsolute', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Path) ) deallocate(This%Path, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Path', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Identifier) ) deallocate(This%Identifier, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Identifier', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
