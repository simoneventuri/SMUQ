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

module SMUQFile_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use String_Library

implicit none

private

public                                                                ::    SMUQFile_Type

type                                                                  ::    SMUQFile_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    File
  character(:), allocatable                                           ::    FullFile
  integer                                                             ::    Unit
  character(:), allocatable                                           ::    Comment
  character(:), allocatable                                           ::    Separator
  logical                                                             ::    FileOpened=.false.
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Import                  =>    ImportFile
  generic, public                                                     ::    Export                  =>    Export0D_CharString,    &
                                                                                                          Export0D_String,        &
                                                                                                          Export1D_Strings
  generic, public                                                     ::    Append                  =>    Append0D_CharString,    &
                                                                                                          Append0D_String,        &
                                                                                                          Append1D_Strings
  procedure, private                                                  ::    Export0D_CharString
  procedure, private                                                  ::    Export0D_String
  procedure, private                                                  ::    Export1D_Strings
  procedure, private                                                  ::    Append0D_CharString
  procedure, private                                                  ::    Append0D_String
  procedure, private                                                  ::    Append1D_Strings
  procedure, public                                                   ::    Open
  procedure, public                                                   ::    Close
  procedure, public                                                   ::    Opened
  procedure, public                                                   ::    Exists
  procedure, public                                                   ::    Rewind
  procedure, public                                                   ::    Backspace
  procedure, public                                                   ::    GetComment
  procedure, public                                                   ::    GetSeparator
  procedure, public                                                   ::    GetFile
  procedure, public                                                   ::    GetFullFile
  procedure, public                                                   ::    GetNbLines
  procedure, nopass, public                                           ::    ReadRecord
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'smuqfile'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%File = '<undefined>'
    This%FullFile = '<undefined>'
    This%Unit = -1
    This%FileOpened = .false.

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Comment = '#'
    This%Separator = ' '

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    Found

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'file'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%File = VarC0D
    This%FullFile = PrefixLoc // This%File

    ParameterName = 'comment'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Comment = VarC0D

    ParameterName = 'separator'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Separator = VarC0D

    if ( This%Comment == '1X' .or. len(This%Comment) == 0 ) This%Comment = ' '
    if ( This%Separator == '1X' .or. len(This%Separator) == 0 ) This%Separator = ' '

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, File, Prefix, Comment, Separator, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    character(*), intent(in)                                          ::    File
    character(*), optional, intent(in)                                ::    Comment
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%File = trim(adjustl(File))
    This%FullFile = PrefixLoc // This%File

    if ( present(Comment) ) This%Comment = Comment

    if ( present(Separator) ) This%Separator = Separator

    if ( This%Comment == '1X' .or. len(This%Comment) == 0 ) This%Comment = ' '
    if ( This%Separator == '1X' .or. len(This%Separator) == 0 ) This%Separator = ' '

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(SMUQFile_Type), intent(in)                                  ::    This
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
    character(:), allocatable                                         ::    CommentLoc
    character(:), allocatable                                         ::    SeparatorLoc

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

    CommentLoc = This%Comment
    if ( CommentLoc == ' ' ) CommentLoc = '1X'
    SeparatorLoc = This%Separator
    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='file', Value=This%File )
    call GetInput%AddParameter( Name='comment', Value=CommentLoc  )
    call GetInput%AddParameter( Name='separator', Value=SeparatorLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportFile( This, Strings, Mandatory, Found, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportFile'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    MandatoryLoc=.true.
    integer                                                           ::    UnitLoc=0 
    integer                                                           ::    NbLines=0
    logical                                                           ::    FoundLoc
    character(:), allocatable                                         ::    Record
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    call This%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=Mandatory, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = This%GetNbLines()
      allocate(Strings(NbLines), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, NbLines
        call This%ReadRecord(Unit=UnitLoc, Record=Record, Stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong reading the record from file', ProcName=ProcName )
        call Strings(i)%Set_Value( Value=Record )
      end do

      call This%Close()
    end if

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Export0D_CharString( This, String, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Export0D_CharStrings='
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call This%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )

    write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) String
    if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )

    call This%Close()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Export0D_String( This, String, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(String_Type), intent(in)                                     ::    String
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Export0D_Strings'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call This%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )

    write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) String%GetValue()
    if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )

    call This%Close()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Export1D_Strings( This, Strings, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(String_Type), dimension(:), intent(in)                       ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Export1D_Strings'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    NbLines=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call This%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )

    NbLines = size(Strings,1)
    i = 1
    do i = 1, NbLines
      write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) Strings(i)%GetValue()
      if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )
    end do

    call This%Close()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Append0D_CharString( This, String, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Append0D_CharString'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0 
    logical                                                           ::    Found 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%Exists() ) then
      call This%Open( Unit=UnitLoc, Action='write', Status='old', Position='append', Mandatory=.false., Found=Found )
      write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) String
      if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )
      call This%Close()
    else
      call This%Export( String=String )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Append0D_String( This, String, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(String_Type), intent(in)                                     ::    String
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Append0D_String'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0
    logical                                                           ::    Found 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%Exists() ) then
      call This%Open( Unit=UnitLoc, Action='write', Status='old', Position='append', Mandatory=.false., Found=Found )
      write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) String%GetValue()
      if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )
      call This%Close()
    else
      call This%Export( String=String )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Append1D_Strings( This, Strings, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    type(String_Type), dimension(:), intent(in)                       ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Append1D_Strings'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    NbLines=0
    integer                                                           ::    i
    logical                                                           ::    Found

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%Exists() ) then
    call This%Open( Unit=UnitLoc, Action='write', Status='old', Position='append', Mandatory=.false., Found=Found )
      NbLines = size(Strings,1)
      i = 1
      do i = 1, NbLines
        write( unit=UnitLoc, fmt='(A)', iostat=StatLoc ) Strings(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )
      end do
      call This%Close()
    else
      call This%Export( Strings=Strings )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Open( This, Unit, Action, Status, Position, Stat, Mandatory, Found, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    integer, optional, intent(out)                                    ::    Unit
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    character(*), optional, intent(in)                                ::    Action
    character(*), optional, intent(in)                                ::    Status
    character(*), optional, intent(in)                                ::    Position
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Open'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    character(:), allocatable                                         ::    PositionLoc
    character(:), allocatable                                         ::    ActionLoc
    character(:), allocatable                                         ::    StatusLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ActionLoc = 'readwrite'
    StatusLoc = 'old'
    PositionLoc = 'rewind'
    if ( present(Mandatory) ) MandatoryLoc = Mandatory
    if ( present(Action) ) ActionLoc = Action
    if ( present(Status) ) StatusLoc = Status
    if ( present(Position) ) PositionLoc = Position

    if ( .not. This%Exists() .and. StatusLoc == 'old' ) then
      if ( MandatoryLoc ) call Error%Raise( Line='Mandatory file does not exist: ' // This%FullFile, ProcName=ProcName )
      FoundLoc = .false.
      UnitLoc = -1
    else
      if ( This%Opened() ) call This%Close()
      open( newunit=UnitLoc, file=This%FullFile, action=ActionLoc, status=StatusLoc, position=PositionLoc, iostat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Open( File=This%FullFile, ProcName=ProcName, iostat=StatLoc )
      This%Unit = UnitLoc
      This%FileOpened = .true.
      FoundLoc = .true.
    end if

    if ( present(Found) ) Found = FoundLoc
    if ( present(Unit) ) Unit = UnitLoc
    if ( present(Stat) ) Stat=StatLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Close( This, Stat, Debug )

    class(SMUQFile_Type), intent(inout)                               ::    This
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Close'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Opened() ) close(Unit=This%Unit, iostat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Close( ProcName=ProcName, Unit=This%Unit, File=This%FullFile, iostat=StatLoc )
    This%Unit = -1
    This%FileOpened = .false.

    if( present(Stat) ) Stat = StatLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Exists( This, Debug )

    logical                                                           ::    Exists

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Exists'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    inquire( file=This%FullFile, exist=Exists )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Opened( This, Debug )

    logical                                                           ::    Opened

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Opened'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

! broken in gcc 7.3
!    inquire( file=This%FullFile, opened=Opened )

    Opened = This%FileOpened

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetComment( This, Debug )

    character(:), allocatable                                         ::    GetComment

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetComment'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetComment = This%Comment

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSeparator( This, Debug )

    character(:), allocatable                                         ::    GetSeparator

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetSeparator'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetSeparator = This%Separator

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetFile( This, Debug )

    character(:), allocatable                                         ::    GetFile

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetFile'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetFile = This%File

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetFullFile( This, Debug )

    character(:), allocatable                                         ::    GetFullFile

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetFullFile'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetFullFile = This%FullFIle

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbLines( This, Debug )

    integer                                                           ::    GetNbLines

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbLines'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%Opened() ) call Error%Raise( Line='File was never opened', ProcName=ProcName)

    GetNbLines = 0

    call This%Rewind()

    do 
      read(unit=This%Unit, fmt=*, iostat=StatLoc)
      if ( IS_IOSTAT_END(StatLoc) ) exit
      GetNbLines = GetNbLines + 1
    end do

    call This%Rewind()

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Rewind( This, Debug )

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Rewind'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%Opened() ) call Error%Raise( Line='File was never opened', ProcName=ProcName)

    Rewind(This%Unit, iostat=StatLoc)
    if (StatLoc /= 0) call Error%Rewind(ProcName=ProcName, Unit=This%Unit, File=This%FullFile, iostat=StatLoc)

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Backspace( This, Debug )

    class(SMUQFile_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Backspace'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%Opened() ) call Error%Raise( Line='File was never opened', ProcName=ProcName)

    Backspace(This%Unit, iostat=StatLoc)
    if (StatLoc /= 0) call Error%Raise( Line='Something went wrong backspacing the file', ProcName=ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadRecord( Unit, Record, Stat, Debug )

    integer, intent(in)                                               ::    Unit
    character(:), allocatable, intent(out)                            ::    Record
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReadRecord'
    integer                                                           ::    StatLoc=0
    character(10000)                                                  ::    Buffer
    logical                                                           ::    Trip
    integer                                                           ::    ReadSize

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

!    Trip = .false.
!    Record = ''
!    do
!      read (unit=Unit,fmt='(A)',advance='NO',iostat=StatLoc, size=ReadSize) Buffer
!      if ( .not. IS_IOSTAT_END(StatLoc) ) exit
!      Record = Record // Buffer(1:100)
!      if (IS_IOSTAT_EOR(StatLoc)) exit
!    end do
!    Record = trim(adjustl(Record))

    read (unit=Unit,fmt='(A10000)', iostat=StatLoc) Buffer
    if ( StatLoc == 0 ) Record = trim(adjustl(Buffer))
    if ( present(Stat) ) Stat=StatLoc

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(SMUQFile_Type), intent(out)                                 ::    LHS
    class(SMUQFile_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%File = RHS%File
      LHS%FullFile = RHS%FullFile
      LHS%Comment = RHS%Comment
      LHS%Separator = RHS%Separator
      LHS%Unit = RHS%Unit
      LHS%FileOpened = RHS%FileOpened
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
