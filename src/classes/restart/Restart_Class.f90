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

module Restart_Class

use Input_Library
use Parameters_Library
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs

implicit none

private

public                                                                ::    RestartUtility

type                                                                  ::    Restart_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(SMUQFile_Type)                                                 ::    RestartFile
  type(InputSection_Type)                                             ::    Input
  character(:), allocatable                                           ::    Prefix
  character(4)                                                        ::    InputName='work'
  character(:), allocatable                                           ::    RestartSection
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetDirectory
  procedure, public                                                   ::    GetPrefix
  procedure, public                                                   ::    Update
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.
type(Restart_Type)                                                    ::    RestartUtility

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(Restart_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'restart'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(Restart_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%RestartFile%Reset()

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(Restart_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Prefix = ''
    This%RestartSection = ''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, Input, Prefix, Debug )
    
    class(Restart_Type), intent(inout)                                ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc=0
    character(:), allocatable                                         ::    VarC0D


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    call This%Input%SetName( SectionName=This%InputName )

    This%RestartSection = Input%GetName()

    This%Prefix = Prefix

    VarC0D = This%Prefix // ProgramDefs%GetInputFilePrefix()
    call MakeDirectory( Path=VarC0D, Options='-p' )

    call This%Input%AddSection( Section=Input )

    VarC0D = ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
    call This%RestartFile%Construct( File=VarC0D, Prefix=This%Prefix )

    call This%RestartFile%Open( Unit=UnitLoc, Action='write', Status='replace' )

    call Input%Write( FileUnit=UnitLoc )

    call This%RestartFile%Close()

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPrefix( This, Debug )
    
    character(:), allocatable                                         ::    GetPrefix

    class(Restart_Type), intent(inout)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPrefix'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetPrefix = This%Prefix

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetDirectory( This, SectionChain, Debug )
    
    use String_Library

    character(:), allocatable                                         ::    GetDirectory

    class(Restart_Type), intent(inout)                                ::    This
    character(*), intent(in)                                          ::    SectionChain
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDirectory'
    integer                                                           ::    StatLoc=0
    character(:), allocatable, dimension(:)                           ::    SectionNames
    integer                                                           ::    NbSections=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call Parse( Input=SectionChain, Separator='>', Output=SectionNames )
    NbSections = size(SectionNames,1)

    GetDirectory = ''

    do i = 1, NbSections
      GetDirectory = GetDirectory // '/' // trim(adjustl(SectionNames(i)))
    end do

    deallocate(SectionNames, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SectionNames', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Update( This, InputSection, SectionChain, Debug )
    
    use String_Library

    class(Restart_Type), intent(inout)                                ::    This
    type(InputSection_Type), intent(in)                               ::    InputSection
    character(*), intent(in)                                          ::    SectionChain
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSectionPointer=>null()
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    UnitLoc=0
    character(:), allocatable                                         ::    Line

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call This%Input%FindTargetSection( TargetSection=InputSectionPointer, FromSubSection=SectionChain, Mandatory=.true. )

    SectionName = InputSectionPointer%GetName()

    InputSectionPointer = InputSection

    call InputSectionPointer%SetName( SectionName=SectionName )

    nullify(InputSectionPointer)

    call This%Input%FindTargetSection( TargetSection=InputSectionPointer, FromSubSection=This%RestartSection, Mandatory=.true. )

    call This%RestartFile%Open( Unit=UnitLoc, Action='write', Status='replace' )

    call InputSectionPointer%Write( FileUnit=UnitLoc )

    call This%RestartFile%Close()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(Restart_Type), intent(out)                                  ::    LHS
    class(Restart_Type), intent(in)                                   ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%RestartFile = RHS%RestartFile
      LHS%Input = RHS%Input
      LHS%Prefix = RHS%Prefix
      LHS%RestartSection = RHS%RestartSection
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(Restart_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
