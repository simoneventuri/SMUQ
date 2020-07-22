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
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    RestartUtility
public                                                                ::    RestartTarget

type                                                                  ::    Restart_Type
  logical                                                             ::    Constructed=.false.
  type(SMUQFile_Type)                                                 ::    RestartFile
  type(InputSection_Type)                                             ::    Input
  character(:), allocatable                                           ::    Prefix
  character(4)                                                        ::    InputName='work'
  character(:), allocatable                                           ::    RestartSection
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetDirectory
  procedure, public                                                   ::    GetPrefix
  generic, public                                                     ::    Update                  =>    Update_Section, &
                                                                                                          Update_Routine
  procedure, private                                                  ::    Update_Section
  procedure, private                                                  ::    Update_Routine
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.
type(Restart_Type)                                                    ::    RestartUtility

abstract interface
  !!------------------------------------------------------------------------------------------------------------------------------
  function RestartTarget(Name, Prefix, Directory)
    import                                                            ::    InputSection_Type
    type(InputSection_Type), allocatable                              ::    RestartTarget
    character(*), intent(in)                                          ::    Name
    character(*), intent(in)                                          ::    Prefix
    character(*), intent(in)                                          ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------
end interface

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(Restart_Type), intent(inout)                                  ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  call This%RestartFile%Reset()

  This%Prefix = ''
  This%RestartSection = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Input, Prefix)
  
  class(Restart_Type), intent(inout)                                  ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    Prefix 

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    UnitLoc=0
  character(:), allocatable                                           ::    VarC0D

  call This%Reset()

  call This%Input%SetName(SectionName=This%InputName)

  This%RestartSection = Input%GetName()

  This%Prefix = Prefix

  call CopyDirectory(Source=ProgramDefs%GetCaseDir(), Destination=This%Prefix, ContentsOnly=.true.)

  call This%Input%AddSection(Section=Input)

  VarC0D = ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
  call This%RestartFile%Construct(File=VarC0D, Prefix=This%Prefix)

  call This%RestartFile%Open(Unit=UnitLoc, Action='write', Status='replace')

  call Input%Write(FileUnit=UnitLoc)

  call This%RestartFile%Close()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetPrefix(This)
  
  character(:), allocatable                                           ::    GetPrefix

  class(Restart_Type), intent(inout)                                  ::    This 

  character(*), parameter                                             ::    ProcName='GetPrefix'
  integer                                                             ::    StatLoc=0

  GetPrefix = This%Prefix

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDirectory(This, SectionChain)
  
  character(:), allocatable                                           ::    GetDirectory

  class(Restart_Type), intent(inout)                                  ::    This
  character(*), intent(in)                                            ::    SectionChain 

  character(*), parameter                                             ::    ProcName='GetDirectory'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), allocatable, dimension(:)                    ::    SectionNames
  integer                                                             ::    NbSections=0
  integer                                                             ::    i

  call ConvertToStrings(Value=SectionChain, Strings=SectionNames, Separator='>')

  NbSections = size(SectionNames,1)

  GetDirectory = ''

  do i = 1, NbSections
    GetDirectory = GetDirectory // '/' // SectionNames(i)
  end do

  deallocate(SectionNames, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='SectionNames', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Update_Section(This, InputSection, SectionChain)
  
  use String_Library

  class(Restart_Type), intent(inout)                                  ::    This
  type(InputSection_Type), intent(in)                                 ::    InputSection
  character(*), intent(in)                                            ::    SectionChain 

  character(*), parameter                                             ::    ProcName='Update_Section'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSectionPointer=>null()
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    UnitLoc=0
  character(:), allocatable                                           ::    Line

  call This%Input%FindTargetSection(TargetSection=InputSectionPointer, FromSubSection=SectionChain, Mandatory=.true.)

  SectionName = InputSectionPointer%GetName()

  InputSectionPointer = InputSection

  call InputSectionPointer%SetName(SectionName=SectionName)

  nullify(InputSectionPointer)

  call This%Input%FindTargetSection(TargetSection=InputSectionPointer, FromSubSection=This%RestartSection, Mandatory=.true.)

  call This%RestartFile%Open(Unit=UnitLoc, Action='write', Status='replace')

  call InputSectionPointer%Write(FileUnit=UnitLoc)

  call This%RestartFile%Close()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Update_Routine(This, Input, SectionChain)
  
  class(Restart_Type), intent(inout)                                  ::    This
  procedure(RestartTarget), pointer                                   ::    Input
  character(*), intent(in)                                            ::    SectionChain 

  character(*), parameter                                             ::    ProcName='Update_Routine'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), allocatable                                ::    InputSection

  allocate(InputSection, source=Input(Name='temp', Prefix=This%GetPrefix(), &
                                      Directory=This%GetDirectory(SectionChain=SectionChain)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='InputSection', ProcName=ProcName, stat=StatLoc)

  call This%Update(InputSection=InputSection, SectionChain=SectionChain)

  deallocate(InputSection, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='InputSection', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(Restart_Type), intent(out)                                    ::    LHS
  class(Restart_Type), intent(in)                                     ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  call LHS%Reset()
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%RestartFile = RHS%RestartFile
    LHS%Input = RHS%Input
    LHS%Prefix = RHS%Prefix
    LHS%RestartSection = RHS%RestartSection
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(Restart_Type), intent(inout)                                   ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
