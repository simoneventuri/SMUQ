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

module TestFunction_Factory_Class

use Logger_Class                                                    ,only:    Logger
use Error_Class                                                     ,only:    Error
use String_Module
use TestFunction_Class                                              ,only:    TestFunction_Type
use TestIshigami_Class                                              ,only:    TestIshigami_Type
use TestGFun_Class                                                  ,only:    TestGFun_Type
use TestBorehole_Class                                              ,only:    TestBorehole_Type
use TestSpill_Class                                                 ,only:    TestSpill_Type
use InputVerifier_Class                                             ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    TestFunction_Factory

type                                                                  ::    TestFunction_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D,          &
                                                                                                          Construct_Input
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, public                                                   ::    Construct_Input
  procedure, nopass, public                                           ::    GetOption
  procedure, public                                                   ::    GetObjectInput
End Type

type(TestFunction_Factory_Type)                                       ::    TestFunction_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Construct_C0D(Object, DesiredType)

  class(TestFunction_Type), allocatable, intent(inout)                ::    Object
  character(*), intent(in)                                            ::    DesiredType

  character(*), parameter                                             ::    ProcName='Construct_C0D'                                    

  if (allocated(Object)) call Error%Raise(Line='Object already allocated', ProcName=ProcName)

  select case (LowerCase(DesiredType))

    case('ishigami')
      allocate(TestIshigami_Type   :: Object)

    case('gfunction')
      allocate(TestGFun_Type   :: Object)

    case('borehole')
      allocate(TestBorehole_Type   :: Object)

    case('spill')
      allocate(TestSpill_Type   :: Object)

    case default
      call Error%Raise("Type not supported: DesiredType = " // LowerCase(DesiredType))

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Construct_Input(This, Object, Input, Prefix)
  
  use Input_Library

  class(TestFunction_Factory_Type), intent(in)                        ::    This
  class(TestFunction_Type), allocatable, intent(inout)                ::    Object
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='Construct_Input'                                   
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    StatLoc=0 
  type(InputVerifier_Type)                                            ::    InputVerifier

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()
  call InputVerifier%AddParameter(Parameter='type')
  call InputVerifier%AddSection(Section='type')
  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  ParameterName = 'type'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  call This%Construct(Object=Object, DesiredType=VarC0D)

  SectionName = 'type'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call Object%Construct(Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetOption(Object)

  character(:), allocatable                                           ::    GetOption

  class(TestFunction_Type), intent(in)                                ::    Object                                                                                            

  character(*), parameter                                             ::    ProcName='GetOption' 

  select type (Object)

    type is (TestIshigami_Type)
      GetOption = 'ishigami'

    type is (TestGFun_Type)
      GetOption = 'gfunction'

    type is (TestBorehole_Type)
      GetOption = 'borehole'

    type is (TestSpill_Type)
      GetOption = 'spill'

    class default
      call Error%Raise(Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName)

  end select

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetObjectInput(This, Object, Name, Prefix, Directory)

  use Input_Library

  type(InputSection_Type)                                             ::    GetObjectInput

  class(TestFunction_Factory_Type), intent(in)                        ::    This
  class(TestFunction_Type), intent(in)                                ::    Object
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetObjectInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  integer                                                             ::    StatLoc=0
  logical                                                             ::    ExternalFlag=.false.

  DirectoryLoc = ''
  PrefixLoc = ''
  DirectorySub = DirectoryLoc
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix

  call GetObjectInput%SetName(SectionName=Name)

  call GetObjectInput%AddParameter(Name='type', Value=This%GetOption(Object=Object))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'type/'

  call GetObjectInput%AddSection(Section=Object%GetInput(Name='type', Prefix=PrefixLoc, Directory=DirectorySub))

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
