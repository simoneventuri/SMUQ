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

module TFUN_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use TestFunction_Class                                            ,only:    TestFunction_Type
use TestFunctionContainer_Class                                   ,only:    TestFunctionContainer_Type
use TestFunction_Factory_Class                                    ,only:    TestFunction_Factory
use ModelInternal_Class                                           ,only:    ModelInternal_Type
use Model_Class                                                   ,only:    Model_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputProcessor_Class                                          ,only:    InputProcessor_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    TFUN_Type

type, extends(ModelInternal_Type)                                     ::    TFUN_Type
  class(TestFunctionContainer_Type), allocatable, dimension(:)        ::    TestFunctions
  integer                                                             ::    NbFunctions
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run_0D
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(TFUN_Type), intent(inout)                                   ::    This

  character(*), parameter                                           ::    ProcName='Reset'
  integer                                                           ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%TestFunctions)) deallocate(This%TestFunctions, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%TestFunctions', ProcName=ProcName, stat=StatLoc)
  This%NbFunctions = 0

  This%Label = 'tfun'
  This%NbOutputs = 0
  This%Silent = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(TFUN_Type), intent(inout)                                   ::    This
  class(InputSection_Type), intent(in)                              ::    Input
  character(*), optional, intent(in)                                ::    Prefix

  character(*), parameter                                           ::    ProcName='ConstructInput'
  character(:), allocatable                                         ::    PrefixLoc
  integer                                                           ::    StatLoc=0
  type(InputSection_Type), pointer                                  ::    InputSection=>null()
  character(:), allocatable                                         ::    ParameterName
  character(:), allocatable                                         ::    SectionName
  character(:), allocatable                                         ::    SubSectionName
  character(:), allocatable                                         ::    VarC0D
  logical                                                           ::    VarL0D
  integer                                                           ::    i
  integer                                                           ::    ii
  class(TestFunction_Type), allocatable                             ::    TestFunction
  type(SMUQString_Type), allocatable, dimension(:)                  ::    Labels
  logical                                                           ::    Found
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Label = VarC0D

  ParameterName = 'silent'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Silent = VarL0D

  SectionName = 'functions'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  This%NbFunctions = InputSection%GetNumberofSubSections()
  nullify(InputSection)

  allocate(This%TestFunctions(This%NbFunctions), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%TestFunctions', ProcName=ProcName, stat=StatLoc)

  allocate(Labels(This%NbFunctions), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbFunctions
    SubSectionName = SectionName // '>function' // ConvertToString(Value=i)
  call InputVerifier%AddSection(Section='function' // ConvertToString(Value=i), ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call TestFunction_Factory%Construct(Object=TestFunction, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%TestFunctions(i)%Set(Object=TestFunction)
    Labels(i) = TestFunction%GetLabel()
    deallocate(TestFunction, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TestFunction', ProcName=ProcName, stat=StatLoc)
  end do

  i = 1
  do i = 1, This%NbFunctions
    ii = i
    do ii = i, This%NbFunctions
      if (i == ii) cycle
      if (Labels(i) == Labels(ii)) call Error%Raise('Detected duplicate output label', ProcName=ProcName)
    end do
  end do

  This%NbOutputs = This%NbFunctions

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                           ::    GetInput

  class(TFUN_Type), intent(in)                                      ::    This
  character(*), intent(in)                                          ::    Name
  character(*), optional, intent(in)                                ::    Prefix
  character(*), optional, intent(in)                                ::    Directory

  character(*), parameter                                           ::    ProcName='GetInput'
  character(:), allocatable                                         ::    PrefixLoc
  character(:), allocatable                                         ::    DirectoryLoc
  character(:), allocatable                                         ::    DirectorySub
  character(:), allocatable                                         ::    SectionName
  logical                                                           ::    ExternalFlag=.false.
  class(TestFunction_Type), pointer                                 ::    TestFunctionPtr=>null()
  integer                                                           ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='label', Value=This%Label)

  call GetInput%AddSection(SectionName='functions')

  i = 1
  do i = 1, This%NbFunctions
    SectionName = 'function' // ConvertToString(Value=i)
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'function' // ConvertToString(Value=i) // '/'
    TestFunctionPtr => This%TestFunctions(i)%GetPointer()
    call GetInput%AddSection(Section=TestFunction_Factory%GetObjectInput(Object=TestFunctionPtr, Name=SectionName,  &
                                                          Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection='functions')
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Run_0D(This, Input, Output, Stat)

  class(TFUN_Type), intent(inout)                                   ::    This
  type(Input_Type), intent(in)                                      ::    Input
  type(Output_Type), dimension(:), intent(inout)                    ::    Output
  integer, optional, intent(out)                                    ::    Stat

  character(*), parameter                                           ::    ProcName='Run_0D'
  integer                                                           ::    StatLoc=0
  class(TestFunction_Type), pointer                                 ::    TestFunctionPtr=>null()
  integer                                                           ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (size(Output,1) /= This%NbOutputs) call Error%Raise('Passed down an output array of incorrect length',                  &
                                                                                                              ProcName=ProcName)

  i = 1
  do i = 1, This%NbFunctions
    TestFunctionPtr => This%TestFunctions(i)%GetPointer()
    call TestFunctionPtr%Run(Input=Input, Output=Output(i))
  end do

  if (present(Stat)) Stat = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(TFUN_Type), intent(out)                                     ::    LHS
  class(Model_Type), intent(in)                                     ::    RHS

  character(*), parameter                                           ::    ProcName='Copy'
  integer                                                           ::    StatLoc=0

  select type (RHS)

    type is (TFUN_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%NbFunctions = RHS%NbFunctions
        allocate(LHS%TestFunctions, source=RHS%TestFunctions, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%TestFunctions', ProcName=ProcName, stat=StatLoc)
        LHS%NbOutputs = RHS%NbOutputs
        LHS%Label = RHS%Label
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
