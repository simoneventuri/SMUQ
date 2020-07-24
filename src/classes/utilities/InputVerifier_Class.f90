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

module InputVerifier_Class

use Input_Library
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    InputVerifier_Type

type                                                                  ::    InputVerifier_Type
  logical                                                             ::    Constructed=.false.
  type(InputSection_Type)                                             ::    Outline
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    AddParameter            =>    AddParam
  procedure, public                                                   ::    AddSection
  procedure, public                                                   ::    Process
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(InputVerifier_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  call This%Outline%Free()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This)

  class(InputVerifier_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  call This%Outline%SetName(SectionName='main')

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine AddParam(This, Parameter, ToSubSection)

  class(InputVerifier_Type), intent(inout)                            ::    This
  character(*), intent(in)                                            ::    Parameter 
  character(*), optional, intent(in)                                  ::    ToSubSection

  character(*), parameter                                             ::    ProcName='AddParam'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbParameters
  class(InputSection_Type), pointer                                   ::    InputSection=>null()
  logical                                                             ::    Found

  if (.not. This%Constructed) call Error%Raise('Object not constructed', ProcName=ProcName)
  if (len_trim(adjustl(Parameter)) < 1) call Error%Raise('Passed an empty parameter name', ProcName=ProcName)

  if (present(ToSubSection)) then
    Found = .false.
    call This%Outline%FindTargetSection(TargetSection=InputSection, FromSubSection=ToSubSection, &
                                        Mandatory=.false., FoundSection=Found)
    if (.not. Found) call Error%Raise('Did not find requested section in the outline', ProcName=ProcName)
    NbParameters = InputSection%GetNumberOfParameters()
    call This%Outline%AddParameter(Name=Parameter, Value='null', SectionName=ToSubSection)
  else
    NbParameters = This%Outline%GetNumberOfParameters() 
    call This%Outline%AddParameter(Name=Parameter, Value='null')
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine AddSection(This, Section, ToSubSection)

  class(InputVerifier_Type), intent(inout)                            ::    This
  character(*), intent(in)                                            ::    Section
  character(*), optional, intent(in)                                  ::    ToSubSection

  character(*), parameter                                             ::    ProcName='AddSection'
  integer                                                             ::    StatLoc=0
  class(InputSection_Type), pointer                                   ::    InputSection=>null()
  logical                                                             ::    Found

  if (.not. This%Constructed) call Error%Raise('Object not constructed', ProcName=ProcName)
  if (len_trim(adjustl(Section)) < 1) call Error%Raise('Passed an empty section name', ProcName=ProcName)

  if (present(ToSubSection)) then
    Found = .false.
    call This%Outline%FindTargetSection(TargetSection=InputSection, FromSubSection=ToSubSection, &
                                        Mandatory=.false., FoundSection=Found)
    if (.not. Found) call Error%Raise('Did not find requested section in the outline', ProcName=ProcName)

    call This%Outline%AddSection(SectionName=Section, To_SubSection=ToSubSection)
  else
    call This%Outline%AddSection(SectionName=Section)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Process(This, Input)

  class(InputVerifier_Type), intent(inout)                            ::    This
  class(InputSection_Type), intent(in)                                ::    Input

  character(*), parameter                                             ::    ProcName='Process'
  integer                                                             ::    StatLoc=0
  
  if (.not. This%Constructed) call Error%Raise('Object not constructed', ProcName=ProcName)
  call VerifyInput(Input=Input, Outline=This%Outline)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
recursive subroutine VerifyInput(Input, Outline)

  class(InputSection_Type), intent(in)                                ::    Input
  class(InputSection_Type), intent(in)                                ::    Outline

  character(*), parameter                                             ::    ProcName='VerifyInput'
  integer                                                             ::    StatLoc=0
  class(InputSection_Type), pointer                                   ::    InputSection=>null()
  class(InputSection_Type), pointer                                   ::    OutlineSection=>null()
  logical                                                             ::    Found
  integer                                                             ::    NbInputMembers
  character(:), allocatable                                           ::    InputMember
  logical                                                             ::    BadParameters
  logical                                                             ::    BadSubSections
  integer                                                             ::    i 

  NbInputMembers = Input%GetNumberOfParameters()  
  BadParameters = .false. 

  i = 1
  do i = 1, NbInputMembers
    InputMember = Input%Parameters(i)%GetName()
    if (Outline%HasParameter(ParameterName=InputMember)) cycle
    if (.not. BadParameters) then 
      BadParameters = .true.
      write(*,'(A)')
      write(*,'(A)') 'Unrecognized parameter(s) in input section : ' // Input%GetName()
    end if
    write(*,'(A)') '  *  ' // InputMember
  end do

  if (BadParameters) then
    if (Outline%GetNumberOfParameters() > 0) then
      write(*,'(A)') 
      write(*,'(A)') 'This section supports following parameters'
      i = 1
      do i = 1, Outline%GetNumberOfParameters()
        write(*,'(A)') '  -  ' // Outline%Parameters(i)%GetName()
      end do
    else 
      write(*,'(A)') 
      write(*,'(A)') 'This section does not support any parameter specification'
    end if
  end if

  NbInputMembers = Input%GetNumberOfSubSections()
  BadSubSections = .false.

  i = 1
  do i = 1, NbInputMembers 
    InputMember = Input%Sections(i)%GetName()
    if (Outline%HasSection(SubSectionName=InputMember)) cycle
    if (.not. BadSubSections) then 
      BadSubSections = .true.
      write(*,'(A)')
      write(*,'(A)') 'Unrecognized subsection(s) in input section : ' // Input%GetName()
    end if
    write(*,'(A)') '  *  ' // InputMember
  end do

  if (BadSubSections) then
    if (Outline%GetNumberOfSubSections() > 0) then
      write(*,'(A)') 
      write(*,'(A)') 'This section supports following subsections'
      i = 1
      do i = 1, Outline%GetNumberOfSubSections()
        write(*,'(A)') '  -  ' // Outline%Sections(i)%GetName()
      end do
    else 
      write(*,'(A)') 
      write(*,'(A)') 'This section does not support any subsection specification'
    end if
  end if

  if (BadParameters) call Error%Raise('Errors in input section specification detected', ProcName=ProcName)

  i = 1
  do i = 1, NbInputMembers
    if (associated(InputSection)) nullify(InputSection)
    if (associated(OutlineSection)) nullify(OutlineSection)
    InputMember = Input%Sections(i)%GetName()
    call Outline%FindTargetSection(TargetSection=OutlineSection, FromSubSection=InputMember, Mandatory=.true.)
    if (OutlineSection%GetNumberOfParameters() == 0 .and. OutlineSection%GetNUmberofSubSections() == 0) cycle
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=InputMember, Mandatory=.true.)
    call VerifyInput(Input=InputSection, Outline=OutlineSection)
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(InputVerifier_Type), intent(out)                              ::    LHS
  class(InputVerifier_Type), intent(in)                               ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call LHS%Reset()
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%Outline = RHS%Outline
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(InputVerifier_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module