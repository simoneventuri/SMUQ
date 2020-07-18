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

module InputRoutines_Module

use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    VerifyInput

logical, parameter                                                    ::    DebugGlobal = .false.

interface VerifyInput 
  module procedure                                                    ::    VerifyInput_C0D_C0D
  module procedure                                                    ::    VerifyInput_String0D_String0D
  module procedure                                                    ::    VerifyInput_String1D_C0D
  module procedure                                                    ::    VerifyInput_String1D_String0D
  module procedure                                                    ::    VerifyInput_C0D_String1D
  module procedure                                                    ::    VerifyInput_String0D_String1D
  module procedure                                                    ::    VerifyInput_String1D_String1D
end interface

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_C0D_C0D(Input, Parameter, SubSection)

  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    Parameter
  character(*), intent(in)                                            ::    SubSection

  character(*), parameter                                             ::    ProcName='VerifyInput_C0D_C0D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    Parameters
  type(SMUQString_Type), dimension(1)                                 ::    SubSections
  
  Parameters(1) = Parameter 
  SubSections(1) = SubSection 
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_String0D_String0D(Input, Parameter, SubSection)

  type(InputSection_Type), intent(in)                                 ::    Input
  type(SMUQString_Type), intent(in)                                   ::    Parameter
  type(SMUQString_Type), intent(in)                                   ::    SubSection

  character(*), parameter                                             ::    ProcName='VerifyInput_String0D_String0D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    Parameters
  type(SMUQString_Type), dimension(1)                                 ::    SubSections
  
  Parameters(1) = Parameter 
  SubSections(1) = SubSection 
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_String1D_C0D(Input, Parameters, SubSection)

  type(InputSection_Type), intent(in)                                 ::    Input
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Parameters
  character(*), intent(in)                                            ::    SubSection

  character(*), parameter                                             ::    ProcName='VerifyInput_String1D_C0D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    SubSections
  
  SubSections(1) = SubSection 
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_String1D_String0D(Input, Parameters, SubSection)

  type(InputSection_Type), intent(in)                                 ::    Input
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Parameters
  type(SMUQString_Type), intent(in)                                   ::    SubSection

  character(*), parameter                                             ::    ProcName='VerifyInput_String1D_String0D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    SubSections
  
  SubSections(1) = SubSection 
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_C0D_String1D(Input, Parameter, SubSections)

  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    Parameter
  type(SMUQString_Type), dimension(:), intent(in)                     ::    SubSections

  character(*), parameter                                             ::    ProcName='VerifyInput_C0D_String1D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    Parameters
  
  Parameters(1) = Parameter
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_String0D_String1D(Input, Parameter, SubSections)

  type(InputSection_Type), intent(in)                                 ::    Input
  type(SMUQString_Type), intent(in)                                   ::    Parameter
  type(SMUQString_Type), dimension(:), intent(in)                     ::    SubSections

  character(*), parameter                                             ::    ProcName='VerifyInput_String0D_String1D'
  integer                                                             ::    StatLoc=0
  type(SMUQString_Type), dimension(1)                                 ::    Parameters
  
  Parameters(1) = Parameter
  call VerifyInput(Input=Input, Parameters=Parameters, SubSections=SubSections)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine VerifyInput_String1D_String1D(Input, Parameters, SubSections)

  type(InputSection_Type), intent(in)                                 ::    Input
  type(SMUQString_Type), dimension(:), optional, intent(in)           ::    Parameters
  type(SMUQString_Type), dimension(:), optional, intent(in)           ::    SubSections

  character(*), parameter                                             ::    ProcName='VerifyInput_String1D_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbSubSections 
  integer                                                             ::    NbSectionSubSections
  type(SMUQString_Type), allocatable, dimension(:)                    ::    SectionSubSections
  logical, allocatable, dimension(:)                                  ::    BadSection
  integer                                                             ::    NbParameters 
  integer                                                             ::    NbSectionParameters 
  type(SMUQString_Type), allocatable, dimension(:)                    ::    SectionParameters
  logical, allocatable, dimension(:)                                  ::    BadParameter 
  integer                                                             ::    i 
  integer                                                             ::    ii

  ! Scan over subsections
  if (present(SubSections)) then
    NbSubSections = size(SubSections,1)
    NbSectionSubSections = Input%GetNumberOfSubSections() 

    i = 1
    do i = 1, NbSubSections
      ii = 1
      do ii = i+1, NbSectionSubSections
        if (SubSections(i) /= SubSections(ii)) cycle
        call Error%Raise('Passed subsections list with duplicate entries', ProcName=ProcName)
      end do
    end do

    if (NbSectionSubSections > 0) then
      allocate(BadSection(NbSectionSubSections), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='BadSection', ProcName=ProcName, stat=StatLoc)
      BadSection = .true. 

      allocate(SectionSubSections(NbSectionSubSections), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='SectionSubSections', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, NbSectionSubSections
        SectionSubSections(i) = Input%Sections(i)%GetName()
      end do

      i = 1
      do i = 1, NbSectionSubSections 
        ii = 1
        do ii = i+1, NbSectionSubSections
          if (SectionSubSections(i) /= SectionSubSections(ii)) cycle
          call Error%Raise('Input section ' // Input%GetName() // ' has duplicate subsection entries : ' // &
                          SectionSubSections(i)%Get(), ProcName=ProcName)
        end do
      end do

      i = 1
      do i = 1, NbSubSections
        ii = 1
        do ii = 1, NbSectionSubSections 
          if (SectionSubSections(ii) /= SubSections(i)) cycle
          BadSection(ii) = .false.
          exit
        end do
      end do

      if (any(BadSection)) then
        write(*,'(A)')
        write(*,'(A)') 'Unrecognized SubSections in input section : ' // Input%GetName()
        i = 1
        do i = 1, NbSectionSubSections
          if (.not. BadSection(i)) cycle
          write(*,'(A)') ' - ' // SectionSubSections(i)%Get()
        end do
        write(*,'(A)')
        if (NbSubSections > 0) then
          write(*,'(A)') 'This section supports subsections' // Input%GetName()
          i = 1
          do i = 1, NbSectionSubSections
            write(*,'(A)') ' - ' // SubSections(i)%Get()
          end do
        else
          write(*,'(A)') 'This section should not contain subsections'
        end if
        call Error%Raise(ProcName=ProcName)
      end if

    end if
  end if

  ! Scan over parameters
  if (present(Parameters)) then
    NbParameters = size(Parameters,1)
    NbSectionParameters = Input%GetNumberOfParameters() 
  
    i = 1
    do i = 1, NbParameters 
      ii = 1
      do ii = i+1, NbParameters
        if (Parameters(i) /= Parameters(ii)) cycle
        call Error%Raise('Passed parameters list with duplicate entries', ProcName=ProcName)
      end do
    end do
  
    if (NbSectionParameters > 0) then
      allocate(BadParameter(NbSectionParameters), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='BadParameter', ProcName=ProcName, stat=StatLoc)
      BadParameter = .true. 
  
      allocate(SectionParameters(NbSectionParameters), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='SectionParameters', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, NbSectionParameters
        SectionParameters(i) = Input%Parameters(i)%GetName()
      end do
  
      i = 1
      do i = 1, NbSectionParameters 
        ii = 1
        do ii = i+1, NbSectionParameters
          if (SectionParameters(i) /= SectionParameters(ii)) cycle
          call Error%Raise('Input section ' // Input%GetName() // ' has duplicate parameter entries : ' // &
                           SectionParameters(i)%Get(), ProcName=ProcName)
        end do
      end do
  
      i = 1
      do i = 1, NbParameters
        ii = 1
        do ii = 1, NbSectionParameters 
          if (SectionParameters(ii) /= Parameters(i)) cycle
          BadParameter(ii) = .false.
          exit
        end do
      end do
  
      if (any(BadParameter)) then
        write(*,'(A)')
        write(*,'(A)') 'Unrecognized parameters in input section : ' // Input%GetName()
        i = 1
        do i = 1, NbSectionParameters
          if (.not. BadParameter(i)) cycle
          write(*,'(A)') ' - ' // SectionParameters(i)%Get()
        end do
        write(*,'(A)')
        if (NbParameters > 0) then
          write(*,'(A)') 'This section supports following parameter inputs' // Input%GetName()
          i = 1
          do i = 1, NbSectionParameters
            write(*,'(A)') ' - ' // Parameters(i)%Get()
          end do
        else
          write(*,'(A)') 'This section should not contain parameter inputs'
        end if
        call Error%Raise(ProcName=ProcName)
      end if
  
    end if
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module