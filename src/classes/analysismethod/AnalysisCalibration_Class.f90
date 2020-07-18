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

module AnalysisCalibration_Class

use Input_Library
use InputRoutines_Module
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use AnalysisMethod_Class                                          ,only:    AnalysisMethod_Type
use CalibrationMethod_Class                                       ,only:    CalibrationMethod_Type
use CalibrationMethod_Factory_Class                               ,only:    CalibrationMethod_Factory
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type

implicit none

private

public                                                                ::    AnalysisCalibration_Type

type, extends(AnalysisMethod_Type)                                    ::    AnalysisCalibration_Type
  class(CalibrationMethod_Type), allocatable                          ::    CalibrationMethod
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(AnalysisCalibration_Type), intent(inout)                      ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  call This%CalibrationMethod%Reset()

  This%SectionChain = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  class(AnalysisCalibration_Type), intent(inout)                      ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    SectionChain
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%SectionChain = SectionChain

  call CalibrationMethod_Factory%Construct(Object=This%CalibrationMethod, Input=Input, SectionChain=This%SectionChain,         &
                                                                                                              Prefix=PrefixLoc)

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput
  class(AnalysisCalibration_Type), intent(inout)                      ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  GetInput = CalibrationMethod_Factory%GetObjectInput(Object=This%CalibrationMethod, Name=Name,           &
                                                                                        Prefix=PrefixLoc, Directory=DirectorySub)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, SampleSpace, Responses, Model, OutputDirectory)

  class(AnalysisCalibration_Type), intent(inout)                      ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(Model_Type), intent(inout)                                    ::    Model
  character(*), optional, intent(in)                                  ::    OutputDirectory

  character(*), parameter                                             ::    ProcName='Run'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    OutputDirectoryLoc

  OutputDirectoryLoc = ''
  if (present(OutputDirectory)) OutputDirectoryLoc = OutputDirectory

  if (present(OutputDirectory)) then
    call This%CalibrationMethod%Run(SampleSpace=SampleSpace, Responses=Responses, Model=Model, OutputDirectory=OutputDirectoryLoc)
  else
    call This%CalibrationMethod%Run(SampleSpace=SampleSpace, Responses=Responses, Model=Model)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(AnalysisCalibration_Type), intent(out)                        ::    LHS
  class(AnalysisMethod_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (AnalysisCalibration_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%CalibrationMethod, source=RHS%CalibrationMethod, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%CalibrationMethod', ProcName=ProcName, stat=StatLoc)
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(AnalysisCalibration_Type), intent(inout)                       ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%CalibrationMethod)) deallocate(This%CalibrationMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CalibrationMethod', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------


end module
