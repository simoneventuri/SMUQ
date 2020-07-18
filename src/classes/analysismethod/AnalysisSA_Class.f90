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

module AnalysisSA_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use AnalysisMethod_Class                                          ,only:    AnalysisMethod_Type
use SAMethod_Class                                                ,only:    SAMethod_Type
use SAMethod_Factory_Class                                        ,only:    SAMethod_Factory
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type

implicit none

private

public                                                                ::    AnalysisSA_Type

type, extends(AnalysisMethod_Type)                                    ::    AnalysisSA_Type
  class(SAMethod_Type), allocatable                                   ::    SAMethod
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

  class(AnalysisSA_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  deallocate(This%SAMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SAMethod', ProcName=ProcName, stat=StatLoc)

  This%SectionChain = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  class(AnalysisSA_Type), intent(inout)                               ::    This
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

  call SAMethod_Factory%Construct(Object=This%SAMethod, Input=Input, SectionChain=This%SectionChain, Prefix=PrefixLoc)

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput
  class(AnalysisSA_Type), intent(inout)                               ::    This
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

  GetInput = SAMethod_Factory%GetObjectInput(Object=This%SAMethod, Name=Name, Prefix=PrefixLoc, &
                                             Directory=DirectorySub)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, SampleSpace, Responses, Model, OutputDirectory)

  class(AnalysisSA_Type), intent(inout)                               ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(Model_Type), intent(inout)                                    ::    Model
  character(*), optional, intent(in)                                  ::    OutputDirectory

  character(*), parameter                                             ::    ProcName='Run'
  integer                                                             ::    StatLoc=0

  if (present(OutputDirectory)) then
    call This%SAMethod%Run(SampleSpace=SampleSpace, Responses=Responses, Model=Model, OutputDirectory=OutputDirectory)
  else
    call This%SAMethod%Run(SampleSpace=SampleSpace, Responses=Responses, Model=Model)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(AnalysisSA_Type), intent(out)                                 ::    LHS
  class(AnalysisMethod_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (AnalysisSA_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%SAMethod, source=RHS%SAMethod, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%SAMethod', ProcName=ProcName, stat=StatLoc)
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(AnalysisSA_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%SAMethod)) deallocate(This%SAMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%SAMethod', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
