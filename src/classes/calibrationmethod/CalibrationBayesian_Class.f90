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

module CalibrationBayesian_Class

use Input_Library
use Parameters_Library
use CommandRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CalibrationMethod_Class                                       ,only:    CalibrationMethod_Type
use BayesInvMethod_Class                                          ,only:    BayesInvMethod_Type
use BayesInvMethod_Factory_Class                                  ,only:    BayesInvMethod_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type
use LikelihoodProduct_Class                                       ,only:    LikelihoodProduct_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    CalibrationBayesian_Type

type, extends(CalibrationMethod_Type)                                 ::    CalibrationBayesian_Type
  class(BayesInvMethod_Type), allocatable                             ::    BayesInvMethod
  type(LikelihoodProduct_Type)                                        ::    Likelihood
  logical                                                             ::    Silent
contains
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type                                                                

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(CalibrationBayesian_Type), intent(inout)                      ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%BayesInvMethod)) deallocate(This%BayesInvMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%BayesInvMethod', ProcName=ProcName, stat=StatLoc)

  call This%Likelihood%Reset()

  This%Silent = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput (This, Input, SectionChain, Prefix)

  class(CalibrationBayesian_Type), intent(inout)                      ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    SectionChain
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    VarL0D
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%SectionChain = SectionChain

  call InputVerifier%Construct()

  ParameterName= 'silent'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Silent=VarL0D

  SectionName = 'likelihood'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call This%Likelihood%Construct(Input=InputSection, Prefix=PrefixLoc)

  VarC0D = SectionChain // '>method'
  SectionName = 'method'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call BayesInvMethod_Factory%Construct(Object=This%BayesInvMethod, Input=InputSection, SectionChain=VarC0D, Prefix=PrefixLoc)
  nullify(InputSection)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine 
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)
  
  type(InputSection_Type)                                             ::    GetInput

  class(CalibrationBayesian_Type), intent(inout)                      ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='silent', Value=ConvertToString(Value=This%Silent))

  SectionName = 'likelihood'
  if (ExternalFlag) DirectorySub = DirectoryLoc // 'likelihood/'
  call GetInput%AddSection(Section=This%Likelihood%GetInput(Name=SectioNName, Prefix=PrefixLoc,                    &
                                                                                                      Directory=DirectorySub))

  SectionName = 'method'
  if (ExternalFlag) DirectorySub = DirectoryLoc // 'method/'
  call GetInput%AddSection(Section=BayesInvMethod_Factory%GetObjectInput(Object=This%BayesInvMethod,                          &
                                                        Name=SectionName, Prefix=PrefixLoc, Directory=DirectorySub))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, SampleSpace, Responses, Model, OutputDirectory)

  class(CalibrationBayesian_Type), intent(inout)                      ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(Model_Type), intent(inout)                                    ::    Model
  character(*), optional, intent(in)                                  ::    OutputDirectory

  character(*), parameter                                             ::    ProcName='Run'
  integer                                                             ::    StatLoc=0

  if (present(OutputDirectory)) then
    call This%BayesInvMethod%Calibrate(Model=Model, SampleSpace=SampleSpace, Responses=Responses,                              &
                                                            LikelihoodFunction=This%Likelihood, OutputDirectory=OutputDirectory)
  else
    call This%BayesInvMethod%Calibrate(Model=Model, SampleSpace=SampleSpace, Responses=Responses,                              &
                                                                                            LikelihoodFunction=This%Likelihood)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteOutput(This, Posterior, Directory)

  class(CalibrationBayesian_Type), intent(inout)                      ::    This
  real(rkp), dimension(:,:), intent(in)                               ::    Posterior
  character(*), intent(in)                                            ::    Directory

  character(*), parameter                                             ::    ProcName='WriteOutput'
  type(InputSection_Type)                                             ::    Input
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  logical                                                             ::    SilentLoc
  character(:), allocatable                                           ::    Line
  integer                                                             ::    IOLoc
  integer                                                             ::    UnitLoc
  type(SMUQFile_Type)                                                 ::    File
  integer                                                             ::    i, ii

  if (len_trim(Directory) /= 0) then

    call MakeDirectory(Path=Directory, Options='-p')

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(CalibrationBayesian_Type), intent(out)                        ::    LHS
  class(CalibrationMethod_Type), intent(in)                           ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (CalibrationBayesian_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%BayesInvMethod, source=RHS%BayesInvMethod, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%BayesInvMethod', ProcName=ProcName, stat=StatLoc)
        LHS%Likelihood = RHS%Likelihood
        LHS%Silent = RHS%Silent
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(CalibrationBayesian_Type), intent(inout)                       ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%BayesInvMethod)) deallocate(This%BayesInvMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%BayesInvMethod', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
