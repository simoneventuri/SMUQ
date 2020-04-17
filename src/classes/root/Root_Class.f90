! -*-f90-*-
!!-----------------------------------------------------------------------------
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
!!-----------------------------------------------------------------------------

module Root_Class

use Input_Library
use Parameters_Library
use String_Library
use CommandRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use AnalysisMethod_Class                                          ,only:    AnalysisMethod_Type
use AnalysisMethod_Factory_Class                                  ,only:    AnalysisMethod_Factory
use Response_Class                                                ,only:    Response_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use Model_Class                                                   ,only:    Model_Type
use Model_Factory_Class                                           ,only:    Model_Factory
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    Root_Type

type                                                                  ::    Root_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  class(AnalysisMethod_Type), allocatable                             ::    AnalysisMethod
  type(Response_Type), allocatable, dimension(:)                      ::    Responses
  type(ParamSpace_Type)                                               ::    ParameterSpace
  class(Model_Type), allocatable                                      ::    Model
  character(:), allocatable                                           ::    SectionChain  
  integer                                                             ::    NbRepetitions                                           
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, private                                                  ::    WriteOutput
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!-----------------------------------------------------------------------------
subroutine Initialize(This)

  class(Root_Type), intent(inout)                                   ::    This

  character(*), parameter                                           ::    ProcName='Initialize'
  integer                                                           ::    StatLoc=0

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'root'
    call This%SetDefaults()
  end if

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
subroutine Reset(This)

  class(Root_Type), intent(inout)                                   ::    This

  character(*), parameter                                           ::    ProcName='Reset'
  integer                                                           ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%Model)) deallocate(This%Model, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Model',                  &
                                          ProcName=ProcName,                  &
                                          stat=StatLoc)

  if (allocated(This%AnalysisMethod)) deallocate(This%AnalysisMethod,         &
                                                 stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%AnalysisMethod',         &
                                          ProcName=ProcName,                  &
                                          stat=StatLoc)

  if (allocated(This%Responses)) deallocate(This%Responses, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Responses',              &
                                          ProcName=ProcName,                  &
                                          stat=StatLoc)

  call This%ParameterSpace%Reset()

  call This%Initialize()

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(Root_Type), intent(inout)                                   ::    This

  character(*), parameter                                           ::    ProcName='SetDefaults'
  integer                                                           ::    StatLoc=0

  This%NbRepetitions = 1

  This%SectionChain = ''

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  use StringRoutines_Module

  class(Root_Type), intent(inout)                                   ::    This
  class(InputSection_Type), intent(in)                              ::    Input
  character(*), intent(in)                                          ::    SectionChain
  character(*), optional, intent(in)                                ::    Prefix

  character(*), parameter                                           ::    ProcName='ConstructInput'
  integer                                                           ::    StatLoc=0
  character(:), allocatable                                         ::    PrefixLoc
  type(InputSection_Type), pointer                                  ::    InputSection=>null()
  character(:), allocatable                                         ::    SectionName
  character(:), allocatable                                         ::    SubSectionName
  character(:), allocatable                                         ::    ParameterName
  integer                                                           ::    VarI0D
  integer                                                           ::    NbResponses
  integer                                                           ::    i
  integer                                                           ::    ii
  character(:), allocatable                                         ::    Label1
  character(:), allocatable                                         ::    Label2
  character(:), allocatable                                         ::    VarC0D
  type(InputReader_Type)                                            ::    ModelInput
  character(:), allocatable                                         ::    FileName
  logical                                                           ::    Found

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%SectionChain = SectionChain

  call Input%GetValue(Value=VarI0D, ParameterName='nb_repetitions', Mandatory=.false., Found=Found)
  if ( Found ) This%NbRepetitions = VarI0D

  SectionName = 'model'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call Model_Factory%Construct(Object=This%Model, Input=InputSection, Prefix=PrefixLoc)
  nullify (InputSection)

  SectionName = "parameter_space"
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call This%ParameterSpace%Construct(Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'analysis'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call AnalysisMethod_Factory%Construct(Object=This%AnalysisMethod, Input=InputSection,                                           &
                                        SectionChain=This%SectionChain // '>analysis', Prefix=PrefixLoc)
  nullify (InputSection)

  SectionName = "responses"
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  NbResponses = InputSection%GetNumberofSubSections()
  if (NbResponses < 1) call Error%Raise(Line='Number of specified responses below minimum of 1', ProcName=ProcName)
  allocate(This%Responses(NbResponses), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Responses', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, NbResponses
    SubSectionName = SectionName // ">response" // ConvertToString(Value=i)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.) 
    call This%Responses(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify (InputSection)
  end do

  i = 1
  do i = 1, NbResponses
    Label1 = This%Responses(i)%GetLabel()
    ii = i+1
    do ii = i + 1, NbResponses
      Label2 = This%Responses(ii)%GetLabel()
      if (Label1 /= Label2) cycle
      call Error%Raise('Detected duplicate label: ' // Label1, ProcName=ProcName)
    end do
  end do

  This%Constructed = .true.

end subroutine
!!-----------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput( This, MainSectionName, Prefix, Directory )

  use StringRoutines_Module

  type(InputSection_Type)                                           ::    GetInput
  class(Root_Type), intent(inout)                                   ::    This
  character(*), intent(in)                                          ::    MainSectionName
  character(*), optional, intent(in)                                ::    Prefix
  character(*), optional, intent(in)                                ::    Directory

  character(*), parameter                                           ::    ProcName='GetInput'
  character(:), allocatable                                         ::    PrefixLoc
  character(:), allocatable                                         ::    DirectoryLoc
  character(:), allocatable                                         ::    DirectorySub
  logical                                                           ::    ExternalFlag=.false.
  character(:), allocatable                                         ::    SectionName
  character(:), allocatable                                         ::    SubSectionName
  integer                                                           ::    i

  if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

  DirectoryLoc = ''
  PrefixLoc = ''
  if ( present(Directory) ) DirectoryLoc = Directory
  if ( present(Prefix) ) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

  call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

  call GetInput%AddParameter( Name='nb_repetitions', Value=ConvertToString(Value=This%NbRepetitions) )

  SectionName = 'model'
  if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/model'
  call GetInput%AddSection( Section=Model_Factory%GetObjectInput(Object=This%Model, MainSectionName=SectionName,                &
                                                                                     Prefix=PrefixLoc, Directory=DirectorySub) )

  SectionName = 'parameter_space'
  if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/parameter_space'
  call GetInput%AddSection( Section=This%ParameterSpace%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                 &
                                                                                                       Directory=DirectorySub) )

  SectionName = 'responses'
  call GetInput%AddSection( SectionName=SectionName )
  do i = 1, size(This%Responses,1)
    SubSectionName = 'response' // ConvertToString(Value=i)
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/response' // ConvertToString(i)
    call GetInput%AddSection( Section=This%Responses(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,              &
                                                                            Directory=DirectorySub), To_SubSection=SectionName )
  end do

  SectionName = 'analysis'
  if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/analysis'
  call GetInput%AddSection( Section=AnalysisMethod_Factory%GetObjectInput(Object=This%AnalysisMethod,                             &
                                                        MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub) )

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
subroutine Run(This)

  class(Root_Type), intent(inout)                                   ::    This

  character(*), parameter                                           ::    ProcName='Run'
  integer                                                           ::    StatLoc=0
  character(:), allocatable                                         ::    OutputDirectory
  character(:), allocatable                                         ::    OutputDirectoryLoc
  integer                                                           ::    i
  character(:), allocatable                                         ::    Line
  character(:), allocatable                                         ::    LineLoc

  OutputDirectory = ProgramDefs%GetOutputDir()
  if (len_trim(OutputDirectory) > 0) OutputDirectory = OutputDirectory // '/analysis'
  OutputDirectoryLoc = OutputDirectory

  Line = 'Running Analysis'
  LineLoc = Line

  i = 1
  do i = 1, THis%NbRepetitions
    if ( This%NbRepetitions > 1 .and. len_trim(OutputDirectory) > 0 ) OutputDirectoryLoc = OutputDirectory // '_' //              &
                                                                                           ConvertToString(Value=i)
    if ( This%NbRepetitions > 1 ) LineLoc = Line // ' ' // ConvertToString(Value=i)
    write(*,*)
    write(*,'(A)') LineLoc
    call This%AnalysisMethod%Run(SampleSpace=This%ParameterSpace, Responses=This%Responses, Model=This%Model,                     &                                            
                                 OutputDirectory=OutputDirectoryLoc)
  end do

  call This%WriteOutput( Directory=ProgramDefs%GetOutputDir() )

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
subroutine WriteOutput(This, Directory)

  class(Root_Type), intent(inout)                                   ::    This
  character(*), intent(in)                                          ::    Directory

  character(*), parameter                                           ::    ProcName='WriteOutput'
  integer                                                           ::    StatLoc=0
  integer                                                           ::    i
  character(:), allocatable                                         ::    Label
  character(:), allocatable                                         ::    PrefixLoc
  type(String_Type), allocatable, dimension(:)                      ::    ResponseLabels
  integer                                                           ::    NbResponses
  type(SMUQFile_Type)                                               ::    File
  character(:), allocatable                                         ::    FileName

  if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

  if ( len_trim(Directory) /= 0 ) then

    call MakeDirectory( Path=Directory, Options='-p' )

    PrefixLoc = Directory // '/sample_space'
    call This%ParameterSpace%WriteInfo( Directory=PrefixLoc )

    NbResponses = size(This%Responses,1)
    allocate(ResponseLabels(NbResponses), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ResponseLabels(size(This%Responses,1))', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbResponses
      Label = This%Responses(i)%GetLabel()
      PrefixLoc = Directory // '/responses/' // Label
      call This%Responses(i)%WriteInfo( Directory=PrefixLoc )
      ResponseLabels(i) = Label
    end do

    PrefixLoc = Directory // '/responses'
    FileName = '/responses.dat'
    call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
    call File%Export( Strings=ResponseLabels )

    deallocate(ResponseLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ResponseLabels', ProcName=ProcName, stat=StatLoc )

  end if

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(Root_Type), intent(out)                                     ::    LHS
  class(Root_Type), intent(in)                                      ::    RHS

  character(*), parameter                                           ::    ProcName='Copy'
  integer                                                           ::    StatLoc=0

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbRepetitions = RHS%NbRepetitions
    allocate(LHS%Model, source=RHS%Model, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Model', ProcName=ProcName, stat=StatLoc)
    LHS%ParameterSpace = RHS%ParameterSpace
    allocate(LHS%AnalysisMethod, source=RHS%AnalysisMethod, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%AnalysisMethod', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Responses, source=RHS%Responses, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Response', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!-----------------------------------------------------------------------------

!!-----------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(Root_Type), intent(inout)                                    ::    This

  character(*), parameter                                           ::    ProcName='Finalizer'
  integer                                                           ::    StatLoc=0

  if (allocated(This%Model)) deallocate(This%Model, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Model', ProcName=ProcName, stat=StatLoc) 

  if (allocated(This%AnalysisMethod)) deallocate(This%AnalysisMethod, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%AnalysisMethod', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Responses)) deallocate(This%Responses, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Responses', ProcName=ProcName, stat=StatLoc)

end subroutine
!!-----------------------------------------------------------------------------

end module
