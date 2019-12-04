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

module Root_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use AnalysisMethod_Class                                          ,only:    AnalysisMethod_Type
use AnalysisMethod_Factory_Class                                  ,only:    AnalysisMethod_Factory
use Response_Class                                                ,only:    Response_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use ModelExt_Factory_Class                                        ,only:    ModelExt_Factory
use ModelInterface_Class                                          ,only:    ModelInterface_Type

implicit none

private

public                                                                ::    Root_Type

type                                                                  ::    Root_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  class(AnalysisMethod_Type), allocatable                             ::    AnalysisMethod
  type(Response_Type), allocatable, dimension(:)                      ::    Response
  type(SpaceParam_Type)                                               ::    ParameterSpace
  class(ModelExtTemplate_Type), allocatable                           ::    Model
  character(:), allocatable                                           ::    SectionChain                                             
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(Root_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'root'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(Root_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    deallocate(This%AnalysisMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%AnalysisMethod', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Response, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Response', ProcName=ProcName, stat=StatLoc )

    call This%ParameterSpace%Reset()

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(Root_Type), intent(inout)                                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%SectionChain = ''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, SectionChain, Prefix, Debug )

    use StringRoutines_Module

    class(Root_Type), intent(inout)                                   ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    SectionName = 'plugin'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ModelExt_Factory%Construct( Object=This%Model, Input=InputSection, Prefix=PrefixLoc )
    nullify ( InputSection )

    SectionName = "parameter_space"
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%ParameterSpace%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'analysis'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call AnalysisMethod_Factory%Construct( Object=This%AnalysisMethod, Input=InputSection,                                        &
                                                                 SectionChain=This%SectionChain // '>analysis', Prefix=PrefixLoc )
    nullify ( InputSection )

    SectionName = "responses"
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    NbResponses = InputSection%GetNumberofSubSections()
    if ( NbResponses < 1 ) call Error%Raise( Line='Number of specified responses below minimum of 1', ProcName=ProcName )
    allocate( This%Response(NbResponses), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Responses', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbResponses
      SubSectionName = SectionName // ">response" // ConvertToString( Value=i )
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Response(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify ( InputSection )
    end do

    i = 1
    do i = 1, NbResponses
      Label1 = This%Response(i)%GetLabel()
      ii = i+1
      do ii = i + 1, NbResponses
        Label2 = This%Response(ii)%GetLabel()
        if ( Label1 /= Label2 ) cycle
        call Error%Raise( 'Detected duplicate label: ' // Label1, ProcName=ProcName )
      end do
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(Root_Type), intent(inout)                                   ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'plugin'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/plugin'
    call GetInput%AddSection( Section=ModelExt_Factory%GetObjectInput(Object=This%Model,                                          &
                                                          MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub) )

    SectionName = 'parameter_space'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/parameter_space'
    call GetInput%AddSection( Section=This%ParameterSpace%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                 &
                                                                                                         Directory=DirectorySub) )

    SectionName = 'responses'
    call GetInput%AddSection( SectionName=SectionName )
    do i = 1, size(This%Response,1)
      SubSectionName = 'response' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/response' // ConvertToString(i)
      call GetInput%AddSection( Section=This%Response(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,               &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )
    end do

    SectionName = 'analysis'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/analysis'
    call GetInput%AddSection( Section=AnalysisMethod_Factory%GetObjectInput(Object=This%AnalysisMethod,                           &
                                                          MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run( This, Debug )

    class(Root_Type), intent(inout)                                   ::    This

    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call This%AnalysisMethod%Run( SpaceInput=This%ParameterSpace, Response=This%Response, Model=This%Model,                       &
                                                                                      OutputDirectory=ProgramDefs%GetOutputDir() )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(Root_Type), intent(out)                                     ::    LHS
    class(Root_Type), intent(in)                                      ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (Root_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          allocate(LHS%Model, source=RHS%Model, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Model', ProcName=ProcName, stat=StatLoc )
          LHS%ParameterSpace = RHS%ParameterSpace
          allocate(LHS%AnalysisMethod, source=RHS%AnalysisMethod, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%AnalysisMethod', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Response, source=RHS%Response, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Response', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(Root_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    deallocate(This%AnalysisMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%AnalysisMethod', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Response, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Response', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
