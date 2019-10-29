! -*-f90-*-
!!!-------------------------------------------------------------------------------------------------------------------------------
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
!!!-------------------------------------------------------------------------------------------------------------------------------

module SpaceTransf_Factory_Class

use Input_Library
use String_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransfStdNormal_Class                                    ,only:    SpaceTransfStdNormal_Type
use SpaceTransfCustom_Class                                       ,only:    SpaceTransfCustom_Type
use SpaceTransfNone_Class                                         ,only:    SpaceTransfNone_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type

implicit none

private

public                                                                ::    SpaceTransf_Factory

type                                                                  ::    SpaceTransf_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D,          &
                                                                                                          Construct_Input
  generic, public                                                     ::    ConstructPointer        =>    ConstructPointer_C0D,   &
                                                                                                          ConstructPointer_Input
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, public                                                   ::    Construct_Input
  procedure, nopass, public                                           ::    ConstructPointer_C0D
  procedure, public                                                   ::    ConstructPointer_Input
  procedure, nopass, public                                           ::    GetOption
  procedure, public                                                   ::    GetObjectInput
End Type

type(SpaceTransf_Factory_Type)                                        ::    SpaceTransf_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_C0D( Object, DesiredType, Debug )

    class(SpaceTransf_Type), allocatable, intent(inout)               ::    Object                                            
    character(*), intent(in)                                          ::    DesiredType
    logical, optional, intent(in)                                     ::    Debug                                               

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Construct_C0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( allocated( Object ) ) call Error%Raise( Line="Object already allocated", ProcName=ProcName )

    select case ( LowerCase(DesiredType) )

      case('stdnormal')
        allocate( SpaceTransfStdNormal_Type :: Object )

      case('custom')
        allocate( SpaceTransfCustom_Type :: Object )

      case('None')
        allocate( SpaceTransfNone_Type :: Object )

      case default
        call Error%Raise( Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName )

    end select

    call Object%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_Input( This, Object, Input, SpaceInput, Prefix, Debug )
    
    use Input_Library

    class(SpaceTransf_Factory_Type), intent(in)                       ::    This
    class(SpaceTransf_Type), allocatable, intent(inout)               ::    Object
    type(InputSection_Type), intent(in)                               ::    Input
    type(SpaceParam_Type), optional, intent(in)                       ::    SpaceInput
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Construct_Input'                                   
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'type'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    call This%Construct( Object=Object, DesiredType=VarC0D )

    SectionName = 'type'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    if ( present(SpaceInput) ) then
      call Object%Construct( Input=InputSection, SpaceInput=SpaceInput, Prefix=PrefixLoc )
    else
      call Object%Construct( Input=InputSection, Prefix=PrefixLoc )
    end if

    nullify( InputSection )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructPointer_C0D( Object, DesiredType, Debug )

    class(SpaceTransf_Type), pointer, intent(inout)                   ::    Object                                            
    character(*), intent(in)                                          ::    DesiredType
    logical, optional, intent(in)                                     ::    Debug                                               

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructPointer_C0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    if ( associated( Object ) ) call Error%Raise( Line="Object already associated", ProcName=ProcName )

    select case ( LowerCase(DesiredType) )

      case('stdnormal')
        allocate( SpaceTransfStdNormal_Type ::Object )

      case('custom')
        allocate( SpaceTransfCustom_Type :: Object )

      case('None')
        allocate( SpaceTransfNone_Type :: Object )

      case default
        call Error%Raise( Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName )

    end select

    call Object%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructPointer_Input( This, Object, Input, SpaceInput, Prefix, Debug )
    
    use Input_Library

    class(SpaceTransf_Factory_Type), intent(in)                       ::    This
    class(SpaceTransf_Type), pointer, intent(inout)                   ::    Object
    type(InputSection_Type), intent(in)                               ::    Input
    type(SpaceParam_Type), optional, intent(in)                       ::    SpaceInput
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructPointer_Input'                                   
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'type'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    call This%ConstructPointer( Object=Object, DesiredType=VarC0D )

    SectionName = 'type'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    if ( present(SpaceInput) ) then
      call Object%Construct( Input=InputSection, SpaceInput=SpaceInput, Prefix=PrefixLoc )
    else
      call Object%Construct( Input=InputSection, Prefix=PrefixLoc )
    end if
    nullify( InputSection )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOption( Object, Debug )

    character(:), allocatable                                         ::    GetOption

    class(SpaceTransf_Type), intent(in)                               ::    Object                                             
    logical, optional, intent(in)                                     ::    Debug                                               

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOption'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName ) 

    select type (Object)

      type is (SpaceTransfStdNormal_Type)
        GetOption = 'stdnormal'

      type is (SpaceTransfCustom_Type)
        GetOption = 'custom'

      type is (SpaceTransfNone_Type)
        GetOption = 'none'

      class default
        call Error%Raise( Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetObjectInput( This, Object, MainSectionName, Prefix, Directory, Debug )

    use Input_Library

    type(InputSection_Type)                                           ::    GetObjectInput

    class(SpaceTransf_Factory_Type), intent(in)                       ::    This
    class(SpaceTransf_Type), intent(in)                               ::    Object
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetObjectInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetObjectInput%SetName( SectionName=MainSectionName )

    call GetObjectInput%AddParameter( Name='type', Value=This%GetOption( Object=Object ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/type'

    call GetObjectInput%AddSection( Section=Object%GetInput( MainSectionName='type', Prefix=PrefixLoc, Directory=DirectorySub ) )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
