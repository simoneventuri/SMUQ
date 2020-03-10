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

module MCMCMethod_Factory_Class

use Input_Library
use String_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MCMCMethod_Class                                              ,only:    MCMCMethod_Type
use MCMCDRAM_Class                                                ,only:    MCMCDRAM_Type

implicit none

private

public                                                                ::    MCMCMethod_Factory

type                                                                  ::    MCMCMethod_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D,          &
                                                                                                          Construct_Input
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, public                                                   ::    Construct_Input
  procedure, nopass, public                                           ::    GetOption
  procedure, public                                                   ::    GetObjectInput
End Type

type(MCMCMethod_Factory_Type)                                         ::    MCMCMethod_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_C0D( Object, DesiredType )

    class(MCMCMethod_Type), allocatable, intent(inout)                ::    Object                                             
    character(*), intent(in)                                          ::    DesiredType                                               

    character(*), parameter                                           ::    ProcName='Construct_C0D' 

    if ( allocated( Object ) ) call Error%Raise( Line="Object already allocated", ProcName=ProcName )

    select case ( LowerCase(DesiredType) )

      case('dram')
        allocate( MCMCDRAM_Type :: Object )

      case default
        call Error%Raise( Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName )

    end select

    call Object%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_Input( This, Object, Input, SectionChain, Prefix, Mandatory )
    
    use Input_Library

    class(MCMCMethod_Factory_Type), intent(in)                        ::    This
    class(MCMCMethod_Type), allocatable, intent(inout)                ::    Object
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Mandatory

    character(*), parameter                                           ::    ProcName='Construct_Input'                                   
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc=0
    logical                                                           ::    Found
    logical                                                           ::    MandatoryLoc 

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'type'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    call This%Construct( Object=Object, DesiredType=VarC0D )

    SectionName = 'type'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=MandatoryLoc,                 &
                                                                                                              FoundSection=Found )
    if ( Found ) then
      call Object%Construct( Input=InputSection, SectionChain=SectionChain, Prefix=PrefixLoc )
      nullify(InputSection)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOption( Object )

    character(:), allocatable                                         ::    GetOption

    class(MCMCMethod_Type), intent(in)                                ::    Object                                                                                            

    character(*), parameter                                           ::    ProcName='GetOption' 

    select type (Object)

      type is (MCMCDRAM_Type)
        GetOption = 'dram'

      class default
        call Error%Raise( Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName )

    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetObjectInput( This, Object, MainSectionName, Prefix, Directory, Mandatory )

    use Input_Library

    type(InputSection_Type)                                           ::    GetObjectInput

    class(MCMCMethod_Factory_Type), intent(in)                        ::    This
    class(MCMCMethod_Type), intent(in)                                ::    Object
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Mandatory

    character(*), parameter                                           ::    ProcName='GetObjectInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    integer                                                           ::    StatLoc=0
    logical                                                           ::    MandatoryLoc

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    DirectorySub = DirectoryLoc
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    MandatoryLoc = .true.
    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    call GetObjectInput%SetName( SectionName=MainSectionName )

    call GetObjectInput%AddParameter( Name='type', Value=This%GetOption( Object=Object ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/type'

    if ( Object%IsConstructed() ) then
      call GetObjectInput%AddSection( Section=Object%GetInput( MainSectionName='type', Prefix=PrefixLoc, Directory=DirectorySub ))
    else
      if ( MandatoryLoc ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
