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

module MParamScalar_Factory_class

use Input_Library
use String_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamScalar_Class                                            ,only:    MParamScalar_Type
use MParamScalarDirect_Class                                      ,only:    MParamScalarDirect_Type

implicit none

private

public                                                                ::    MParamScalar_Factory

type                                                                  ::    MParamScalar_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D,          &
                                                                                                          Construct_Input
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, public                                                   ::    Construct_Input
  procedure, nopass, public                                           ::    GetOption
  procedure, public                                                   ::    GetObjectInput
end type

type(MParamScalar_Factory_Type)                                       ::    MParamScalar_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_C0D(Object, DesiredType)

    class(MParamScalar_Type), allocatable, intent(inout)              ::    Object
    character(*), intent(in)                                          ::    DesiredType

    character(*), parameter                                           ::    ProcName='Construct_C0D' 

    if (allocated(Object)) call Error%Raise(Line='Object already allocated', ProcName=ProcName)

    select case (LowerCase(DesiredType))

      case('direct')
        allocate(MParamScalarDirect_Type :: Object)

      case default
        call Error%Raise(Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName)

    end select

    call Object%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_Input(This, Object, Input, Prefix)
    
    use Input_Library

    class(MParamScalar_Factory_Type), intent(in)                      ::    This
    class(MParamScalar_Type), allocatable, intent(inout)              ::    Object
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='Construct_Input'                                   
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc=0 

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'type'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    call This%Construct(Object=Object, DesiredType=VarC0D)

    SectionName = 'type'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call Object%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOption(Object)

    character(:), allocatable                                         ::    GetOption

    class(MParamScalar_Type), intent(in)                              ::    Object                                                                                            

    character(*), parameter                                           ::    ProcName='GetOption' 

    select type (Object)

      type is (MParamScalarDirect_Type)
        GetOption = 'direct'

      class default
        call Error%Raise(Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName)

    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetObjectInput(This, Object, Name, Prefix, Directory)

    use Input_Library

    type(InputSection_Type)                                           ::    GetObjectInput

    class(MParamScalar_Factory_Type), intent(in)                      ::    This
    class(MParamScalar_Type), intent(in)                              ::    Object
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetObjectInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    integer                                                           ::    StatLoc=0
    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    DirectorySub = DirectoryLoc
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix

    call GetObjectInput%SetName(SectionName=Name)

    call GetObjectInput%AddParameter(Name='type', Value=This%GetOption(Object=Object))

    call GetObjectInput%AddSection(Section=Object%GetInput(Name='type', Prefix=PrefixLoc, Directory=DirectoryLoc))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
