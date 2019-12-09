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

module UQMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type

implicit none

private

public                                                                ::    UQMethod_Type

type, abstract                                                        ::    UQMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_UQMethod), deferred, public                    ::    Initialize
  procedure(Reset_UQMethod), deferred, public                         ::    Reset
  procedure(SetDefaults_UQMethod), deferred, public                   ::    SetDefaults
  procedure(ConstructInput_UQMethod), deferred, private               ::    ConstructInput
  procedure(GetInput_UQMethod), deferred, public                      ::    GetInput
  procedure(Run_UQMethod), deferred, public                           ::    Run
  procedure(Copy_UQMethod), deferred, public                          ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_UQMethod( This, Debug )
    import                                                            ::    UQMethod_Type
    class(UQMethod_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_UQMethod( This, Debug )
    import                                                            ::    UQMethod_Type
    class(UQMethod_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_UQMethod( This, Debug )
    import                                                            ::    UQMethod_Type
    class(UQMethod_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_UQMethod( This, Input, SectionChain, Prefix, Debug )
    import                                                            ::    UQMethod_Type
    import                                                            ::    InputSection_Type
    class(UQMethod_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_UQMethod( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    UQMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_UQMethod
    class(UQMethod_Type), intent(inout)                               ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_UQMethod( This, SampleSpace, Responses, Model, OutputDirectory, Debug )
    import                                                            ::    Response_Type
    import                                                            ::    SampleSpace_Type
    import                                                            ::    Model_Type
    import                                                            ::    UQMethod_Type
    class(UQMethod_Type), intent(inout)                               ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_UQMethod( LHS, RHS )
    import                                                            ::    UQMethod_Type
    class(UQMethod_Type), intent(out)                                 ::    LHS
    class(UQMethod_Type), intent(in)                                  ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(UQMethod_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
