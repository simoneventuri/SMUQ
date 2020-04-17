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

module SurrogateMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type

implicit none

private

public                                                                ::    SurrogateMethod_Type

type, abstract                                                        ::    SurrogateMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_SurrogateMethod), deferred, public             ::    Initialize
  procedure(Reset_SurrogateMethod), deferred, public                  ::    Reset
  procedure(SetDefaults_SurrogateMethod), deferred, public            ::    SetDefaults
  procedure(ConstructInput_SurrogateMethod), deferred, private        ::    ConstructInput
  procedure(GetInput_SurrogateMethod), deferred, public               ::    GetInput
  procedure(Run_SurrogateMethod), deferred, public                    ::    Run
  procedure(Copy_SurrogateMethod), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SurrogateMethod(This)
    import                                                            ::    SurrogateMethod_Type
    class(SurrogateMethod_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SurrogateMethod(This)
    import                                                            ::    SurrogateMethod_Type
    class(SurrogateMethod_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SurrogateMethod(This)
    import                                                            ::    SurrogateMethod_Type
    class(SurrogateMethod_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SurrogateMethod(This, Input, SectionChain, Prefix)
    import                                                            ::    SurrogateMethod_Type
    import                                                            ::    InputSection_Type
    class(SurrogateMethod_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SurrogateMethod(This, Name, Prefix, Directory)
    import                                                            ::    SurrogateMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_SurrogateMethod
    class(SurrogateMethod_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_SurrogateMethod(This, SampleSpace, Responses, Model, SurrogateModel, OutputDirectory)
    import                                                            ::    Response_Type
    import                                                            ::    Model_Type
    import                                                            ::    SurrogateMethod_Type
    import                                                            ::    SampleSpace_Type
    class(SurrogateMethod_Type), intent(inout)                        ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    class(Model_Type), allocatable, dimension(:),optional,intent(out) ::    SurrogateModel
    character(*), optional, intent(in)                                ::    OutputDirectory
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_SurrogateMethod(LHS, RHS)
    import                                                            ::    SurrogateMethod_Type
    class(SurrogateMethod_Type), intent(out)                          ::    LHS
    class(SurrogateMethod_Type), intent(in)                           ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(SurrogateMethod_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
