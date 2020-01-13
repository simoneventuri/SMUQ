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

module TestFunction_Class

use Input_Library
use Parameters_Library
use Input_Class                                                   ,only:    Input_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    TestFunction_Type

type, abstract                                                        ::    TestFunction_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    GetLabel
  procedure(Initialize_TestFunction), deferred, public                ::    Initialize
  procedure(Reset_TestFunction), deferred, public                     ::    Reset
  procedure(SetDefaults_TestFunction), deferred, public               ::    SetDefaults
  procedure(ConstructInput_TestFunction), deferred, private           ::    ConstructInput
  procedure(GetInput_TestFunction), deferred, public                  ::    GetInput
  procedure(Run_TestFunction), deferred, public                       ::    Run
  procedure(Copy_TestFunction), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_TestFunction( This )
    import                                                            ::    TestFunction_Type
    class(TestFunction_Type), intent(inout)                           ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_TestFunction( This )
    import                                                            ::    TestFunction_Type
    class(TestFunction_Type), intent(inout)                           ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_TestFunction( This )
    import                                                            ::    TestFunction_Type
    class(TestFunction_Type), intent(inout)                           ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_TestFunction( This, Input, Prefix )
    import                                                            ::    TestFunction_Type
    import                                                            ::    InputSection_Type
    class(TestFunction_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_TestFunction( This, MainSectionName, Prefix, Directory )
    import                                                            ::    TestFunction_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_TestFunction
    class(TestFunction_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run_TestFunction( This, Input, Output )
    use Parameters_Library
    import                                                            ::    TestFunction_Type
    import                                                            ::    Input_Type
    import                                                            ::    Output_Type
    class(TestFunction_Type), intent(inout)                           ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), intent(inout)                                  ::    Output
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy_TestFunction( LHS, RHS )
    import                                                            ::    TestFunction_Type
    class(TestFunction_Type), intent(out)                             ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This )

    character(:), allocatable                                         ::    GetName

    class(TestFunction_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    if ( .not. This%Constructed ) call Error%Raise( 'Object not yet constructed', ProcName=ProcName )

    GetName = This%Name

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetLabel( This )

    character(:), allocatable                                         ::    GetLabel

    class(TestFunction_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='GetLabel'

    if ( .not. This%Constructed ) call Error%Raise( 'Object not yet constructed', ProcName=ProcName )

    GetLabel = This%Label

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
