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

module IScalarValue_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    IScalarValue_Type

type, abstract                                                        ::    IScalarValue_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_IScalarValue), deferred, public                ::    Initialize
  procedure(Reset_IScalarValue), deferred, public                     ::    Reset
  procedure(SetDefaults_IScalarValue), deferred, public               ::    SetDefaults
  procedure(ConstructInput_IScalarValue), deferred, private           ::    ConstructInput
  procedure(GetInput_IScalarValue), deferred, public                  ::    GetInput
  procedure(GetValue_IScalarValue), deferred, public                  ::    GetValue
  procedure(GetCharValue_IScalarValue), deferred, public              ::    GetCharValue
  procedure(Copy_IScalarValue), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_IScalarValue(This)
    import                                                            ::    IScalarValue_Type
    class(IScalarValue_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_IScalarValue(This)
    import                                                            ::    IScalarValue_Type
    class(IScalarValue_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_IScalarValue(This)
    import                                                            ::    IScalarValue_Type
    class(IScalarValue_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_IScalarValue(This, Input, Prefix)
    import                                                            ::    IScalarValue_Type
    import                                                            ::    InputSection_Type
    class(IScalarValue_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_IScalarValue(This, Name, Prefix, Directory)
    import                                                            ::    IScalarValue_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_IScalarValue
    class(IScalarValue_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue_IScalarValue(This, Input)
    use Parameters_Library
    import                                                            ::    Input_Type
    import                                                            ::    IScalarValue_Type  
    real(rkp)                                                         ::    GetValue_IScalarValue
    class(IScalarValue_Type), intent(in)                              ::    This
    type(Input_Type, intent(in)                                       ::    Input
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue_IScalarValue(This, Input, Format)
    use Parameters_Library
    import                                                            ::    Input_Type
    import                                                            ::    IScalarValue_Type
    character(:), allocatable                                         ::    GetCharValue_IScalarValue
    class(IScalarValue_Type), intent(in)                              ::    This
    type(Input_Type, intent(in)                                       ::    Input
    character(*), optional, intent(in)                                ::    Format
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_IScalarValue(LHS, RHS)
    import                                                            ::    IScalarValue_Type
    class(IScalarValue_Type), intent(out)                             ::    LHS
    class(IScalarValue_Type), intent(in)                              ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!------------------------------------------------------------------------------------------------------------------------------
function GetName(This)

  character(:), allocatable                                         ::    GetName
  class(IScalarValue_Type), intent(inout)                           ::    This

  character(*), parameter                                           ::    ProcName='GetName'

  GetName = This%Name

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
