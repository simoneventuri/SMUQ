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

module ITableValue_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    ITableValue_Type

type, abstract                                                        ::    ITableValue_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_ITableValue), deferred, public                 ::    Initialize
  procedure(Reset_ITableValue), deferred, public                      ::    Reset
  procedure(SetDefaults_ITableValue), deferred, public                ::    SetDefaults
  procedure(ConstructInput_ITableValue), deferred, private            ::    ConstructInput
  procedure(GetInput_ITableValue), deferred, public                   ::    GetInput
  procedure(GetValue_ITableValue), deferred, public                   ::    GetValue
  procedure(GetCharValue_ITableValue), deferred, public               ::    GetCharValue
  procedure(Copy_ITableValue), deferred, public                       ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_ITableValue(This, Input, Prefix)
    import                                                            ::    ITableValue_Type
    import                                                            ::    InputSection_Type
    class(ITableValue_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_ITableValue(This, Name, Prefix, Directory)
    import                                                            ::    ITableValue_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_ITableValue
    class(ITableValue_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue_ITableValue(This, Input, Abscissa)
    use Parameters_Library
    import                                                            ::    Input_Type
    import                                                            ::    ITableValue_Type  
    real(rkp), allocatable, dimension(:)                              ::    GetValue_ITableValue
    class(ITableValue_Type), intent(in)                               ::    This
    type(Input_Type, intent(in)                                       ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue_ITableValue(This, Input, Abscissa, Format)
    use Parameters_Library
    use String_Library
    import                                                            ::    Input_Type
    import                                                            ::    ITableValue_Type
    type(String_Type), allocatable, dimension(:)                      ::    GetCharValue_ITableValue
    class(ITableValue_Type), intent(in)                               ::    This
    type(Input_Type, intent(in)                                       ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    character(*), optional, intent(in)                                ::    Format
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_ITableValue(LHS, RHS)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(out)                              ::    LHS
    class(ITableValue_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!--------------------------------------------------------------------------------------------------------------------------------
function GetName(This)

  character(:), allocatable                                             ::    GetName
  class(ITableValue_Type), intent(inout)                                ::    This

  character(*), parameter                                               ::    ProcName='GetName'

  GetName = This%Name

end function
!!--------------------------------------------------------------------------------------------------------------------------------

end module
