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

module HierCovFunction_Class

use Input_Library
use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovFunction_Class                                             ,only:    CovFunction_Type

implicit none

private

public                                                                ::    HierCovFunction_Type

type, abstract                                                        ::    HierCovFunction_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsConstructed
  procedure, public                                                   ::    IsInputRequired
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_HierCovFunction), deferred, public             ::    Initialize
  procedure(Reset_HierCovFunction), deferred, public                  ::    Reset
  procedure(SetDefaults_HierCovFunction), deferred, public            ::    SetDefaults
  procedure(ConstructInput_HierCovFunction), deferred, private        ::    ConstructInput
  procedure(GetInput_HierCovFunction), deferred, public               ::    GetInput
  procedure(Generate_HierCovFunction), deferred, public               ::    Generate
  procedure(Copy_HierCovFunction), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_HierCovFunction(This)
    import                                                            ::    HierCovFunction_Type
    class(HierCovFunction_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_HierCovFunction(This)
    import                                                            ::    HierCovFunction_Type
    class(HierCovFunction_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_HierCovFunction(This)
    import                                                            ::    HierCovFunction_Type
    class(HierCovFunction_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_HierCovFunction(This, Input, Prefix)
    import                                                            ::    HierCovFunction_Type
    import                                                            ::    InputSection_Type
    class(HierCovFunction_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_HierCovFunction(This, Name, Prefix, Directory)
    import                                                            ::    HierCovFunction_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_HierCovFunction
    class(HierCovFunction_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate_HierCovFunction(This, Input, CovFunction)
    import                                                            ::    HierCovFunction_Type
    import                                                            ::    Input_Type
    import                                                            ::    CovFunction_Type
    class(HierCovFunction_Type), intent(in)                           ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(CovFunction_Type), allocatable, intent(out)                 ::    CovFunction
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_HierCovFunction(LHS, RHS)
    import                                                            ::    HierCovFunction_Type
    class(HierCovFunction_Type), intent(out)                          ::    LHS
    class(HierCovFunction_Type), intent(in)                           ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName

    class(HierCovFunction_Type), intent(in)                           ::    This
    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed(This)

    logical                                                           ::    IsConstructed

    class(HierCovFunction_Type), intent(in)                           ::    This
    character(*), parameter                                           ::    ProcName='IsConstructed'

    IsConstructed = This%Constructed

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsInputRequired(This)

    logical                                                           ::    IsInputRequired

    class(HierCovFunction_Type), intent(in)                           ::    This
    character(*), parameter                                           ::    ProcName='IsInputRequired'

    IsInputRequired = This%InputRequired

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
