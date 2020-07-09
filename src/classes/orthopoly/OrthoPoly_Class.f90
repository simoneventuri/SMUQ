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

module OrthoPoly_class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error

implicit none

private

public                                                                ::    OrthoPoly_Type

type, abstract                                                        ::    OrthoPoly_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  real(rkp)                                                           ::    polyorderm1 = Zero
  real(rkp)                                                           ::    polyorder0 = One
  logical                                                             ::    Normalized = .false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Eval                    =>    Eval_N,             &
                                                                                                          Eval_MN
  procedure(Initialize_OrthoPoly), deferred, public                   ::    Initialize
  procedure(Reset_OrthoPoly), deferred, public                        ::    Reset
  procedure(SetDefaults_OrthoPoly), deferred, public                  ::    SetDefaults
  procedure(Eval_N_OrthoPoly), deferred, public                       ::    Eval_N
  procedure(Eval_MN_OrthoPoly), deferred, public                      ::    Eval_MN
  procedure(ConstructInput_OrthoPoly), deferred, private              ::    ConstructInput
  procedure(GetInput_OrthoPoly), deferred, public                     ::    GetInput
  procedure(Copy_OrthoPoly), deferred, public                         ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_OrthoPoly(This)
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_OrthoPoly(This)
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_OrthoPoly(This)
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_OrthoPoly(This, Input, Prefix)
    import                                                            ::    OrthoPoly_Type
    import                                                            ::    InputSection_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_OrthoPoly(This, Name, Prefix, Directory)
    import                                                            ::    OrthoPoly_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_OrthoPoly
    class(OrthoPoly_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eval_N_OrthoPoly(This, Order, X, Value, Normalized)
    use Parameters_Library
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    Order
    real(rkp), intent(out)                                            ::    Value
    logical, optional, intent(in)                                     ::    Normalized
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eval_MN_OrthoPoly(This, MinOrder, MaxOrder, X, Values, Normalized)
    use Parameters_Library
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(inout)                              ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    MinOrder
    integer, intent(in)                                               ::    MaxOrder
    real(rkp), dimension(:), intent(inout)                            ::    Values
    logical, optional, intent(in)                                     ::    Normalized
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_OrthoPoly(LHS, RHS)
    import                                                            ::    OrthoPoly_Type
    class(OrthoPoly_Type), intent(out)                                ::    LHS
    class(OrthoPoly_Type), intent(in)                                 ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(OrthoPoly_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
