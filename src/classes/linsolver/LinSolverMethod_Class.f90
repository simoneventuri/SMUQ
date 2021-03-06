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

module LinSolverMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error

implicit none

private

public                                                                ::    LinSolverMethod_Type

type, abstract                                                        ::    LinSolverMethod_Type
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Reset_LinSolverMethod), deferred, public                  ::    Reset
  procedure(ConstructInput_LinSolverMethod), deferred, private        ::    ConstructInput
  procedure(GetInput_LinSolverMethod), deferred, public               ::    GetInput
  procedure(Solve_LinSolverMethod), deferred, public                  ::    Solve
  procedure(Copy_LinSolverMethod), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LinSolverMethod(This)
    import                                                            ::    LinSolverMethod_Type
    class(LinSolverMethod_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_LinSolverMethod(This, Input, Prefix)
    import                                                            ::    LinSolverMethod_Type
    import                                                            ::    InputSection_Type
    class(LinSolverMethod_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Solve_LinSolverMethod(This, System, Goal, Coefficients, CVError)
    use Parameters_Library
    import                                                            ::    LinSolverMethod_Type
    class(LinSolverMethod_Type), intent(in)                           ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), dimension(:), intent(inout)                            ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_LinSolverMethod(This, Name, Prefix, Directory)
    import                                                            ::    LinSolverMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_LinSolverMethod
    class(LinSolverMethod_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_LinSolverMethod(LHS, RHS)
    import                                                            ::    LinSolverMethod_Type
    class(LinSolverMethod_Type), intent(out)                          ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

end module
