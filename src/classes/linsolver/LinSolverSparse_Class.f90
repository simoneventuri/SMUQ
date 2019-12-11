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

module LinSolverSparse_Class

use Input_Library
use Parameters_Library
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type

implicit none

private

public                                                                ::    LinSolverSparse_Type

type, abstract, extends(LinSolverMethod_Type)                         ::    LinSolverSparse_Type

contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    SolveSystem
  generic, public                                                     ::    Solve                   =>    SolveSparse,            &
                                                                                                          SolveFull
  procedure(SolveSparse_LinSolverSparse), deferred, public            ::    SolveSparse
  procedure(SolveFull_LinSolverSparse), deferred, public              ::    SolveFull
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse_LinSolverSparse( This, System, Goal, ModelSet, CoefficientsSet, CVError )
    use Parameters_Library
    import                                                            ::    LinSolverSparse_Type
    class(LinSolverSparse_Type), intent(in)                           ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull_LinSolverSparse( This, System, Goal, Coefficients, CVError )
    use Parameters_Library
    import                                                            ::    LinSolverSparse_Type
    class(LinSolverSparse_Type), intent(in)                           ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName

    class(LinSolverSparse_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSystem( This, System, Goal, Coefficients, CVError )

    class(LinSolverSparse_Type), intent(in)                           ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError

    character(*), parameter                                           ::    ProcName='SolveSystem'
  
    if ( present(CVError) ) then
      call This%SolveFull( System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError )
    else
      call This%SolveFull( System=System, Goal=Goal, Coefficients=Coefficients )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
