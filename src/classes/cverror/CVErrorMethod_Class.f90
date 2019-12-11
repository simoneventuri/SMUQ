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

module CVErrorMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type

implicit none

private

public                                                                ::    CVErrorMethod_Type

type, abstract                                                        ::    CVErrorMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Corrected=.true.
  logical                                                             ::    Normalized=.true.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    IsCorrected
  procedure, public                                                   ::    IsNormalized
  procedure(Initialize_CVErrorMethod), deferred, public               ::    Initialize
  procedure(Reset_CVErrorMethod), deferred, public                    ::    Reset
  procedure(SetDefaults_CVErrorMethod), deferred, public              ::    SetDefaults
  procedure(ConstructInput_CVErrorMethod), deferred, private          ::    ConstructInput
  procedure(GetInput_CVErrorMethod), deferred, public                 ::    GetInput
  procedure(ComputeError_CVErrorMethod), deferred, public             ::    ComputeError
  procedure(Copy_CVErrorMethod), deferred, public                     ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_CVErrorMethod( This )
    import                                                            ::    CVErrorMethod_Type
    class(CVErrorMethod_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_CVErrorMethod( This )
    import                                                            ::    CVErrorMethod_Type
    class(CVErrorMethod_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_CVErrorMethod( This )
    import                                                            ::    CVErrorMethod_Type
    class(CVErrorMethod_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_CVErrorMethod( This, Input, Prefix )
    import                                                            ::    CVErrorMethod_Type
    import                                                            ::    InputSection_Type
    class(CVErrorMethod_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_CVErrorMethod( This, MainSectionName, Prefix, Directory )
    import                                                            ::    CVErrorMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_CVErrorMethod
    class(CVErrorMethod_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeError_CVErrorMethod( This, Solver, System, Goal, Coefficients )
    use Parameters_Library
    import                                                            ::    LinSolverMethod_Type
    import                                                            ::    CVErrorMethod_Type
    real(rkp)                                                         ::    ComputeError_CVErrorMethod
    class(CVErrorMethod_Type), intent(in)                             ::    This
    class(LinSolverMethod_Type), intent(in)                           ::    Solver
    real(rkp), dimension(:,:), intent(in)                             ::    System
    real(rkp), dimension(:), intent(in)                               ::    Goal
    real(rkp), dimension(:), intent(in)                               ::    Coefficients
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_CVErrorMethod( LHS, RHS )
    import                                                            ::    CVErrorMethod_Type
    class(CVErrorMethod_Type), intent(out)                            ::    LHS
    class(CVErrorMethod_Type), intent(in)                             ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName

    class(CVErrorMethod_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetName'    

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrected( This )

    logical                                                           ::    IsCorrected

    class(CVErrorMethod_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='IsCorrected'

    IsCorrected = This%Corrected

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsNormalized( This )

    logical                                                           ::    IsNormalized

    class(CVErrorMethod_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='IsNormalized'

    IsNormalized = This%Normalized

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
