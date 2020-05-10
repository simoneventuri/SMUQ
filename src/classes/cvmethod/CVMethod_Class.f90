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

module CVMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    CVMethod_Type
public                                                                ::    FitTarget

type, abstract                                                        ::    CVMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Normalized=.true.
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    IsNormalized
  procedure(Initialize_CVMethod), deferred, public                    ::    Initialize
  procedure(Reset_CVMethod), deferred, public                         ::    Reset
  procedure(SetDefaults_CVMethod), deferred, public                   ::    SetDefaults
  procedure(ConstructInput_CVMethod), deferred, private               ::    ConstructInput
  procedure(GetInput_CVMethod), deferred, public                      ::    GetInput
  procedure(ComputeError_CVMethod), deferred, public                  ::    Calculate
  procedure(Copy_CVMethod), deferred, public                          ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_CVMethod(This)
    import                                                            ::    CVMethod_Type
    class(CVMethod_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_CVMethod(This)
    import                                                            ::    CVMethod_Type
    class(CVMethod_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_CVMethod(This)
    import                                                            ::    CVMethod_Type
    class(CVMethod_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_CVMethod(This, Input, Prefix)
    import                                                            ::    CVMethod_Type
    import                                                            ::    InputSection_Type
    class(CVMethod_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_CVMethod(This, Name, Prefix, Directory)
    import                                                            ::    CVMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_CVMethod
    class(CVMethod_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Calculate_CVMethod(This, Fit, FitData)
    use Parameters_Library
    import                                                            ::    CVMethod_Type
    import                                                            ::    FitTarget
    class(CVMethod_Type), intent(in)                                  ::    This
    procedure(FitTarget), pointer                                     ::    Fit 
    real(rkp), dimension(:), intent(in)                               ::    FitData
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_CVMethod(LHS, RHS)
    import                                                            ::    CVMethod_Type
    class(CVMethod_Type), intent(out)                                 ::    LHS
    class(CVMethod_Type), intent(in)                                  ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!--------------------------------------------------------------------------------------------------------------------------------
  subroutine FitTarget(TrainingSet, TrainingSetIndices, ValidationSet, ValidationSetIndices, Residual)
    use Parameters_Library
    real(rkp), dimension(:), intent(inout)                            ::    TrainingSet
    integer, dimension(:), intent(in)                                 ::    TrainingSetIndices
    real(rkp), dimension(:), intent(inout)                            ::    ValidationSet
    integer, dimension(:), intent(in)                                 ::    ValidationSetIndices
    real(rkp), dimension(:), intent(inout)                            ::    Residual
  end subroutine
  !!--------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!--------------------------------------------------------------------------------------------------------------------------------
function IsNormalized(This)

  logical                                                             ::    IsNormalized

  class(CVMethod_Type), intent(in)                                    ::    This

  character(*), parameter                                             ::    ProcName='IsNormalized'

  IsNormalized = This%Normalized

end function
!!--------------------------------------------------------------------------------------------------------------------------------

end module
