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

module LowDiscSequence_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    LowDiscSequence_Type

type, abstract                                                        ::    LowDiscSequence_Type
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    GetSequence             =>    Get0D, &
                                                                                                          Get1D                   
  procedure(Reset_LowDiscSequence), deferred, public                  ::    Reset
  procedure(ConstructInput_LowDiscSequence), deferred, private        ::    ConstructInput
  procedure(GetInput_LowDiscSequence), deferred, public               ::    GetInput
  procedure(Get0D_LowDiscSequence), deferred, private                 ::    Get0D
  procedure(Get1D_LowDiscSequence), deferred, private                 ::    Get1D
  procedure(Copy_LowDiscSequence), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LowDiscSequence(This)
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_LowDiscSequence (This, Input, Prefix)
    import                                                            ::    LowDiscSequence_Type
    import                                                            ::    InputSection_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_LowDiscSequence(This, Name, Prefix, Directory)
    import                                                            ::    LowDiscSequence_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_LowDiscSequence
    class(LowDiscSequence_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Get0D_LowDiscSequence(This, Sequence, NbPoints, Offset)
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(in)                           ::    This
    real(rkp), contiguous, dimension(:), intent(inout)                ::    Sequence
    integer, intent(in)                                               ::    NbPoints
    integer, optional, intent(in)                                     ::    Offset                                          
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Get1D_LowDiscSequence(This, Sequence, NbPoints, NbDim, Offset)
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(in)                           ::    This
    real(rkp), contiguous, dimension(:,:), intent(inout)              ::    Sequence
    integer, intent(in)                                               ::    NbPoints
    integer, intent(in)                                               ::    NbDim
    integer, optional, intent(in)                                     ::    Offset                                          
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_LowDiscSequence(LHS, RHS)
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(out)                          ::    LHS
    class(LowDiscSequence_Type), intent(in)                           ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

end module
