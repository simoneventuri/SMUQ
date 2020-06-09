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

implicit none

private

public                                                                ::    LowDiscSequence_Type

type, abstract                                                        ::    LowDiscSequence_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    GetSequence             =>    Get0D, &
                                                                                                          Get1D                   
  procedure(Initialize_LowDiscSequence), deferred, public             ::    Initialize
  procedure(Reset_LowDiscSequence), deferred, public                  ::    Reset
  procedure(SetDefaults_LowDiscSequence), deferred, public            ::    SetDefaults
  procedure(ConstructInput_LowDiscSequence), deferred, private        ::    ConstructInput
  procedure(GetInput_LowDiscSequence), deferred, public               ::    GetInput
  procedure(Get0D_LowDiscSequence), deferred, private                 ::    Get0D
  procedure(Get1D_LowDiscSequence), deferred, private                 ::    Get1D
  procedure(Copy_LowDiscSequence), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_LowDiscSequence(This)
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LowDiscSequence(This)
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_LowDiscSequence(This)
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
  subroutine Get0D_LowDiscSequence(This, Sequence, Offset)
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), dimension(:), intent(inout)                            ::    Sequence
    class(LowDiscSequence_Type), intent(in)                           ::    This
    integer, optional, intent(in)                                     ::    Offset                                          
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Get1D_LowDiscSequence(This, Sequence, Offset)
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(in)                           ::    This
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Sequence
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

!!------------------------------------------------------------------------------------------------------------------------------
function GetName(This)

  character(:), allocatable                                           ::    GetName

  class(LowDiscSequence_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='GetName'

  GetName = This%Name

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
