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

module Model_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    Model_Type

type, abstract                                                        ::    Model_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    Run                     =>    RunCase1
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure(Initialize_Model), deferred, public                       ::    Initialize
  procedure(Reset_Model), deferred, public                            ::    Reset
  procedure(SetDefaults_Model), deferred, public                      ::    SetDefaults
  procedure(RunCase1_Model), deferred, public                         ::    RunCase1
  procedure(Copy_Model), deferred, public                             ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_Model( This )
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_Model( This )
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_Model( This )
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine RunCase1_Model( This, Input, Output, Stat )
    use Parameters_Library
    use Output_Class                                              ,only:    Output_Type
    use Input_Class                                               ,only:    Input_Type
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy_Model( LHS, RHS )
    import                                                            ::    Model_Type
    class(Model_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(Model_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
