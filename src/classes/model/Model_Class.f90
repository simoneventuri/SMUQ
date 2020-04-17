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
use InputProcessor_Class                                          ,only:    InputProcessor_Type
use Output_Class                                                  ,only:    Output_Type

implicit none

private

public                                                                ::    Model_Type

type, abstract                                                        ::    Model_Type
  character(:), allocatable                                           ::    Name
  character(:), allocatable                                           ::    Label
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbOutputs
  logical                                                             ::    Silent
  type(InputProcessor_Type)                                           ::    InputProcessor
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetLabel
  procedure, public                                                   ::    GetNbOutputs
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Run                     =>    RunPreprocess_0D,       &
                                                                                                          RunPreprocess_1D
  procedure, private                                                  ::    RunPreprocess_0D
  procedure, private                                                  ::    RunPreprocess_1D
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure(Initialize_Model), deferred, public                       ::    Initialize
  procedure(Reset_Model), deferred, public                            ::    Reset
  procedure(SetDefaults_Model), deferred, public                      ::    SetDefaults
  procedure(ConstructInput_Model), deferred, private                  ::    ConstructInput
  procedure(GetInput_Model), deferred, public                         ::    GetInput
  procedure(Run_0D_Model), deferred, public                           ::    Run_0D
  procedure(Run_1D_Model), deferred, public                           ::    Run_1D
  procedure(Copy_Model), deferred, public                             ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Model(This)
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Model(This)
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Model(This)
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Model(This, Input, Prefix)
    import                                                            ::    Model_Type
    import                                                            ::    InputSection_Type
    class(Model_Type), intent(inout)                                  ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Model(This, Name, Prefix, Directory)
    import                                                            ::    Model_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_Model
    class(Model_Type), intent(in)                                     ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D_Model(This, Input, Output, Stat)
    import                                                            ::    Output_Type
    import                                                            ::    Input_Type
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_1D_Model(This, Input, Output, Stat)
    import                                                            ::    Output_Type
    import                                                            ::    Input_Type
    import                                                            ::    Model_Type
    class(Model_Type), intent(inout)                                  ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_Model(LHS, RHS)
    import                                                            ::    Model_Type
    class(Model_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(Model_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel(This)

    character(:), allocatable                                         ::    GetLabel
    class(Model_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetLabel'

    GetLabel = This%Label

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs(This)

    integer                                                           ::    GetNbOutputs
    class(Model_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetNbOutputs'

    GetNbOutputs = This%NbOutputs

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunPreProcess_0D(This, Input, Output, Stat)
 
    class(Model_Type), intent(inout)                                  ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='RunPreProcess_0D'
    integer                                                           ::    StatLoc=0
    type(Input_Type)                                                  ::    InputLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    if (This%InputProcessor%IsConstructed()) then
      call This%InputProcessor%ProcessInput(Input=Input, ProcessedInput=InputLoc)
      if (present(Stat)) then
        call This%Run_0D(Input=InputLoc, Output=Output, Stat=Stat)
      else
        call This%Run_0D(Input=InputLoc, Output=Output)
      end if
    else
      if (present(Stat)) then
        call This%Run_0D(Input=Input, Output=Output, Stat=Stat)
      else
        call This%Run_0D(Input=Input, Output=Output)
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunPreprocess_1D(This, Input, Output, Stat)
 
    class(Model_Type), intent(inout)                                  ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat

    character(*), parameter                                           ::    ProcName='RunPreProcess_1D'
    integer                                                           ::    StatLoc=0
    type(Input_Type), allocatable, dimension(:)                       ::    InputLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    if (This%InputProcessor%IsConstructed()) then
      call This%InputProcessor%ProcessInput(Input=Input, ProcessedInput=InputLoc)
      if (present(Stat)) then
        call This%Run_1D(Input=InputLoc, Output=Output, Stat=Stat)
      else
        call This%Run_1D(Input=InputLoc, Output=Output)
      end if
      deallocate(InputLoc, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='InputLoc', ProcName=ProcName, stat=StatLoc)
    else
      if (present(Stat)) then
        call This%Run_1D(Input=Input, Output=Output, Stat=Stat)
      else
        call This%Run_1D(Input=Input, Output=Output)
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
