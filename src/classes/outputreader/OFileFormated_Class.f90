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

module OFileFormated_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use String_Library

implicit none

private

public                                                                ::    OFileFormated_Type

type, abstract                                                        ::    OFileFormated_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(SMUQFile_Type)                                                 ::    OutputFile
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    Exists
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_OFileFormated), deferred, public               ::    Initialize
  procedure(Reset_OFileFormated), deferred, public                    ::    Reset
  procedure(SetDefaults_OFileFormated), deferred, public              ::    SetDefaults
  procedure(ConstructInput_OFileFormated), deferred, private          ::    ConstructInput
  procedure(GetInput_OFileFormated), deferred, public                 ::    GetInput
  procedure(ReadOutput_OFileFormated), deferred, public               ::    ReadOutput
  procedure(Copy_OFileFormated), deferred, public                     ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_OFileFormated(This)
    import                                                            ::    OFileFormated_Type  
    class(OFileFormated_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_OFileFormated(This)
    import                                                            ::    OFileFormated_Type  
    class(OFileFormated_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_OFileFormated(This)
    import                                                            ::    OFileFormated_Type  
    class(OFileFormated_Type), intent(inout)                          ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_OFileFormated(This, Input, Prefix)
    import                                                            ::    OFileFormated_Type
    import                                                            ::    InputSection_Type
    class(OFileFormated_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_OFileFormated(This, Name, Prefix, Directory)
    import                                                            ::    OFileFormated_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_OFileFormated
    class(OFileFormated_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput_OFileFormated(This, Values)
    use Output_Class                                              ,only:    Output_Type
    use Parameters_Library
    import                                                            ::    OFileFormated_Type
    class(OFileFormated_Type), intent(in)                             ::    This
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Values
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_OFileFormated(LHS, RHS)
    import                                                            ::    OFileFormated_Type  
    class(OFileFormated_Type), intent(out)                            ::    LHS
    class(OFileFormated_Type), intent(in)                             ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(OFileFormated_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Exists(This)

    logical, allocatable                                              ::    Exists

    class(OFileFormated_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Exists'

    Exists = This%OutputFile%Exists()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
