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

module MFileInput_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    MFileInput_Type

type, abstract                                                        ::    MFileInput_Type
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Reset_MFileInput), deferred, public                       ::    Reset
  procedure(ConstructInput_MFileInput), deferred, private             ::    ConstructInput
  procedure(GetInput_MFileInput), deferred, public                    ::    GetInput
  procedure(WriteInput_MFileInput), deferred, public                  ::    WriteInput
  procedure(Copy_MFileInput), deferred, public                        ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_MFileInput(This)
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_MFileInput(This, Input, Prefix)
    import                                                            ::    MFileInput_Type
    import                                                            ::    InputSection_Type
    class(MFileInput_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_MFileInput(This, Name, Prefix, Directory)
    import                                                            ::    InputSection_Type
    import                                                            ::    MFileInput_Type
    type(InputSection_Type)                                           ::    GetInput_MFileInput
    class(MFileInput_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput_MFileInput(This, Input, Template, ProcessedTemplate, File)
    import                                                            ::    MFileInput_Type
    import                                                            ::    Input_Type
    import                                                            ::    SMUQString_Type
    import                                                            ::    SMUQFile_Type
    class(MFileInput_Type), intent(inout)                             ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(SMUQString_Type), dimension(:), intent(in)                   ::    Template
    type(SMUQString_Type), dimension(:), intent(inout)                ::    ProcessedTemplate
    type(SMUQFile_Type), intent(in)                                   ::    File
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_MFileInput(LHS, RHS)
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(out)                               ::    LHS
    class(MFileInput_Type), intent(in)                                ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

end module
