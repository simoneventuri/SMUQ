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

use String_Library
use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    MFileInput_Type

type, abstract                                                        ::    MFileInput_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_MFileInput), deferred, public                  ::    Initialize
  procedure(Reset_MFileInput), deferred, public                       ::    Reset
  procedure(SetDefaults_MFileInput), deferred, public                 ::    SetDefaults
  procedure(ConstructInput_MFileInput), deferred, private             ::    ConstructInput
  procedure(GetInput_MFileInput), deferred, public                    ::    GetInput
  procedure(WriteInput_MFileInput), deferred, public                  ::    WriteInput
  procedure(Copy_MFileInput), deferred, public                        ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_MFileInput( This )
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_MFileInput( This )
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_MFileInput( This )
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_MFileInput( This, Input, Prefix )
    import                                                            ::    MFileInput_Type
    import                                                            ::    InputSection_Type
    class(MFileInput_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_MFileInput( This, MainSectionName, Prefix, Directory )
    import                                                            ::    InputSection_Type
    import                                                            ::    MFileInput_Type
    type(InputSection_Type)                                           ::    GetInput_MFileInput
    class(MFileInput_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput_MFileInput( This, Input, Strings )
    use Parameters_Library
    use String_Library
    import                                                            ::    InputDet_Type
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(inout)                             ::    This
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings
    type(InputDet_Type), intent(in)                                   ::    Input
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_MFileInput( LHS, RHS )
    import                                                            ::    MFileInput_Type
    class(MFileInput_Type), intent(out)                               ::    LHS
    class(MFileInput_Type), intent(in)                                ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(MFileInput_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
