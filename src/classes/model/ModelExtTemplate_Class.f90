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

module ModelExtTemplate_class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type

implicit none

private

public                                                                ::    ModelExtTemplate_Type

type, abstract, extends(Model_Type)                                   ::    ModelExtTemplate_Type
contains

  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase
  procedure(ConstructCase_ModelExtTemplate), deferred, private        ::    ConstructCase
  procedure(ConstructInput_ModelExtTemplate), deferred, private       ::    ConstructInput
  procedure(GetInput_ModelExtTemplate), deferred, public              ::    GetInput

end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_ModelExtTemplate( This, Input, Prefix, Debug )
    import                                                            ::    ModelExtTemplate_Type
    import                                                            ::    InputSection_Type
    class(ModelExtTemplate_Type), intent(inout)                       ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase_ModelExtTemplate( This, CaseDir, Prefix, Debug )
    import                                                            ::    ModelExtTemplate_Type
    class(ModelExtTemplate_Type), intent(inout)                       ::    This
    character(*), intent(in)                                          ::    CaseDir
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_ModelExtTemplate( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    ModelExtTemplate_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_ModelExtTemplate
    class(ModelExtTemplate_Type), intent(in)                          ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

end module
