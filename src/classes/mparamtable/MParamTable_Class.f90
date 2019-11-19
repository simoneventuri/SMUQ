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

module MParamTable_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    MParamTable_Type

type, abstract                                                        ::    MParamTable_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_MParamTable), deferred, public                 ::    Initialize
  procedure(Reset_MParamTable), deferred, public                      ::    Reset
  procedure(SetDefaults_MParamTable), deferred, public                ::    SetDefaults
  procedure(ConstructInput_MParamTable), deferred, private            ::    ConstructInput
  procedure(GetInput_MParamTable), deferred, public                   ::    GetInput
  procedure(GetValue_MParamTable), deferred, public                   ::    GetValue
  procedure(GetCharValue_MParamTable), deferred, public               ::    GetCharValue
  procedure(Copy_MParamTable), deferred, public                       ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_MParamTable( This, Debug )
    import                                                            ::    MParamTable_Type
    class(MParamTable_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_MParamTable( This, Debug )
    import                                                            ::    MParamTable_Type
    class(MParamTable_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_MParamTable( This, Debug )
    import                                                            ::    MParamTable_Type
    class(MParamTable_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_MParamTable( This, Input, Prefix, Debug )
    import                                                            ::    MParamTable_Type
    import                                                            ::    InputSection_Type
    class(MParamTable_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_MParamTable( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    MParamTable_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_MParamTable
    class(MParamTable_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue_MParamTable( This, Input, Abscissa, Debug )
    use Parameters_Library
    import                                                            ::    InputDet_Type
    import                                                            ::    MParamTable_Type  
    real(rkp), allocatable, dimension(:)                              ::    GetValue_MParamTable
    class(MParamTable_Type), intent(in)                               ::    This
    type(InputDet_Type, intent(in)                                    ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue_MParamTable( This, Input, Abscissa, Format, Debug )
    use Parameters_Library
    use String_Library
    import                                                            ::    InputDet_Type
    import                                                            ::    MParamTable_Type
    type(String_Type), allocatable, dimension(:)                      ::    GetCharValue_MParamTable
    class(MParamTable_Type), intent(in)                               ::    This
    type(InputDet_Type, intent(in)                                    ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_MParamTable( LHS, RHS )
    import                                                            ::    MParamTable_Type
    class(MParamTable_Type), intent(out)                              ::    LHS
    class(MParamTable_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(MParamTable_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
