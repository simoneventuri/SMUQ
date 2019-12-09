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

module CalibrationMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type

implicit none

private

public                                                                ::    CalibrationMethod_Type

type, abstract                                                        ::    CalibrationMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_CalibrationMethod), deferred, public           ::    Initialize
  procedure(Reset_CalibrationMethod), deferred, public                ::    Reset
  procedure(SetDefaults_CalibrationMethod), deferred, public          ::    SetDefaults
  procedure(ConstructInput_CalibrationMethod), deferred, private      ::    ConstructInput
  procedure(GetInput_CalibrationMethod), deferred, public             ::    GetInput
  procedure(Run_CalibrationMethod), deferred, public                  ::    Run
  procedure(Copy_CalibrationMethod), deferred, public                 ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_CalibrationMethod( This, Debug )
    import                                                            ::    CalibrationMethod_Type
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_CalibrationMethod( This, Debug )
    import                                                            ::    CalibrationMethod_Type
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_CalibrationMethod( This, Debug )
    import                                                            ::    CalibrationMethod_Type
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_CalibrationMethod( This, Input, SectionChain, Prefix, Debug )
    import                                                            ::    CalibrationMethod_Type
    import                                                            ::    InputSection_Type
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_CalibrationMethod( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    CalibrationMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_CalibrationMethod
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_CalibrationMethod( This, SampleSpace, Responses, Model, OutputDirectory, Debug )
    import                                                            ::    SampleSpace_Type
    import                                                            ::    Response_Type
    import                                                            ::    ModelType
    import                                                            ::    CalibrationMethod_Type
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_CalibrationMethod( LHS, RHS )
    import                                                            ::    CalibrationMethod_Type
    class(CalibrationMethod_Type), intent(out)                        ::    LHS
    class(CalibrationMethod_Type), intent(in)                         ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(CalibrationMethod_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    call Logger%Entering( ProcName )
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
