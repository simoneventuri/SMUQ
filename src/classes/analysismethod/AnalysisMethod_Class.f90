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

module AnalysisMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    AnalysisMethod_Type

type, abstract                                                        ::    AnalysisMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_AnalysisMethod), deferred, public              ::    Initialize
  procedure(Reset_AnalysisMethod), deferred, public                   ::    Reset
  procedure(SetDefaults_AnalysisMethod), deferred, public             ::    SetDefaults
  procedure(ConstructInput_AnalysisMethod), deferred, private         ::    ConstructInput
  procedure(GetInput_AnalysisMethod), deferred, public                ::    GetInput
  procedure(Run_AnalysisMethod), deferred, public                     ::    Run
  procedure(Copy_AnalysisMethod), deferred, public                    ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_AnalysisMethod( This, Debug )
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_AnalysisMethod( This, Debug )
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_AnalysisMethod( This, Debug )
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_AnalysisMethod( This, Input, SectionChain, Prefix, Debug )
    import                                                            ::    AnalysisMethod_Type
    import                                                            ::    InputSection_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_AnalysisMethod( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    AnalysisMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_AnalysisMethod
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run_AnalysisMethod( This, SpaceInput, Response, Model, OutputDirectory, Debug )
    use Model_Class                                               ,only:    Model_Type
    use SpaceParam_Class                                          ,only:    SpaceParam_Type
    use Response_Class                                            ,only:    Response_Type
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy_AnalysisMethod( LHS, RHS )
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(out)                           ::    LHS
    class(AnalysisMethod_Type), intent(in)                            ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(AnalysisMethod_Type), intent(inout)                         ::    This
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
