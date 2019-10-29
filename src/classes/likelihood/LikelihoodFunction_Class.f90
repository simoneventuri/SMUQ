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

module LikelihoodFunction_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use Response_Class                                                ,only:    Response_Type
use Output_Class                                                  ,only:    Output_Type

implicit none

private

public                                                                ::    LikelihoodFunction_Type

type, abstract                                                        ::    LikelihoodFunction_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Evaluate                =>    EvaluateDet,            &
                                                                                                          EvaluateStoch
  procedure(Initialize_LikelihoodFunction), deferred, public          ::    Initialize
  procedure(Reset_LikelihoodFunction), deferred, public               ::    Reset
  procedure(SetDefaults_LikelihoodFunction), deferred, public         ::    SetDefaults
  procedure(ConstructInput_LikelihoodFunction), deferred, private     ::    ConstructInput
  procedure(GetInput_LikelihoodFunction), deferred, public            ::    GetInput
  procedure(EvaluateDet_LikelihoodFunction), deferred, public         ::    EvaluateDet
  procedure(EvaluateStoch_LikelihoodFunction), deferred, public       ::    EvaluateStoch
  procedure(Copy_LikelihoodFunction), deferred, public                ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_LikelihoodFunction( This, Debug )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LikelihoodFunction( This, Debug )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_LikelihoodFunction( This, Debug )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_LikelihoodFunction( This, Input, Prefix, Debug )
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    InputSection_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_LikelihoodFunction( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EvaluateDet_LikelihoodFunction( This, Response, Input, Output, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    Response_Type
    import                                                            ::    InputDet_Type
    import                                                            ::    Output_Type
    real(rkp)                                                         ::    EvaluateDet_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EvaluateStoch_LikelihoodFunction( This, Response, Input, Output, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    Response_Type
    import                                                            ::    InputStoch_Type
    import                                                            ::    Output_Type
    real(rkp)                                                         ::    EvaluateStoch_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    type(InputStoch_Type), intent(in)                                 ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_LikelihoodFunction( LHS, RHS )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(out)                       ::    LHS
    class(LikelihoodFunction_Type), intent(in)                        ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
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
