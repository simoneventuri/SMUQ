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
  character(:), allocatable                                           ::    Label
contains
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetLabel
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Evaluate                =>    Evaluate_0D,            &
                                                                                                          Evaluate_1D
  procedure(Initialize_LikelihoodFunction), deferred, public          ::    Initialize
  procedure(Reset_LikelihoodFunction), deferred, public               ::    Reset
  procedure(SetDefaults_LikelihoodFunction), deferred, public         ::    SetDefaults
  procedure(ConstructInput_LikelihoodFunction), deferred, private     ::    ConstructInput
  procedure(GetInput_LikelihoodFunction), deferred, public            ::    GetInput
  procedure(Evaluate_0D_LikelihoodFunction), deferred, private        ::    Evaluate_0D
  procedure(Evaluate_1D_LikelihoodFunction), deferred, private        ::    Evaluate_1D
  procedure(Copy_LikelihoodFunction), deferred, public                ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_LikelihoodFunction( This )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LikelihoodFunction( This )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_LikelihoodFunction( This )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_LikelihoodFunction( This, Input, Prefix )
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    InputSection_Type
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_LikelihoodFunction( This, MainSectionName, Prefix, Directory )
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_1D_LikelihoodFunction( This, Responses, Input, Output, LogValue )
    use                                                               ::    Parameters_Library
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    Response_Type
    import                                                            ::    InputDet_Type
    import                                                            ::    Output_Type
    real(rkp)                                                         ::    Evaluate_1D_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional, intent(in)                                     ::    LogValue
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_0D_LikelihoodFunction( This, Response, Input, Output, LogValue )
    use                                                               ::    Parameters_Library
    import                                                            ::    LikelihoodFunction_Type
    import                                                            ::    Response_Type
    import                                                            ::    InputDet_Type
    import                                                            ::    Output_Type
    real(rkp)                                                         ::    Evaluate_0D_LikelihoodFunction
    class(LikelihoodFunction_Type), intent(inout)                     ::    This
    type(Response_Type), intent(in)                                   ::    Response
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), intent(in)                                     ::    Output
    logical, optional, intent(in)                                     ::    LogValue
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_LikelihoodFunction( LHS, RHS )
    import                                                            ::    LikelihoodFunction_Type
    class(LikelihoodFunction_Type), intent(out)                       ::    LHS
    class(LikelihoodFunction_Type), intent(in)                        ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(LikelihoodFunction_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel( This )

    character(:), allocatable                                         ::    GetLabel
    class(LikelihoodFunction_Type), intent(inout)                     ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetLabel'

    GetLabel = This%Label

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
