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

module BayesInvMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Model_Class                                                   ,only:    Model_Type
use Response_Class                                                ,only:    Response_Type
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type

implicit none

private

public                                                                ::    BayesInvMethod_Type

type, abstract                                                        ::    BayesInvMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_BayesInvMethod), deferred, public              ::    Initialize
  procedure(Reset_BayesInvMethod), deferred, public                   ::    Reset
  procedure(SetDefaults_BayesInvMethod), deferred, public             ::    SetDefaults
  procedure(ConstructInput_BayesInvMethod), deferred, private         ::    ConstructInput
  procedure(GetInput_BayesInvMethod), deferred, public                ::    GetInput
  procedure(Calibrate_BayesInvMethod), deferred, public               ::    Calibrate
  procedure(Copy_BayesInvMethod), deferred, public                    ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_BayesInvMethod( This )
    import                                                            ::    BayesInvMethod_Type
    class(BayesInvMethod_Type), intent(inout)                         ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_BayesInvMethod( This )
    import                                                            ::    BayesInvMethod_Type
    class(BayesInvMethod_Type), intent(inout)                         ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_BayesInvMethod( This )
    import                                                            ::    BayesInvMethod_Type
    class(BayesInvMethod_Type), intent(inout)                         ::    This
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_BayesInvMethod( This, Input, SectionChain, Prefix )
    import                                                            ::    BayesInvMethod_Type
    import                                                            ::    InputSection_Type
    class(BayesInvMethod_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_BayesInvMethod( This, MainSectionName, Prefix, Directory )
    import                                                            ::    BayesInvMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_BayesInvMethod
    class(BayesInvMethod_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Calibrate_BayesInvMethod( This, Model, SampleSpace, Responses, LikelihoodFunction, OutputDirectory)
    import                                                            ::    BayesInvMethod_Type
    import                                                            ::    Response_Type
    import                                                            ::    SampleSpace_Type
    import                                                            ::    Model_Type
    import                                                            ::    LikelihoodFunction_Type
    class(BayesInvMethod_Type), intent(inout)                         ::    This
    class(Model_Type), intent(inout)                                  ::    Model
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(LikelihoodFunction_Type), intent(inout)                     ::    LikelihoodFunction
    character(*), optional, intent(in)                                ::    OutputDirectory
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy_BayesInvMethod( LHS, RHS )
    import                                                            ::    BayesInvMethod_Type
    class(BayesInvMethod_Type), intent(out)                           ::    LHS
    class(BayesInvMethod_Type), intent(in)                            ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(BayesInvMethod_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
