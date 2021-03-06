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
use Model_Class                                                   ,only:    Model_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Response_Class                                                ,only:    Response_Type

implicit none

private

public                                                                ::    AnalysisMethod_Type

type, abstract                                                        ::    AnalysisMethod_Type
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Reset_AnalysisMethod), deferred, public                   ::    Reset
  procedure(ConstructInput_AnalysisMethod), deferred, private         ::    ConstructInput
  procedure(GetInput_AnalysisMethod), deferred, public                ::    GetInput
  procedure(Run_AnalysisMethod), deferred, public                     ::    Run
  procedure(Copy_AnalysisMethod), deferred, public                    ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_AnalysisMethod(This)
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_AnalysisMethod(This, Input, SectionChain, Prefix)
    import                                                            ::    AnalysisMethod_Type
    import                                                            ::    InputSection_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_AnalysisMethod(This, Name, Prefix, Directory)
    import                                                            ::    AnalysisMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_AnalysisMethod
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_AnalysisMethod(This, SampleSpace, Responses, Model, OutputDirectory)
    import                                                            ::    SampleSpace_Type
    import                                                            ::    Response_Type
    import                                                            ::    Model_Type
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(inout)                         ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_AnalysisMethod(LHS, RHS)
    import                                                            ::    AnalysisMethod_Type
    class(AnalysisMethod_Type), intent(out)                           ::    LHS
    class(AnalysisMethod_Type), intent(in)                            ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

end module