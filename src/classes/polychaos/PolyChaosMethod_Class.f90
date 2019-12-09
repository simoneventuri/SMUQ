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

module PolyChaosMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use List2D_Class                                                  ,only:    List2D_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type

implicit none

private

public                                                                ::    PolyChaosMethod_Type

type, abstract                                                        ::    PolyChaosMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_PolyChaosMethod), deferred, public             ::    Initialize
  procedure(Reset_PolyChaosMethod), deferred, public                  ::    Reset
  procedure(SetDefaults_PolyChaosMethod), deferred, public            ::    SetDefaults
  procedure(ConstructInput_PolyChaosMethod), deferred, private        ::    ConstructInput
  procedure(GetInput_PolyChaosMethod), deferred, public               ::    GetInput
  procedure(BuildModel_PolyChaosMethod), deferred, public             ::    BuildModel
  procedure(Copy_PolyChaosMethod), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_PolyChaosMethod( This, Debug )
    import                                                            ::    PolyChaosMethod_Type
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_PolyChaosMethod( This, Debug )
    import                                                            ::    PolyChaosMethod_Type
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_PolyChaosMethod( This, Debug )
    import                                                            ::    PolyChaosMethod_Type
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_PolyChaosMethod( This, Input, SectionChain, Prefix, Debug )
    import                                                            ::    PolyChaosMethod_Type
    import                                                            ::    InputSection_Type
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_PolyChaosMethod( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    PolyChaosMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_PolyChaosMethod
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine BuildModel_PolyChaosMethod( This, Basis, SampleSpace, Responses, Model, IndexSetScheme, Coefficients, Indices,       &
                                                                    CVErrors, OutputDirectory, InputSamples, OutputSamples, Debug)
    use Parameters_Library
    import                                                            ::    PolyChaosMethod_Type
    import                                                            ::    OrthoMultivar_Type
    import                                                            ::    SampleSpace_Type
    import                                                            ::    IndexSetScheme_Type
    import                                                            ::    PolyChaosModel_Type
    import                                                            ::    LinkedList0D_Type
    import                                                            ::    LinkedList2D_Type
    import                                                            ::    LinkedList1D_Type
    import                                                            ::    List2D_Type
    import                                                            ::    Response_Type
    import                                                            ::    Model_Type
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
    type(OrthoMultiVar_Type), intent(inout)                           ::    Basis
    class(SampleSpace_Type), intent(inout)                            ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    class(IndexSetScheme_Type), intent(inout)                         ::    IndexSetScheme
    type(LinkedList0D_Type), allocatable, dimension(:), intent(out)   ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:), intent(out)   ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:), intent(out)   ::    Indices
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), optional, dimension(:,:), intent(in)                   ::    InputSamples
    type(List2D_Type), dimension(:), optional, intent(in)             ::    OutputSamples
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy_PolyChaosMethod( LHS, RHS )
    import                                                            ::    PolyChaosMethod_Type
    class(PolyChaosMethod_Type), intent(out)                          ::    LHS
    class(PolyChaosMethod_Type), intent(in)                           ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(PolyChaosMethod_Type), intent(inout)                        ::    This
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
