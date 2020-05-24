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

module PCEMethod_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use PCEModel_Class                                                ,only:    PCEModel_Type
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

public                                                                ::    PCEMethod_Type
public                                                                ::    ComputeSobolIndices

type, abstract                                                        ::    PCEMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    SectionChain
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_PCEMethod), deferred, public                   ::    Initialize
  procedure(Reset_PCEMethod), deferred, public                        ::    Reset
  procedure(SetDefaults_PCEMethod), deferred, public                  ::    SetDefaults
  procedure(ConstructInput_PCEMethod), deferred, private              ::    ConstructInput
  procedure(GetInput_PCEMethod), deferred, public                     ::    GetInput
  procedure(BuildModel_PCEMethod), deferred, public                   ::    BuildModel
  procedure(Copy_PCEMethod), deferred, public                         ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_PCEMethod(This)
    import                                                            ::    PCEMethod_Type
    class(PCEMethod_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_PCEMethod(This)
    import                                                            ::    PCEMethod_Type
    class(PCEMethod_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_PCEMethod(This)
    import                                                            ::    PCEMethod_Type
    class(PCEMethod_Type), intent(inout)                              ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_PCEMethod(This, Input, SectionChain, Prefix)
    import                                                            ::    PCEMethod_Type
    import                                                            ::    InputSection_Type
    class(PCEMethod_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_PCEMethod(This, Name, Prefix, Directory)
    import                                                            ::    PCEMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_PCEMethod
    class(PCEMethod_Type), intent(inout)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BuildModel_PCEMethod(This, Basis, SampleSpace, Responses, Model, IndexSetScheme, Coefficients, Indices,       &
                                                                    CVErrors, OutputDirectory, InputSamples, OutputSamples)
    use Parameters_Library
    import                                                            ::    PCEMethod_Type
    import                                                            ::    OrthoMultivar_Type
    import                                                            ::    SampleSpace_Type
    import                                                            ::    IndexSetScheme_Type
    import                                                            ::    PCEModel_Type
    import                                                            ::    LinkedList0D_Type
    import                                                            ::    LinkedList2D_Type
    import                                                            ::    LinkedList1D_Type
    import                                                            ::    List2D_Type
    import                                                            ::    Response_Type
    import                                                            ::    Model_Type
    class(PCEMethod_Type), intent(inout)                              ::    This
    type(OrthoMultiVar_Type), intent(inout)                           ::    Basis
    class(SampleSpace_Type), intent(inout)                            ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    type(IndexSetScheme_Type), intent(in)                             ::    IndexSetScheme
    type(LinkedList0D_Type), allocatable, dimension(:), intent(out)   ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:), intent(out)   ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:), intent(out)   ::    Indices
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), optional, dimension(:,:), intent(in)                   ::    InputSamples
    type(List2D_Type), dimension(:), optional, intent(in)             ::    OutputSamples
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_PCEMethod(LHS, RHS)
    import                                                            ::    PCEMethod_Type
    class(PCEMethod_Type), intent(out)                                ::    LHS
    class(PCEMethod_Type), intent(in)                                 ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(PCEMethod_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeSobolIndices(Coefficients, Indices, SobolIndices)

    real(rkp), dimension(:), intent(in)                               ::    Coefficients
    integer, dimension(:,:), intent(in)                               ::    Indices
    real(rkp), dimension(:), intent(out)                              ::    SobolIndices

    character(*), parameter                                           ::    ProcName='GetName'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    Variance
    integer                                                           ::    NbDim
    integer                                                           ::    Cardinality
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    i
    integer                                                           ::    ii

    NbDim = size(Indices,1)
    Cardinality = size(Indices,2)

    if (size(Coefficients,1) /= Cardinality) call Error%Raise('Incorrect size', ProcName=ProcName)
    if (size(SobolIndices,1) /= NbDim) call Error%Raise('Incorrect size', ProcName=ProcName)

    SobolIndices = Zero

    if (Cardinality > 1) then
      Variance = dot_product(Coefficients(2:),Coefficients(2:))
      i = 1
      do i = 1, NbDim
        VarR0D = Zero
        ii = 1
        do ii = 1, Cardinality
          if (Indices(i,ii) /= 0) VarR0D = VarR0D + Coefficients(ii)**2
        end do
        SobolIndices(i) = VarR0D / Variance
      end do
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
