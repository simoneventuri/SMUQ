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

module SampleMethod_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    SampleMethod_Type

type, abstract                                                        ::    SampleMethod_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Draw                    =>    Draw0D,                &
                                                                                                          Draw1D
  generic, public                                                     ::    Enrich                  =>    Enrich0D,              &
                                                                                                          Enrich1D
  procedure(Initialize_SampleMethod), deferred, public                ::    Initialize
  procedure(Reset_SampleMethod), deferred, public                     ::    Reset
  procedure(SetDefaults_SampleMethod), deferred, public               ::    SetDefaults
  procedure(ConstructInput_SampleMethod), deferred, private           ::    ConstructInput
  procedure(GetInput_SampleMethod), deferred, public                  ::    GetInput
  procedure(Draw0D_SampleMethod), deferred, private                   ::    Draw0D
  procedure(Draw1D_SampleMethod), deferred, private                   ::    Draw1D
  procedure(Enrich0D_SampleMethod), deferred, private                 ::    Enrich0D
  procedure(Enrich1D_SampleMethod), deferred, private                 ::    Enrich1D
  procedure(Copy_SampleMethod), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SampleMethod(This)
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SampleMethod(This)
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SampleMethod(This)
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SampleMethod (This, Input, Prefix)
    import                                                            ::    SampleMethod_Type
    import                                                            ::    InputSection_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SampleMethod(This, Name, Prefix, Directory)
    import                                                            ::    SampleMethod_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_SampleMethod
    class(SampleMethod_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Draw0D_SampleMethod(This, Samples, NbSamples)
    use Parameters_Library
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NbSamples
    real(rkp), allocatable, dimension(:), intent(inout)               ::    Samples                                     
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Draw1D_SampleMethod(This, Samples, NbSamples, NbDim)
    use Parameters_Library
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NbSamples
    integer, intent(in)                                               ::    NbDim
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Samples                                     
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich0D_SampleMethod(This, Samples, NbEnrichmentSamples, EnrichmentSamples)
    use Parameters_Library
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(inout)               ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich1D_SampleMethod(This, Samples, NbEnrichmentSamples, EnrichmentSamples)
    use Parameters_Library
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(inout)                           ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(inout)             ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_SampleMethod(LHS, RHS)
    import                                                            ::    SampleMethod_Type
    class(SampleMethod_Type), intent(out)                             ::    LHS
    class(SampleMethod_Type), intent(in)                              ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName

    class(SampleMethod_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
