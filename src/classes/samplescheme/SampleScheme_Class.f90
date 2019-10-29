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

module SampleScheme_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    SampleScheme_Type

type, abstract                                                        ::    SampleScheme_Type
  character(:), allocatable                                           ::    Name
  integer                                                             ::    NbSamples=0
  integer                                                             ::    MaxNbSamples=huge(1)
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Draw                    =>    Draw_0D,                &
                                                                                                          Draw_1D
  generic, public                                                     ::    Enrich                  =>    Enrich_0D,              &
                                                                                                          Enrich_1D
  procedure(Initialize_SampleScheme), deferred, public                ::    Initialize
  procedure(Reset_SampleScheme), deferred, public                     ::    Reset
  procedure(SetDefaults_SampleScheme), deferred, public               ::    SetDefaults
  procedure(ConstructInput_SampleScheme), deferred, private           ::    ConstructInput
  procedure(GetInput_SampleScheme), deferred, public                  ::    GetInput
  procedure(DrawSamples_0D_SampleScheme), deferred, private           ::    Draw_0D
  procedure(DrawSamples_1D_SampleScheme), deferred, private           ::    Draw_1D
  procedure(Enrich_0D_SampleScheme), deferred, private                ::    Enrich_0D
  procedure(Enrich_1D_SampleScheme), deferred, private                ::    Enrich_1D
  procedure(Copy_SampleScheme), deferred, public                      ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize_SampleScheme( This, Debug )
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset_SampleScheme( This, Debug )
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults_SampleScheme( This, Debug )
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput_SampleScheme ( This, Input, Prefix, Debug )
    import                                                            ::    SampleScheme_Type
    import                                                            ::    InputSection_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput_SampleScheme( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    SampleScheme_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_SampleScheme
    class(SampleScheme_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function DrawSamples_0D_SampleScheme( This, Debug )
    use Parameters_Library
    import                                                            ::    SampleScheme_Type
    real(rkp), allocatable, dimension(:)                              ::    DrawSamples_0D_SampleScheme   
    class(SampleScheme_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function DrawSamples_1D_SampleScheme( This, NbDim, Debug )
    use Parameters_Library
    import                                                            ::    SampleScheme_Type
    real(rkp), allocatable, dimension(:,:)                            ::    DrawSamples_1D_SampleScheme  
    class(SampleScheme_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_0D_SampleScheme( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized, Debug )
    use Parameters_Library
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(out)                 ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized
    logical, optional ,intent(in)                                     ::    Debug                                             
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_1D_SampleScheme( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized, Debug )
    use Parameters_Library
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(inout)                           ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized
    logical, optional ,intent(in)                                     ::    Debug                                             
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy_SampleScheme( LHS, RHS )
    import                                                            ::    SampleScheme_Type
    class(SampleScheme_Type), intent(out)                             ::    LHS
    class(SampleScheme_Type), intent(in)                              ::    RHS
  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end interface

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName

    class(SampleScheme_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
