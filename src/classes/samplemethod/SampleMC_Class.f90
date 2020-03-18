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

module SampleMC_Class

use Parameters_Library
use Input_Library
use CommandRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type

implicit none

private

public                                                                ::    SampleMC_Type

type, extends(SampleMethod_Type)                                      ::    SampleMC_Type
  type(RandPseudo_Type)                                               ::    RNG
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Draw_0D
  procedure, private                                                  ::    Draw_1D
  procedure, private                                                  ::    Enrich_0D
  procedure, private                                                  ::    Enrich_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(SampleMC_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'monte_carlo'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SampleMC_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%RNG%Reset()

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SampleMC_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput ( This, Input, Prefix )

    class(SampleMC_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'rng'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found) then
      call This%RNG%Construct(Input=InputSection, Prefix=PrefixLoc)
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 ( This, RNG )

    class(SampleMC_Type), intent(inout)                               ::    This
    type(RandPseudo_Type), optional, intent(in)                       ::    RNG

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(RNG) ) then
      This%RNG = RNG
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput
    class(SampleMC_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection( Section=This%RNG%GetInput( MainSectionName='rng', Prefix=PrefixLoc, Directory=DirectorySub ))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Draw_0D( This, NbSamples )

    real(rkp), allocatable, dimension(:)                              ::    Draw_0D

    class(SampleMC_Type), intent(inout)                               ::    This
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( .not. This%Initialized ) call This%Initialize()

    allocate(Draw_0D(NbSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_0D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbSamples
      Draw_0D(i) = This%RNG%Draw()
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Draw_1D( This, NbDim, NbSamples )

    real(rkp), allocatable, dimension(:,:)                            ::    Draw_1D

    class(SampleMC_Type), intent(inout)                               ::    This
    integer, intent(in)                                               ::    NbDim
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0

    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested sample at or below 0', ProcName=ProcName )

    allocate(Draw_1D(NbDim, NbSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_1D', ProcName=ProcName, stat=StatLoc )

    Draw_1D = This%RNG%DrawMat( Size1=NbDim, Size2=NbSamples, DrawType=1 )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich_0D( This, Samples, NbEnrichmentSamples, EnrichmentSamples, ReqNormalized )

    class(SampleMC_Type), intent(inout)                               ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(out)                 ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( NbEnrichmentSamples < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .false.
    else 
      if ( NbEnrichmentSamples + size(Samples,1)  > This%MaxNbSamples ) then
        Exceeded = .true.
      else
        Exceeded = .false.
        allocate(EnrichmentSamples(NbEnrichmentSamples), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )

        i = 1
        do i = 1, NbEnrichmentSamples
          EnrichmentSamples(i) = This%RNG%Draw()
        end do
        This%EnrichmentStage = This%EnrichmentStage + 1
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich_1D( This, Samples, NbEnrichmentSamples, EnrichmentSamples, ReqNormalized )

    class(SampleMC_Type), intent(inout)                               ::    This
    real(rkp), dimension(:,:),intent(in)                              ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    integer                                                           ::    i

    NbDim = size(Samples,1)

    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested samples at or below 0', ProcName=ProcName )

    if ( NbEnrichmentSamples < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .false.
    else 
      if ( NbEnrichmentSamples + size(Samples,2)  > This%MaxNbSamples ) then
        Exceeded = .true.
      else
        Exceeded = .false.
        allocate(EnrichmentSamples(NbDim, NbEnrichmentSamples), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )
        EnrichmentSamples = This%RNG%DrawMat(Size1=NbDim, Size2=NbEnrichmentSamples, DrawType=1)
        This%EnrichmentStage = This%EnrichmentStage + 1
      end if
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SampleMC_Type), intent(out)                                 ::    LHS
    class(SampleMethod_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SampleMC_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%RNG=RHS%RNG
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SampleMC_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
