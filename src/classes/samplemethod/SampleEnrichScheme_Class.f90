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

module SampleEnrichScheme_Class

use Parameters_Library
use Input_Library
use CommandRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    SampleEnrichScheme_Type

type, extends(SampleMethod_Type)                                      ::    SampleEnrichScheme_Type
  character(:), allocatable                                           ::    Name
  integer                                                             ::    MaxNbSamples=huge(1)
  integer                                                             ::    EnrichmentScheme=0
  real(rkp)                                                           ::    EnrichmentMultiplier=1
  integer                                                             ::    EnrichmentIncrement=1
  integer, allocatable, dimension(:)                                  ::    EnrichmentSequence
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    GetNbEnrichSamples
  procedure, private                                                  ::    GetMaxNbSamples
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(SampleEnrichScheme_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'SampleEnrichScheme'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SampleEnrichScheme_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )


    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SampleEnrichScheme_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MaxNbSamples = huge(This%NbSamples)
    This%EnrichmentScheme = 0
    This%EnrichmentMultiplier = Two
    This%EnrichmentIncrement = 1

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput ( This, Input, Prefix )

    use StringRoutines_Module

    class(SampleEnrichScheme_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'scheme'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      SectionName = 'scheme'
      select case (VarC0D)
        case ('multiplier')
          This%EnrichmentScheme = 0
          ParameterName = 'multiplier'
          call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
          This%EnrichmentMultiplier = VarR0D
          if ( This%EnrichmentMultiplier < One ) call Error%Raise( Line='Enrichment multiplier must be above 1',                  &
                                                                                                               ProcName=ProcName )
        case('increment')
          This%EnrichmentScheme = 1
          ParameterName = 'increment'
          call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
          This%EnrichmentIncrement = VarI0D
        case('sequence')
          This%EnrichmentScheme = 2
          ParameterName = 'sequence'
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
          allocate(This%EnrichmentSequence, source=ConvertToIntegers(String=VarC0D), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
          if ( any(This%EnrichmentSequence < 1) ) call Error%Raise( Line='Detected enrichment sequence value of 0 or below',      &
                                                                                                               ProcName=ProcName )
        case default
          call Error%Raise( Line='Uncrecognized enrichment specification', ProcName=ProcName )
      end select
    end if

    ParameterName = 'max_nb_samples'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if ( Found ) This%MaxNbSamples = VarI0D

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 ( This, MaxNbSamples, EnrichmentScheme, EnrichmentMultiplier, EnrichmentIncrement, EnrichmentSequence)

    class(SampleEnrichScheme_Type), intent(inout)                     ::    This
    integer, optional, intent(in)                                     ::    MaxNbSamples
    character(*), optional, intent(in)                                ::    EnrichmentScheme
    real(rkp), optional, intent(in)                                   ::    EnrichmentMultiplier
    integer, optional, intent(in)                                     ::    EnrichmentIncrement
    integer, optional, dimension(:), intent(in)                       ::    EnrichmentSequence

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%MaxNbSamples = huge(1)
    if ( present(MaxNbSamples) ) This%MaxNbSamples = MaxNbSamples

    if ( present(EnrichmentScheme) ) then
      select case (EnrichmentScheme)
        case ('multiplier')
          This%EnrichmentScheme = 0
          if ( present(EnrichmentMultiplier) ) This%EnrichmentMultiplier = EnrichmentMultiplier
          if ( This%EnrichmentMultiplier < One ) call Error%Raise( Line='Enrichment multiplier must be above 1',                  &
                                                                                                               ProcName=ProcName )
        case ('increment')
          This%EnrichmentScheme = 1
          if ( present(EnrichmentIncrement) ) This%EnrichmentIncrement = EnrichmentIncrement
        case ('sequence')
          This%EnrichmentScheme = 2
          if ( present(EnrichmentSequence) ) then
            allocate(This%EnrichmentSequence, source=EnrichmentSequence, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
          else
            call Error%Raise( 'Enrichment sequence must be specified', ProcName=ProcName )
          end if
        case default
          call Error%Raise( Line='Uncrecognized enrichment specification', ProcName=ProcName )
      end select
    else
      This%EnrichmentScheme = 0
      This%EnrichmentMultiplier = Two
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(SampleEnrichScheme_Type), intent(in)                        ::    This
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

    call GetInput%AddParameter( Name='max_nb_samples', Value=ConvertToString(Value=This%MaxNbSamples) )

    SectionName = 'enrichment_scheme'
    call GetInput%AddSection( SectionName=SectionName )
    select case ( This%EnrichmentScheme )
      case ( 0 )
        call GetInput%AddParameter( Name='enrichment_scheme', Value='multiplier' )
        call GetInput%AddParameter( Name='multiplier', Value=ConvertToString(Value=This%EnrichmentMultiplier),                    &
                                                                                                          SectionName=SectionName)
      case ( 1 )
        call GetInput%AddParameter( Name='enrichment_scheme', Value='increment' )
        call GetInput%AddParameter( Name='increment', Value=ConvertToString(Value=This%EnrichmentIncrement),                      &
                                                                                                         SectionName=SectionName )
      case ( 2 )
        call GetInput%AddParameter( Name='enrichment_scheme', Value='sequence' )
        call GetInput%AddParameter( Name='sequence', Value=ConvertToString(Values=This%EnrichmentSequence),                       &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbEnrichSamples( This, NbSamples, Stage )

    integer                                                           ::    GetNbEnrichSamples

    class(SampleEnrichScheme_Type), intent(in)                        ::    This
    integer, intent(in)                                               ::    NbSamples
    integer, intent(in)                                               ::    Stage

    character(*), parameter                                           ::    ProcName='GetNbEnrichSamples'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    select case ( This%EnrichmentScheme ) 
      case (0) 
        GetNbEnrichSamples = nint((This%EnrichmentMultiplier-One)(real(NbSamples,rkp))
      case (1)
        GetNbEnrichSamples = This%EnrichmentIncrement
      case (2)
        if ( Stage > size(This%EnrichmentSequence,1) ) then
          GetNbEnrichSamples = This%EnrichmentSequence(size(This%EnrichmentSequence,1))
        else
          GetNbEnrichSamples = This%EnrichmentSequence(Stage)
        end if
      case default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMaxNbSamples( This, NbSamples )

    integer                                                           ::    GetMaxNbSamples

    class(SampleEnrichScheme_Type), intent(in)                        ::    This
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='GetNbEnrichSamples'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetMaxNbSamples = This%MaxNbSamples

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SampleEnrichScheme_Type), intent(out)                       ::    LHS
    class(SampleEnrichScheme_Type), intent(in)                        ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%MaxNbSamples = RHS%MaxNbSamples
      LHS%EnrichmentScheme = RHS%EnrichmentScheme
      LHS%EnrichmentMultiplier = RHS%EnrichmentMultiplier
      LHS%EnrichmentIncrement = RHS%EnrichmentIncrement
      if ( allocated(RHS%EnrichmentSequence) ) allocate(LHS%EnrichmentSequence, source=RHS%EnrichmentSequence, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SampleEnrichScheme_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
