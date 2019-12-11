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

module SampleLHS_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleScheme_Class                                            ,only:    SampleScheme_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type

implicit none

private

public                                                                ::    SampleLHS_Type

type, extends(SampleScheme_Type)                                      ::    SampleLHS_Type
  type(RandPseudo_Type)                                               ::    RNG
  logical                                                             ::    MedianPoints=.false.
  integer                                                             ::    EnrichmentScheme=0
  integer                                                             ::    EnrichmentMultiplier=1
  integer                                                             ::    EnrichmentIncrement=1
  integer, allocatable, dimension(:)                                  ::    EnrichmentSequence
  integer                                                             ::    EnrichmentStage=1
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    GetNbEnrichSamples
  procedure, private                                                  ::    Draw_0D
  procedure, private                                                  ::    Draw_1D
  procedure, private                                                  ::    Enrich_0D
  procedure, private                                                  ::    Enrich_1D
  generic, public                                                     ::    Shuffle                 =>    Shuffle_R1D,            &
                                                                                                          Shuffle_I1D
  procedure, private                                                  ::    Shuffle_R1D
  procedure, private                                                  ::    Shuffle_I1D
  procedure, nopass, private                                          ::    CheckRepresentation
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This )

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'lhs'
      call This%SetDefaults()
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This )

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%RNG%Reset()

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults(This)

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MedianPoints = .false.
    This%NbSamples = 1
    This%MaxNbSamples = huge(This%NbSamples)
    This%EnrichmentScheme = 0
    This%EnrichmentMultiplier = 1
    This%EnrichmentIncrement = 1
    This%EnrichmentStage = 1

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput ( This, Input, Prefix )

    use StringRoutines_Module

    class(SampleLHS_Type), intent(inout)                              ::    This
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

    ParameterName = 'enrichment_scheme'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      SectionName = 'enrichment_scheme'
      select case (VarC0D)
        case ('multiplier')
          This%EnrichmentScheme = 0
          ParameterName = 'multiplier'
          call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName,Mandatory=.true.)
          This%EnrichmentMultiplier = VarI0D
          if ( This%EnrichmentMultiplier < 1 ) call Error%Raise( Line='Enrichment multiplier must be above 0', ProcName=ProcName )
        case('increment')
          This%EnrichmentScheme = 1
          ParameterName = 'increment'
          call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName,Mandatory=.true.)
          This%EnrichmentIncrement = VarI0D
        case('sequence')
          This%EnrichmentScheme = 2
          ParameterName = 'sequence'
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName,Mandatory=.true.)
          allocate(This%EnrichmentSequence, source=ConvertToIntegers(String=VarC0D), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
          if ( any(This%EnrichmentSequence < 1) ) call Error%Raise( Line='Detected enrichment sequence value of 0 or below',      &
                                                                                                               ProcName=ProcName )
        case default
          call Error%Raise( Line='Uncrecognized enrichment specification', ProcName=ProcName )
      end select
    end if

    ParameterName = 'enrichment_stage'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if ( Found ) This%EnrichmentStage = VarI0D

    ParameterName = 'nb_samples'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%NbSamples = VarI0D
    This%MaxNbSamples = This%NbSamples

    ParameterName = 'max_nb_samples'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if ( Found ) This%MaxNbSamples = VarI0D

    ParameterName = 'median_points'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%MedianPoints = VarL0D

    SectionName = 'rng'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found) then
      call This%RNG%Construct(Input=InputSection, Prefix=PrefixLoc)
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1 ( This, RNG, NbSamples, MedianPoints,MaxNbSamples, EnrichmentScheme, EnrichmentMultiplier,            & 
                                                                                  EnrichmentIncrement, EnrichmentSequence )

    class(SampleLHS_Type), intent(inout)                              ::    This
    type(RandPseudo_Type), optional, intent(in)                       ::    RNG
    logical, optional, intent(in)                                     ::    MedianPoints
    integer, intent(in)                                               ::    NbSamples
    integer, optional, intent(in)                                     ::    MaxNbSamples
    integer, optional, intent(in)                                     ::    EnrichmentScheme
    integer, optional, intent(in)                                     ::    EnrichmentMultiplier
    integer, optional, intent(in)                                     ::    EnrichmentIncrement
    integer, optional, dimension(:), intent(in)                       ::    EnrichmentSequence

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(RNG) ) then
      This%RNG = RNG
    else
      call This%RNG%Construct()
    end if

    if ( present(MedianPoints) ) This%MedianPoints = MedianPoints

    This%NbSamples = NbSamples

    if ( present(MaxNbSamples) ) This%MaxNbSamples = MaxNbSamples

    if ( present(EnrichmentScheme) ) then
      This%EnrichmentScheme = EnrichmentScheme
      select case (This%EnrichmentScheme)
        case (0)
          if ( present(EnrichmentMultiplier) ) This%EnrichmentMultiplier = EnrichmentMultiplier
          if ( This%EnrichmentMultiplier < 1 ) call Error%Raise( Line='Enrichment multiplier must be above 0', ProcName=ProcName )
        case (1)
          if ( present(EnrichmentIncrement) ) This%EnrichmentIncrement = EnrichmentIncrement
        case (2)
          if ( present(EnrichmentSequence) ) then
            allocate(This%EnrichmentSequence, source=EnrichmentSequence, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
          end if
        case default
          call Error%Raise( Line='Uncrecognized enrichment specification', ProcName=ProcName )
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory )

    use CommandRoutines_Module
    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(SampleLHS_Type), intent(in)                                 ::    This
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

    call GetInput%AddParameter( Name='nb_samples', Value=ConvertToString(Value=This%NbSamples) )
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

    call GetInput%AddParameter( Name='enrichment_stage', Value=ConvertToString(Value=This%EnrichmentStage) )

    call GetInput%AddParameter( Name='median_points', Value=ConvertToString(Value=This%MedianPoints) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection( Section=This%RNG%GetInput( MainSectionName='rng', Prefix=PrefixLoc, Directory=DirectorySub ))

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetNbEnrichSamples( This, NbSamples )

    integer                                                           ::    GetNbEnrichSamples

    class(SampleLHS_Type), intent(inout)                              ::    This
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='GetNbEnrichSamples'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) call This%Initialize()

    select case ( This%EnrichmentScheme ) 
      case (0) 
        GetNbEnrichSamples = This%EnrichmentMultiplier*NbSamples
      case (1)
        GetNbEnrichSamples = This%EnrichmentIncrement
      case (2)
        if ( This%EnrichmentStage > size(This%EnrichmentSequence,1) ) then
          GetNbEnrichSamples = This%EnrichmentSequence(size(This%EnrichmentSequence,1))
        else
          GetNbEnrichSamples = This%EnrichmentSequence(This%EnrichmentStage)
        end if
      case default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Draw_0D( This )

    real(rkp), allocatable, dimension(:)                              ::    Draw_0D

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    real(rkp)                                                         ::    dx

    if ( .not. This%Initialized ) call This%Initialize()

    allocate(Draw_0D(This%NbSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_0D', ProcName=ProcName, stat=StatLoc )

    dx = One / real(This%NbSamples,rkp)

    i = 1
    do i = 1, This%NbSamples
      if ( This%MedianPoints ) then
        Draw_0D(i) = 0.5 *dx + real((i-1),rkp)*dx
      else
        if ( i == This%NbSamples ) then
          Draw_0D(i) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
        else
          Draw_0D(i) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
        end if
      end if
    end do

    call This%Shuffle( Array=Draw_0D )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Draw_1D( This, NbDim )

    real(rkp), allocatable, dimension(:,:)                            ::    Draw_1D

    class(SampleLHS_Type), intent(inout)                              ::    This
    integer, intent(in)                                               ::    NbDim

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    real(rkp)                                                         ::    dx

    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested sample at or below 0', ProcName=ProcName )

    allocate(Draw_1D(NbDim, This%NbSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_1D', ProcName=ProcName, stat=StatLoc )

    dx = One / real(This%NbSamples,rkp)

    ii = 1
    do ii = 1, NbDim
      i = 1
      do i = 1, This%NbSamples
        if ( This%MedianPoints ) then
          Draw_1D(ii,i) = 0.5 *dx + real((i-1),rkp)*dx
        else
          if ( i == This%NbSamples ) then
            Draw_1D(ii,i) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
          else
            Draw_1D(ii,i) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
          end if
        end if
      end do
      call This%Shuffle( Array=Draw_1D(ii,:) )
    end do

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_0D( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized )

    use LinkedList0D_Class                                        ,only:    LinkedList0D_Type

    class(SampleLHS_Type), intent(inout)                              ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(out)                 ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    logical, allocatable, dimension(:)                                ::    Representation
    integer                                                           ::    NbBins
    integer                                                           ::    NbEnrichmentSamplesLoc
    real(rkp)                                                         ::    dx

    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(NbEnrichmentSamples) ) then
      NbEnrichmentSamplesLoc = NbEnrichmentSamples
    else
      NbEnrichmentSamplesLoc = This%GetNbEnrichSamples( NbSamples=size(Samples,1) )
    end if 

    if ( NbEnrichmentSamplesLoc < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .true.
    else 
      if ( NbEnrichmentSamplesLoc + size(Samples,1)  > This%MaxNbSamples ) then
        Exceeded = .true.
      else
        Exceeded = .false.

        NbBins = NbEnrichmentSamplesLoc + size(Samples,1)
        dx = One / real(NbBins,rkp)

        Representation = This%CheckRepresentation( NbBins=NbBins, Array=Samples )
        NbEnrichmentSamplesLoc = count(Representation .eqv. .false.)

        if ( NbEnrichmentSamplesLoc + size(Samples,1)  > This%MaxNbSamples ) then
          Exceeded = .true.
        else
          allocate(EnrichmentSamples(NbEnrichmentSamplesLoc), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )

          i = 1
          ii = 0
          do i = 1, size(Representation,1)
            if ( Representation(i) ) cycle
            ii = ii + 1

            if ( This%MedianPoints ) then
              EnrichmentSamples(ii) = 0.5 *dx + real((i-1),rkp)*dx
            else
              if ( i == size(Representation,1) ) then
                EnrichmentSamples(ii) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
              else
                EnrichmentSamples(ii) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
              end if
            end if
          end do

          deallocate(Representation, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Representation', ProcName=ProcName, stat=StatLoc )

          call This%Shuffle( Array=EnrichmentSamples )
        end if
      end if
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_1D( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized )

    use LinkedList0D_Class                                        ,only:    LinkedList0D_Type

    class(SampleLHS_Type), intent(inout)                              ::    This
    real(rkp), dimension(:,:),intent(in)                              ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii, iii, iv
    logical, allocatable, dimension(:)                                ::    Representation
    integer                                                           ::    NbDim
    integer                                                           ::    NbBins
    integer                                                           ::    NbDegenerateBins
    integer                                                           ::    NbEnrichmentSamplesLoc
    type(LinkedList0D_Type)                                           ::    RepRecord
    integer, allocatable, dimension(:)                                ::    PermutationArray
    real(rkp)                                                         ::    dx

    NbDim = size(Samples,1)
    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested samples at or below 0', ProcName=ProcName )

    if ( present(NbEnrichmentSamples) ) then
      NbEnrichmentSamplesLoc = NbEnrichmentSamples
    else
      NbEnrichmentSamplesLoc = This%GetNbEnrichSamples( NbSamples=size(Samples,2) )
    end if 

    if ( NbEnrichmentSamplesLoc < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .true.
    else 
      if ( NbEnrichmentSamplesLoc + size(Samples,2)  > This%MaxNbSamples ) then
        Exceeded = .true.
      else

        NbBins = NbEnrichmentSamplesLoc + size(Samples,2)
        dx = One / real(NbBins,rkp)
      
        i = 1
        do i = 1, size(Samples,1)
          Representation = This%CheckRepresentation( NbBins=NbBins, Array=Samples(i,:) )
          ii = count(Representation .eqv. .false.)
          if ( ii > NbEnrichmentSamplesLoc ) NbEnrichmentSamplesLoc = ii
        end do

        if ( NbEnrichmentSamplesLoc + size(Samples,2)  > This%MaxNbSamples ) then
          Exceeded = .true.
        else
          Exceeded = .false.

          allocate(EnrichmentSamples(NbDim, NbEnrichmentSamplesLoc), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )

          do iii = 1, NbDim
            Representation = This%CheckRepresentation( NbBins=NbBins, Array=Samples(iii,:) )
            iv = 0
            ii = 0
            do
              iv = iv + 1
              i = 1
              do i = 1, size(Representation,1)
                if ( Representation(i) ) cycle
                if ( iv == 1 ) call RepRecord%Append(Value=i)
                ii = ii + 1
                if ( This%MedianPoints ) then
                  EnrichmentSamples(iii,ii) = 0.5 *dx + real((i-1),rkp)*dx
                else
                  if ( i == size(Representation,1) ) then
                    EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
                  else
                    EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
                  end if
                end if
              end do
              NbDegenerateBins = NbEnrichmentSamplesLoc - ii
              if ( NbDegenerateBins < RepRecord%GetLength() ) exit
            end do

            if ( NbDegenerateBins > 0 ) then
              call RepRecord%Get( Values=PermutationArray )
              call This%Shuffle( Array=PermutationArray )
              i = 1
              do i = 1, NbDegenerateBins
                ii = ii + 1
                if ( This%MedianPoints ) then
                  EnrichmentSamples(iii,ii) = 0.5 *dx + real((PermutationArray(i)-1),rkp)*dx
                else
                  if ( PermutationArray(i) == size(Representation,1) ) then
                    EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=1)*dx + real((PermutationArray(i)-1),rkp)*dx
                  else
                    EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=2)*dx + real((PermutationArray(i)-1),rkp)*dx
                  end if
                end if
              end do
            end if
            call This%Shuffle( Array=EnrichmentSamples(iii,:) )
          end do

          deallocate(Representation, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='Representation', ProcName=ProcName, stat=StatLoc )

        end if
      end if
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Shuffle_R1D( This, Array )

    class(SampleLHS_Type), intent(inout)                              ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Array

    character(*), parameter                                           ::    ProcName='Shuffle'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    Length
    integer                                                           ::    i, ii
    real(rkp)                                                         ::    Temp

    Length = size(Array,1)

    do i = 1, Length-1
      ii = i + floor((Length+1-i)*This%RNG%Draw( DrawType=2 ))
      if ( ii == i ) then
        continue
      else
        Temp = Array(ii)
        Array(ii) = Array(i)
        Array(i) = Temp
      end if
    end do

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Shuffle_I1D( This, Array )

    class(SampleLHS_Type), intent(inout)                              ::    This
    integer, dimension(:), intent(inout)                              ::    Array

    character(*), parameter                                           ::    ProcName='Shuffle'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    Length
    integer                                                           ::    i, ii
    integer                                                           ::    Temp

    Length = size(Array,1)

    do i = 1, Length-1
      ii = i + floor((Length+1-i)*This%RNG%Draw( DrawType=2 ))
      if ( ii == i ) then
        continue
      else
        Temp = Array(ii)
        Array(ii) = Array(i)
        Array(i) = Temp
      end if
    end do

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function CheckRepresentation( NbBins, Array )

    logical, allocatable, dimension(:)                                ::    CheckRepresentation
    integer, intent(in)                                               ::    NbBins
    real(rkp), dimension(:), intent(in)                               ::    Array

    character(*), parameter                                           ::    ProcName='CheckRepresentation'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    Length
    integer                                                           ::    i
    real(rkp)                                                         ::    BinMin
    real(rkp)                                                         ::    BinMax
    real(rkp)                                                         ::    dx

    Length = size(Array,1)

    allocate(CheckRepresentation(NbBins), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CheckRepresentation', ProcName=ProcName, stat=StatLoc )

    CheckRepresentation = .false.
    dx = One / real(NbBins,rkp)

    i = 1
    do i = 1, NbBins
      BinMin = (i-1)*dx
      BinMax = i*dx
      if ( i == NbBins ) then
        if ( any( Array >= BinMin .and. Array <= BinMax ) ) CheckRepresentation(i) = .true.
      else
        if ( any( Array >= BinMin .and. Array < BinMax ) ) CheckRepresentation(i) = .true.
      end if
    end do

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(SampleLHS_Type), intent(out)                                ::    LHS
    class(SampleScheme_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SampleLHS_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%EnrichmentScheme = RHS%EnrichmentScheme
          LHS%EnrichmentMultiplier = RHS%EnrichmentMultiplier
          LHS%EnrichmentIncrement = RHS%EnrichmentIncrement
          LHS%EnrichmentStage = RHS%EnrichmentStage
          if ( allocated(RHS%EnrichmentSequence) ) allocate(LHS%EnrichmentSequence, source=RHS%EnrichmentSequence, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )
          LHS%RNG=RHS%RNG
          LHS%MedianPoints = RHS%MedianPoints
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(SampleLHS_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
