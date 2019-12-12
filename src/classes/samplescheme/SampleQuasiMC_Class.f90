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

module SampleQuasiMC_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleScheme_Class                                            ,only:    SampleScheme_Type
use LowDiscSequence_Class                                         ,only:    LowDiscSequence_Type
use LowDiscSequence_Factory_Class                                 ,only:    LowDiscSequence_Factory

implicit none

private

public                                                                ::    SampleQuasiMC_Type

type, extends(SampleScheme_Type)                                      ::    SampleQuasiMC_Type
  class(LowDiscSequence_Type), allocatable                            ::    LowDiscSequence
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
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    GetNbEnrichSamples
  procedure, private                                                  ::    Draw_0D
  procedure, private                                                  ::    Draw_1D
  procedure, private                                                  ::    Enrich_0D
  procedure, private                                                  ::    Enrich_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'quasi'
      call This%SetDefaults()
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%LowDiscSequence) ) deallocate(This%LowDiscSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%NbSamples = 1
    This%MaxNbSamples = huge(This%NbSamples)
    This%EnrichmentScheme = 0
    This%EnrichmentMultiplier = 1
    This%EnrichmentIncrement = 1
    This%EnrichmentStage = 1

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix )

    use StringRoutines_Module

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ProcessInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
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
        case ('mutiplier')
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

    SectionName = 'sequence'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call LowDiscSequence_Factory%Construct( Object=This%LowDiscSequence, Input=InputSection )
    nullify(InputSection)

    This%Constructed=.true.

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1 ( This, LowDiscSequence, NbSamples, MaxNbSamples, EnrichmentScheme, EnrichmentMultiplier,             &
                                                                                  EnrichmentIncrement, EnrichmentSequence )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
    class(LowDiscSequence_Type), intent(in)                           ::    LowDiscSequence
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

    allocate(This%LowDiscSequence, source=LowDiscSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc )

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

    use StringRoutines_Module
    use String_Library

    type(InputSection_Type)                                           ::    GetInput
    class(SampleQuasiMC_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(100)                                                    ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='nb_samples', Value=ConvertToString(Value=This%NbSamples) )
    call GetInput%AddParameter( Name='max_nb_samples', Value=ConvertToString(Value=This%MaxNbSamples) )

    call GetInput%AddParameter( Name='enrichment_scheme', Value=ConvertToString(Value=This%MaxNbSamples) )

    SectionName = 'enrichment_scheme'
    call GetInput%AddSection( SectionName=SectionName )
    select case ( This%EnrichmentScheme )
      case ( 0 )
        call GetInput%AddParameter( Name='multiplier', Value=ConvertToString(Value=This%EnrichmentMultiplier),                    &
                                                                                                          SectionName=SectionName)
      case ( 1 )
        call GetInput%AddParameter( Name='increment', Value=ConvertToString(Value=This%EnrichmentIncrement),                      &
                                                                                                         SectionName=SectionName )
      case ( 2 )
        call GetInput%AddParameter( Name='sequence', Value=ConvertToString(Values=This%EnrichmentSequence),                       &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    call GetInput%AddParameter( Name='enrichment_stage', Value=ConvertToString(Value=This%EnrichmentStage) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sequence'
    call GetInput%AddSection( Section=LowDiscSequence_Factory%GetObjectInput( Object=This%LowDiscSequence,                        &
                                                           MainSectionName='sequence', Prefix=PrefixLoc, Directory=DirectorySub) )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetNbEnrichSamples( This, NbSamples )

    integer                                                           ::    GetNbEnrichSamples

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
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

    class(SampleQuasiMC_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Draw_0D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) call This%Initialize()

    allocate(Draw_0D(This%NbSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_0D', ProcName=ProcName, stat=StatLoc )
    
    Draw_0D = This%LowDiscSequence%Get( NbPoints=This%NbSamples )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Draw_1D( This, NbDim )

    real(rkp), allocatable, dimension(:,:)                            ::    Draw_1D

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
    integer, intent(in)                                               ::    NbDim

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0

    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested sample at or below 0', ProcName=ProcName )

    allocate( Draw_1D(NbDim, This%NbSamples), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Draw_1D', ProcName=ProcName, stat=StatLoc )
    Draw_1D = Zero

    Draw_1D = This%LowDiscSequence%Get( NbPoints=This%NbSamples, NbDim=NbDim )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_0D( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(out)                 ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    SeqStart
    integer                                                           ::    SeqEnd
    integer                                                           ::    NbEnrichmentSamplesLoc

    if ( present(NbEnrichmentSamples) ) then
      NbEnrichmentSamplesLoc = NbEnrichmentSamples
    else
      NbEnrichmentSamplesLoc = This%GetNbEnrichSamples( NbSamples=size(Samples,1) )
    end if 

    if ( NbEnrichmentSamplesLoc < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .false.
    else 
      if ( NbEnrichmentSamplesLoc + size(Samples,1) > This%MaxNbSamples ) then
        Exceeded = .true.
      else
        Exceeded = .false.

        allocate(EnrichmentSamples(NbEnrichmentSamplesLoc), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )
        
        SeqStart = size(Samples,1)+1
        SeqEnd = size(Samples,1)+NbEnrichmentSamplesLoc
        EnrichmentSamples = This%LowDiscSequence%GetPoints( SeqStart=SeqStart, SeqEnd=SeqEnd )
      end if
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Enrich_1D( This, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, ReqNormalized )

    class(SampleQuasiMC_Type), intent(inout)                          ::    This
    real(rkp), dimension(:,:),intent(in)                              ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim=0
    integer(8)                                                        ::    Step_Loc
    real(rkp), allocatable, dimension(:)                              ::    SeqVal
    integer                                                           ::    SeqStart
    integer                                                           ::    SeqEnd
    integer                                                           ::    NbEnrichmentSamplesLoc

    NbDim = size(Samples,1)

    if ( NbDim <= 0 ) call Error%Raise( Line='Dimensionality of requested samples at or below 0', ProcName=ProcName )

    if ( present(NbEnrichmentSamples) ) then
      NbEnrichmentSamplesLoc = NbEnrichmentSamples
    else
      NbEnrichmentSamplesLoc = This%GetNbEnrichSamples( NbSamples=size(Samples,2) )
    end if 

    if ( NbEnrichmentSamplesLoc < 1 ) call Error%Raise( Line='Inquired less than 1 enrichment sample', ProcName=ProcName )

    if ( present(ReqNormalized) ) then
      ReqNormalized = .false.
    else 
      if ( NbEnrichmentSamplesLoc + size(Samples,2)  > This%MaxNbSamples ) then
        Exceeded = .true.
      else
        Exceeded = .false.

        allocate( EnrichmentSamples(NbDim, NbEnrichmentSamplesLoc), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc )
        EnrichmentSamples = Zero

        SeqStart = size(Samples,2)+1
        SeqEnd = size(Samples,2)+NbEnrichmentSamplesLoc
        EnrichmentSamples = This%LowDiscSequence%GetPoints( SeqStart=SeqStart, SeqEnd=SeqEnd, NbDim=NbDim )
      end if
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy( LHS, RHS )

    class(SampleQuasiMC_Type), intent(out)                            ::    LHS
    class(SampleScheme_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    select type (RHS)

      type is (SampleQuasiMC_Type)
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
          allocate(LHS%LowDiscSequence, source=RHS%LowDiscSequence, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%LowDiscSequence', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Finalizer( This )

    type(SampleQuasiMC_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%EnrichmentSequence) ) deallocate(This%EnrichmentSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%EnrichmentSequence', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%LowDiscSequence) ) deallocate(This%LowDiscSequence, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
