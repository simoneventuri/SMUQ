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

module SpaceSampler_Class

use String_Library
use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use SampleScheme_Factory_Class                                    ,only:    SampleScheme_Factory
use SampleScheme_Class                                            ,only:    SampleScheme_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type

implicit none

private

public                                                                ::    SpaceSampler_Type

type                                                                  ::    SpaceSampler_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  class(SampleScheme_Type), pointer                                   ::    Sampler=>null()
  type(RandPseudo_Type)                                               ::    RNG
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Draw                    =>    DrawSpace
  procedure, private                                                  ::    DrawSpace
  generic, public                                                     ::    Enrich                  =>    EnrichSpace
  procedure, private                                                  ::    EnrichSpace
  procedure, public                                                   ::    DrawMVarNormal
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(SpaceSampler_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'spacesampler'
      This%Initialized = .True.
    end if

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(SpaceSampler_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%Sampler) ) deallocate( This%Sampler, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Sampler', ProcName=ProcName, stat=StatLoc )

    call This%RNG%Reset()

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(SpaceSampler_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(SpaceSampler_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    if (DebugLoc) call Logger%Write( "Processing passed down settings")

    SectionName = 'scheme'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call SampleScheme_Factory%ConstructPointer( Object=This%Sampler, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'rng'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found ) then
      call This%RNG%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    else
      call This%RNG%Construct()
    end if

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Sampler, RNG, Debug )

    class(SpaceSampler_Type), intent(inout)                           ::    This
    class(SampleScheme_Type), intent(in)                              ::    Sampler
    type(RandPseudo_Type), optional, intent(in)                       ::    RNG
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%Sampler, source=Sampler, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Sampler', ProcName=ProcName, stat=StatLoc )

    if ( present(RNG) ) then
      This%RNG = RNG
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(SpaceSampler_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'scheme'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/scheme'
    call GetInput%AddSection( Section=SampleScheme_Factory%GetObjectInput( Object=This%Sampler, MainSectionName=SectionName,      &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub ) )

    SectionName = 'rng'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection( Section=This%RNG%GetInput( MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function DrawSpace( This, SpaceInput, Debug)

    use DistNorm_Class                                            ,only:    DistNorm_Type

    real(rkp), allocatable, dimension(:,:)                            ::    DrawSpace 

    class(SpaceSampler_Type), intent(inout)                           ::    This
    class(SpaceInput_Type), intent(in)                                ::    SpaceInput
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='DrawSpace'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    integer                                                           ::    i, ii
    class(DistProb_Type), pointer                                     ::    DistProb=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    NbSamples

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NbDim = SpaceInput%GetNbDim()

    DrawSpace = This%Sampler%Draw(NbDim=NbDim)
    NbSamples = size(DrawSpace,2)

    if ( .not. SpaceInput%IsCorrelated() ) then
      ii = 1
      do ii = 1, NbSamples
        i = 1
        do i = 1, NbDim
          DistProb => SpaceInput%GetDistributionPointer(Num=i)
          DrawSpace(i,ii) = DistProb%InvCDF(P=DrawSpace(i,ii))
        end do
      end do
    else

      allocate(VarR1D(NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

      i = 1
      ii = 0
      do i = 1, NbDim
        DistProb => SpaceInput%GetDistributionPointer(Num=i)
        select type ( DistProb )
          type is (DistNorm_Type)
            VarR1D(i) = DistProb%GetMu()
            ii = ii + 1
          class default
            exit
        end select
      end do
  
      if ( ii < NbDim ) call Error%Raise( Line='Sampling of correlated non normal multivariate spaces is not yet supported',      &
                                                                                                               ProcName=ProcName )

      i = 1
      do i  = 1, NbSamples
        DrawSpace(:,i) = This%DrawMVarNormal( Mu=VarR1D, Cov=SpaceInput%GetCorrMat(), PVec=DrawSpace(:,i) )
      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine EnrichSpace( This, SpaceInput, Samples, EnrichmentSamples, NbEnrichmentSamples, Exceeded, Debug)

    use DistNorm_Class                                            ,only:    DistNorm_Type

    class(SpaceSampler_Type), intent(inout)                           ::    This
    class(SpaceInput_Type), intent(in)                                ::    SpaceInput
    real(rkp), dimension(:,:), target, intent(in)                     ::    Samples
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    EnrichmentSamples
    integer, optional, intent(in)                                     ::    NbEnrichmentSamples
    logical, intent(out)                                              ::    Exceeded
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='EnrichSpace'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    integer                                                           ::    i, ii
    class(DistProb_Type), pointer                                     ::    DistProb=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    NbSamples
    integer                                                           ::    NbEnrichSamples
    logical                                                           ::    ReqNormalized
    real(rkp), target, allocatable, dimension(:,:)                    ::    NormalizedSamples
    real(rkp), pointer, dimension(:,:)                                ::    SamplesPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NbDim = SpaceInput%GetNbDim()

    call This%Sampler%Enrich( Samples=Samples, EnrichmentSamples=EnrichmentSamples, Exceeded=Exceeded,ReqNormalized=ReqNormalized)

    if ( ReqNormalized ) then
      if ( .not. SpaceInput%IsCorrelated() ) then
        allocate(NormalizedSamples, source=Samples, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc )
        NbSamples = size(Samples,2)
        i = 1
        do i = 1, NbSamples
          ii = 1
          do ii = 1, NbDim
            DistProb => SpaceInput%GetDistributionPointer(Num=ii)
            NormalizedSamples(ii,i) = DistProb%CDF(X=Samples(ii,i))
            nullify(DistProb)
          end do
        end do
        SamplesPointer => NormalizedSamples
      else
        call Error%Raise( Line='Enrichment of samples from correlated spaces with methods that require normalized, ' //           &
                                                                       'independent values not yet supported', ProcName=ProcName )
      end if
    else
      SamplesPointer => Samples
    end if

    if ( present(NbEnrichmentSamples) ) then
      call This%Sampler%Enrich( Samples=SamplesPointer, EnrichmentSamples=EnrichmentSamples,                                      &
                                                                      NbEnrichmentSamples=NbEnrichmentSamples, Exceeded=Exceeded )
    else
      call This%Sampler%Enrich( Samples=SamplesPointer, EnrichmentSamples=EnrichmentSamples, Exceeded=Exceeded )
    end if

    if ( Exceeded ) then
      continue
    else
      NbEnrichSamples = size(EnrichmentSamples,2)
      if ( .not. SpaceInput%IsCorrelated() ) then
        i = 1
        do i = 1, NbEnrichSamples
          ii = 1
          do ii = 1, NbDim
            DistProb => SpaceInput%GetDistributionPointer(Num=ii)
            EnrichmentSamples(ii,i) = DistProb%InvCDF(P=EnrichmentSamples(ii,i))
            nullify(DistProb)
          end do
        end do
      else
        allocate(VarR1D(NbDim), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

        i = 1
        ii = 0
        do i = 1, NbDim
          DistProb => SpaceInput%GetDistributionPointer(Num=i)
          select type ( DistProb )
            type is (DistNorm_Type)
              VarR1D(i) = DistProb%GetMu()
              ii = ii + 1
            class default
              exit
          end select
        end do
    
        if ( ii < NbDim ) call Error%Raise( Line='Sampling of correlated non normal multivariate spaces is not yet supported',  &
                                                                                                             ProcName=ProcName )

        do i = 1, NbEnrichSamples
          EnrichmentSamples(:,i) = This%DrawMVarNormal( Mu=VarR1D, Cov=SpaceInput%GetCorrMat(), PVec=EnrichmentSamples(:,i) )
        end do

        deallocate(VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    nullify(SamplesPointer)

    if ( allocated(NormalizedSamples) ) deallocate(NormalizedSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  subroutine DrawDistVecSamples( This, Distributions, CorrMat, NbSamples, Debug)

!    use DistNorm_Class                                            ,only:    DistNorm_Type
!    use ArrayRoutines_Module

!    real(rkp), allocatable, dimension(:,:)                            ::    DrawDistVecSamples

!    class(SpaceSampler_Type), intent(inout)                           ::    This
!    type(DistProb_Vec_Type), dimension(:), intent(in)                 ::    Distributions
!    real(rkp), dimension(:,:), optional, intent(in)                   ::    CorrMat
!    integer, intent(in)                                               ::    NbSamples
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='DrawDistVecSamples'
!    integer                                                           ::    StatLoc=0
!    integer                                                           ::    NbDim
!    integer                                                           ::    i, ii
!    class(DistProb_Type), pointer                                     ::    DistProb=>null()
!    real(rkp), allocatable, dimension(:)                              ::    VarR1D

!    DebugLoc = DebugGlobal
!    if ( present(Debug) ) DebugLoc = Debug
!    if (DebugLoc) call Logger%Entering( ProcName )

!    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

!    if ( NbSamples <= 0 ) call Error%Raise( Line='Number of requested samples at or below 0', ProcName=ProcName )

!    if ( present(CorrMat) ) then
!      if ( size(Distributions,1) /= size(CorrMat,1) ) call Error%Raise( Line='Invalid CorrMat', ProcName=ProcName )
!      if ( size(CorrMat,1) /= size(CorrMat,2) ) call Error%Raise( Line='CorrMat has an invalid shape', ProcName=ProcName)
!    end if

!    NbDim = size(Distributions,1)

!    allocate( DrawDistVecSamples(NbDim, NbSamples), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='DrawDistVecSamples', ProcName=ProcName, stat=StatLoc )

!    DrawDistVecSamples = This%RNG%Draw(NbDim=NbDim, NbSamples=NbSamples)

!    if ( .not. IsDiagonal(CorrMat) ) then
!      ii = 1
!      do ii = 1, NbSamples
!        i = 1
!        do i = 1, NbDim
!          DistProb => Distributions(i)%GetPointer()
!          DrawDistVecSamples(i,ii) = DistProb%InvCDF(P=DrawDistVecSamples(i,ii))
!        end do
!      end do
!    else
!      allocate(VarR1D(NbDim), stat=StatLoc)
!      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

!      i = 1
!      ii = 0
!      do i = 1, NbDim
!        DistProb => Distributions(i)%GetPointer()
!        select type ( DistProb )
!          type is (DistNorm_Type)
!            VarR1D(i) = DistProb%GetMu()
!            ii = ii + 1
!          class default
!            exit
!        end select
!      end do
!  
!      if ( ii < NbDim ) call Error%Raise( Line='Sampling of correlated non-multivariate normal spaces is not yet supported',      &
!                                                                                                               ProcName=ProcName )

!      i = 1
!      do i = 1, NbSamples
!        DrawDistVecSamples(:,i) = This%DrawMVarNormal( Mu=VarR1D, Cov=CorrMat, PVec=DrawDistVecSamples(:,i) )
!      end do
!    end if

!    if (DebugLoc) call Logger%Exiting()

!  end subroutine
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Draw a vector of real(rkp) random numbers from a Multivariate Normal distribution N~[Mu,Cov] where Mu is a vector of means and 
  ! Cov is the covariance matrix
  function DrawMVarNormal( This, Mu, Cov, PVec, Debug )

    use DistNorm_Class                                            ,only:    DistNorm_Type

    real(rkp), dimension(:), allocatable                              ::    DrawMVarNormal

    class(SpaceSampler_Type), intent(inout)                           ::    This
    real(rkp), dimension(:), intent(in)                               ::    Mu
    real(rkp), dimension(:,:), intent(in)                             ::    Cov
    real(rkp), dimension(:), intent(in)                               ::    PVec
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='DrawMVarNormal'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:,:), allocatable                            ::    CovLoc
    real(rkp), dimension(:), allocatable                              ::    VarR1D
!    real(rkp)                                                         ::    Rand1, Rand2
    integer                                                           ::    NbDim
    integer                                                           ::    i, ii, imax
    type(DistNorm_Type)                                               ::    DistNormal

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(Cov,1) /= size(Cov,2) ) call Error%Raise( Line='Covariance matrix is not a square matrix', ProcName=ProcName )

    NbDim = size(Mu,1)
    if ( size(Cov,1) /= NbDim ) call Error%Raise( Line='Leading dimension of Cov larger than number of means', ProcName=ProcName )

    allocate( CovLoc, source=Cov, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CovLoc', ProcName=ProcName, stat=StatLoc )

    call DPOTRF( 'U', NbDim, CovLoc, NbDim, StatLoc )

    if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DPOTRF', ProcName=ProcName )

    allocate( DrawMVarNormal(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DrawMVarNormal', ProcName=ProcName, stat=StatLoc )

    allocate( VarR1D(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    call DistNormal%Construct(Mu=Zero, Sigma=One)

    i = 1
    do i = 1, NbDim
      VarR1D(i) = DistNormal%InvCDF(P=PVec(i))
    end do

!    ! Box-Muller for independent standard normal random numbers

!    allocate( RandVal(NbDim), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='RandVal', ProcName=ProcName, stat=StatLoc )

!    i = 1
!    ii = 0
!    do i = 1, NbDim
!      Rand1 = This%RNG%Draw()
!      Rand2 = This%RNG%Draw()
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dcos(real(Two*pi*Rand2,8))
!      if ( ii >= NbDim ) exit
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dsin(real(Two*pi*Rand2,8))
!      if ( ii >= NbDim ) exit
!    end do

    i = 1
    do i = 1, NbDim
      DrawMVarNormal(i) = dot_product(VarR1D(1:i),CovLoc(1:i,i)) + Mu(i)
    end do

    deallocate( VarR1D, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    deallocate( CovLoc, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='CovLoc', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  ! Draw a vector of real(rkp) random numbers from a Multivariate Normal distribution N~[Mu,Cov] where Mu is a vector of means and 
!  ! Cov is the covariance matrix

!  function DrawMVarNormal( This, Mu, Cov, Debug )

!    real(rkp), dimension(:), allocatable                              ::    DrawMVarNormal

!    class(SpaceSampler_Type), intent(inout)                               ::    This
!    real(rkp), dimension(:), intent(in)                               ::    Mu
!    real(rkp), dimension(:,:), intent(in)                             ::    Cov
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='DrawMVarNormal'
!    integer(8)                                                        ::    NbDim
!    real(rkp), allocatable, dimension(:)                              ::    PARM
!    real(rkp), allocatable, dimension(:)                              ::    WORK
!    integer                                                           ::    StatLoc=0

!    DebugLoc = DebugGlobal
!    if ( present(Debug) ) DebugLoc = Debug
!    if (DebugLoc) call Logger%Entering( ProcName )

!    NbDim=size(Mu,1)

!    allocate(PARM(NbDim*(NbDim+3)/2+1), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='PARM', ProcName=ProcName, stat=StatLoc )
!    PARM = Zero

!    allocate(WORK(NbDim), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
!    WORK = Zero

!    allocate(DrawMVarNormal, source=Mu, stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='DrawMVarNormal', ProcName=ProcName, stat=StatLoc )

!    call SETGMN(DrawMVarNormal, real(cov,4), NbDim, PARM)
!    call GENMN(PARM, DrawMVarNormal, WORK)

!    deallocate(PARM, stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PARM', ProcName=ProcName, stat=StatLoc )
!    deallocate(WORK, stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

!    if (DebugLoc) call Logger%Exiting()

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed( This, Debug )

    logical                                                           ::    IsConstructed

    class(SpaceSampler_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='DrawMVarNormal'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsConstructed = This%Constructed

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(SpaceSampler_Type), intent(out)                             ::    LHS
    class(SpaceSampler_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()

    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed
    
    if ( RHS%Constructed ) then
      allocate(LHS%Sampler, source=RHS%Sampler, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%RNG', ProcName=ProcName, stat=StatLoc )
      LHS%RNG = RHS%RNG
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )
  
    type(SpaceSampler_Type)                                           ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if( associated(This%Sampler) ) deallocate( This%Sampler, stat=StatLoc )
    if( StatLoc /= 0 ) call Error%Deallocate( Name='This%Sampler', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
