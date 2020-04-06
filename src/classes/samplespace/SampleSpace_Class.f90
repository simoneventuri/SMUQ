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

module SampleSpace_Class

use Parameters_Library
use Input_Library
use String_Library
use CommandRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    SampleSpace_Type

type, abstract                                                        ::    SampleSpace_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbDim=0
  logical                                                             ::    Correlated=.false.
  type(DistProbContainer_Type), allocatable, dimension(:)             ::    DistProb
  type(String_Type), allocatable, dimension(:)                        ::    ParamName
  type(String_Type), allocatable, dimension(:)                        ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
contains
  procedure(Initialize_SampleSpace), deferred, public                 ::    Initialize
  procedure(Reset_SampleSpace), deferred, public                      ::    Reset
  procedure(SetDefaults_SampleSpace), deferred, public                ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(ConstructInput_SampleSpace), deferred, private            ::    ConstructInput
  procedure(GetInput_SampleSpace), deferred, public                   ::    GetInput
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsCorrelated
  generic, public                                                     ::    GetDistribution         =>    GetDist0D_Label,        &
                                                                                                          GetDist0D_Num,          &
                                                                                                          GetDist1D
  procedure, private                                                  ::    GetDist0D_Label
  procedure, private                                                  ::    GetDist0D_Num
  procedure, private                                                  ::    GetDist1D
  generic, public                                                     ::    GetLabel                =>    GetLabel0D,             &
                                                                                                          GetLabel1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  generic, public                                                     ::    GetName                 =>    GetName0D_Label,        &
                                                                                                          GetName0D_Num,          &
                                                                                                          GetName1D
  procedure, private                                                  ::    GetName0D_Label
  procedure, private                                                  ::    GetName0D_Num
  procedure, private                                                  ::    GetName1D
  generic, public                                                     ::    GetDistributionPointer  =>    GetDistPointer_Label,   &
                                                                                                          GetDistPointer_Num
  procedure, public                                                   ::    GetDistPointer_Label
  procedure, public                                                   ::    GetDistPointer_Num
  procedure, public                                                   ::    GetCorrMat
  procedure, public                                                   ::    GetCorrMatPointer
  procedure, public                                                   ::    Draw
  procedure, public                                                   ::    Enrich
  procedure, nopass, public                                           ::    DrawMVarNormal
  procedure, public                                                   ::    WriteInfo
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure(Copy_SampleSpace), deferred, public                       ::    Copy
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type),intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------  

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SampleSpace( This, Input, Prefix )
    import                                                            ::    SampleSpace_Type
    import                                                            ::    InputSection_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SampleSpace( This, MainSectionName, Prefix, Directory )
      import                                                          ::    InputSection_Type
      import                                                          ::    SampleSpace_Type
      type(InputSection_Type)                                         ::    GetInput_SampleSpace
      class(SampleSpace_Type), intent(in)                             ::    This
      character(*), intent(in)                                        ::    MainSectionName
      character(*), optional, intent(in)                              ::    Prefix
      character(*), optional, intent(in)                              ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_SampleSpace( LHS, RHS )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(out)                              ::    LHS
    class(SampleSpace_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName0D_Num( This, Num )

    character(:), allocatable                                         ::    GetName0D_Num
    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetName0D_Num'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    GetName0D_Num = This%ParamName(Num)%GetValue()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName0D_Label( This, Label )

    character(:), allocatable                                         ::    GetName0D_Label
    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetName0D_Label'
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    GetName0D_Label = This%ParamName(ii)%GetValue()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName1D( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetName1D
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetName1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetName1D, source=This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetName1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D( This, Num )

    character(:), allocatable                                         ::    GetLabel0D
    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetLabel0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel0D = This%Label(Num)%GetValue()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetLabel1D, source=This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetLabel1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist0D_Num( This, Num )

    class(DistProb_Type), allocatable                                 ::    GetDist0D_Num

    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetDist0D_Num'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    allocate(GetDist0D_Num, source=This%DistProb(Num)%GetPointer(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist0D_Label( This, Label )

    class(DistProb_Type), allocatable                                 ::    GetDist0D_Label

    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetDist0D_Label'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    allocate(GetDist0D_Label, source=This%DistProb(ii)%GetPointer(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist1D( This )

    type(DistProbContainer_Type), allocatable, dimension(:)           ::    GetDist1D

    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetDist1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetDist1D, source=This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPointer_Label( This, Label )

    class(DistProb_Type), pointer                                     ::    GetDistPointer_Label

    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetDistPointer_Label'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    GetDistPointer_Label => This%DistProb(ii)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPointer_Num( This, Num )

    class(DistProb_Type), pointer                                     ::    GetDistPointer_Num

    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetDistPointer_Num'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    GetDistPointer_Num => This%DistProb(Num)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMat( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCorrMat
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetCorrMat'
    integer                                                           ::    StatLoc=0

    allocate(GetCorrMat, source=This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCorrMat', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMatPointer( This )

    real(rkp), pointer, dimension(:,:)                                ::    GetCorrMatPointer
    class(SampleSpace_Type), target, intent(in)                       ::    This

    character(*), parameter                                           ::    ProcName='GetCorrMatPointer'
    integer                                                           ::    StatLoc=0

    GetCorrMatPointer => This%CorrMat

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDim( This )

    integer                                                           ::    GetNbDim
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetNbDim'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbDim = This%NbDim

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrelated( This )

    logical                                                           ::    IsCorrelated
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='IsCorrelated'
    integer                                                           ::    StatLoc=0

    IsCorrelated = This%Correlated

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Draw( This, Sampler, NbSamples )

    real(rkp), allocatable, dimension(:,:)                            ::    Draw

    class(SampleSpace_Type), intent(in)                               ::    This
    class(SampleMethod_Type), intent(inout)                           ::    Sampler
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='Draw'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    integer                                                           ::    i, ii
    class(DistProb_Type), pointer                                     ::    DistProb=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NbDim = This%NbDim

    Draw = Sampler%Draw( NbDim=NbDim, NbSamples=NbSamples )

    if ( .not. This%Correlated ) then
      ii = 1
      do ii = 1, NbSamples
        i = 1
        do i = 1, NbDim
          DistProb => This%DistProb(i)%GetPointer()
          Draw(i,ii) = DistProb%InvCDF(P=Draw(i,ii))
          nullify(DistProb)
        end do
      end do
    else
      allocate(VarR1D(NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

      i = 1
      ii = 0
      do i = 1, NbDim
        DistProb => This%DistProb(i)%GetPointer()
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
        Draw(:,i) = This%DrawMVarNormal( Mu=VarR1D, Cov=This%CorrMat, PVec=Draw(:,i) )
      end do
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich( This, Sampler, NbEnrichmentSamples, Samples, EnrichmentSamples )

    class(SampleSpace_Type), intent(in)                               ::    This
    class(SampleMethod_Type), intent(inout)                           ::    Sampler
    real(rkp), dimension(:,:), target, intent(in)                     ::    Samples
    integer, intent(in)                                               ::    NbEnrichmentSamples
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    EnrichmentSamples

    character(*), parameter                                           ::    ProcName='Enrich'
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

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NbDim = This%NbDim

    call Sampler%Enrich( Samples=Samples, NbEnrichmentSamples=NbEnrichmentSamples, EnrichmentSamples=EnrichmentSamples,           &
                                                                                                     ReqNormalized=ReqNormalized )

    if ( ReqNormalized ) then
      if ( .not. This%Correlated ) then
        allocate(NormalizedSamples, source=Samples, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc )
        NbSamples = size(Samples,2)
        i = 1
        do i = 1, NbSamples
          ii = 1
          do ii = 1, NbDim
            DistProb => This%DistProb(ii)%GetPointer()
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

    call Sampler%Enrich( Samples=SamplesPointer, NbEnrichmentSamples=NbEnrichmentSamples, EnrichmentSamples=EnrichmentSamples )

    NbEnrichSamples = size(EnrichmentSamples,2)

    if ( .not. This%Correlated ) then
      i = 1
      do i = 1, NbEnrichSamples
        ii = 1
        do ii = 1, NbDim
          DistProb => This%DistProb(ii)%GetPointer()
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
        DistProb => This%DistProb(i)%GetPointer()
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
        EnrichmentSamples(:,i) = This%DrawMVarNormal( Mu=VarR1D, Cov=This%CorrMat, PVec=EnrichmentSamples(:,i) )
      end do

      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    nullify(SamplesPointer)

    if ( allocated(NormalizedSamples) ) deallocate(NormalizedSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Draw a vector of real(rkp) random numbers from a Multivariate Normal distribution N~[Mu,Cov] where Mu is a vector of means and 
  ! Cov is the covariance matrix
  function DrawMVarNormal( Mu, Cov, PVec )

    real(rkp), dimension(:), allocatable                              ::    DrawMVarNormal

    real(rkp), dimension(:), intent(in)                               ::    Mu
    real(rkp), dimension(:,:), intent(in)                             ::    Cov
    real(rkp), dimension(:), intent(in)                               ::    PVec

    character(*), parameter                                           ::    ProcName='DrawMVarNormal'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:,:), allocatable                            ::    CovLoc
    real(rkp), dimension(:), allocatable                              ::    VarR1D
!    real(rkp)                                                         ::    Rand1, Rand2
    integer                                                           ::    NbDim
    integer                                                           ::    i, ii, imax
    type(DistNorm_Type)                                               ::    DistNormal

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo( This, Directory )

    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Directory

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    i
    type(SMUQFile_Type)                                               ::    File
    class(DistProb_Type), pointer                                     ::    DistProb=>null()
    character(:), allocatable                                         ::    FileName

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      PrefixLoc = Directory

      FileName = '/variables.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%Label, File=File )

      FileName = '/covariance.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%CorrMat, File=File )

      i = 1
      do i = 1, This%NbDim
        PrefixLoc = Directory // '/' // This%Label(i)
        call MakeDirectory( Path=PrefixLoc, Options='-p' )
        
        FileName = '/name.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call File%Export(String=This%ParamName(i))

        FileName = '/label.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call File%Export(String=This%Label(i))

        FileName = '/distribution.dat'
        DistProb => This%DistProb(i)%GetPointer()
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call DistProb%WriteInfo( File=File )
        nullify(DistProb)
      end do

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
