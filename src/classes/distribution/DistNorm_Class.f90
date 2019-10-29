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

module DistNorm_Class

use Prob_Library
use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    DistNorm_Type

type, extends(DistProb_Type)                                          ::    DistNorm_Type
  real(rkp)                                                           ::    Mu=Zero
  real(rkp)                                                           ::    Sigma=One
  character(:), allocatable                                           ::    MuDependency
  character(:), allocatable                                           ::    SigmaDependency
  character(:), allocatable                                           ::    ADependency
  character(:), allocatable                                           ::    BDependency
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    HierConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF
  procedure, nopass, public                                           ::    ComputeCDF
  procedure, public                                                   ::    InvCDF
  procedure, nopass, public                                           ::    ComputeInvCDF
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, private                                                  ::    ComputeMoment1
  procedure, private                                                  ::    ComputeMoment2
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetSigma
  procedure, nopass, private                                          ::    ComputePhi
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.
real(rkp), parameter                                                  ::    dlogof2pi=dlog(Two*pi)
contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistNorm_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistNorm_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(DistNorm_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = One
    This%B = One
    This%Mu = Zero
    This%Sigma = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.
    This%MuDependency=''
    This%SigmaDependency=''
    This%ADependency=''
    This%BDependency=''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistNorm_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    MandatoryLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    MandatoryLoc = .true.
    ParameterName = 'mu_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MuDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'mu'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Mu = VarR0D

    MandatoryLoc = .true.
    ParameterName = 'sigma_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%SigmaDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'sigma'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Sigma = VarR0D

    ParameterName = 'a_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%ADependency = VarC0D
    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%A = VarR0D
      This%TruncatedLeft = .true.
    end if

    ParameterName = 'b_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BDependency = VarC0D
    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%B = VarR0D
      This%TruncatedRight = .true.
    end if

    if ( This%Sigma <= Zero ) call Error%Raise( Line='Standard deviation specified to be at or below zero', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Mu, Sigma, A, B, Debug )
    
    class(DistNorm_Type), intent(inout)                               ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%Mu = Mu

    This%Sigma = Sigma

    if ( present(A) ) then
      This%A = A
      This%TruncatedLeft = .true.
    end if

    if ( present(B) ) then
      This%B = B
      This%TruncatedRight = .true.
    end if

    if ( This%Sigma < Zero ) call Error%Raise( Line='Standard deviation specified to be below minimum of 0', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine HierConstructCase1( This, Input, Prefix, Debug )

    class(DistNorm_Type), intent(inout)                               ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='HierConstructCase1'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D       

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( len_trim(This%MuDependency) /= 0 ) call Input%GetValue( Value=This%Mu, Label=This%MuDependency )

    if ( len_trim(This%SigmaDependency) /= 0 ) call Input%GetValue( Value=This%Sigma, Label=This%SigmaDependency )

    if ( len_trim(This%ADependency) /= 0 ) then
      call Input%GetValue( Value=This%A, Label=This%ADependency )
      This%TruncatedLeft = .true.
    end if
    
    if ( len_trim(This%BDependency) /= 0 ) then
      call Input%GetValue( Value=This%B, Label=This%BDependency )
      This%TruncatedRight = .true.
    end if

    if ( This%Sigma < Zero ) call Error%Raise( Line='Standard deviation specified to be below minimum of 0', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistNorm_Type), intent(in)                                  ::    This
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='mu', Value=ConvertToString( Value=This%Mu ) )
    call GetInput%AddParameter( Name='sigma', Value=ConvertToString( Value=This%Sigma ) )
    if ( This%TruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )
    if ( len_trim(This%MuDependency) /= 0 ) call GetInput%AddParameter( Name='mu_dependency', Value=This%MuDependency )
    if ( len_trim(This%SigmaDependency) /= 0 ) call GetInput%AddParameter( Name='sigma_dependency', Value=This%SigmaDependency )
    if ( len_trim(This%ADependency) /= 0 ) call GetInput%AddParameter( Name='a_dependency', Value=This%ADependency )
    if ( len_trim(This%BDependency) /= 0 ) call GetInput%AddParameter( Name='b_dependency', Value=This%BDependency )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, Sigma=This%Sigma )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D( This, NbNodes, Debug )

!    real(rkp), allocatable, dimension(:,:)                            ::    PDF_R2D

!    class(DistNorm_Type), intent(in)                                  ::    This
!    integer, intent(in)                                               ::    NbNodes
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='PDF_R2D'
!    real(rkp)                                                         ::    BinMass
!    real(8)                                                           ::    CDFLeft
!    real(8)                                                           ::    CDFRight
!    real(8)                                                           ::    Mu_8
!    real(8)                                                           ::    Sigma_8
!    real(8)                                                           ::    A_8
!    real(8)                                                           ::    B_8
!    real(8)                                                           ::    VarR0D
!    integer                                                           ::    i
!    integer                                                           ::    StatLoc=0

!    DebugLoc = DebugGlobal
!    if ( present(Debug) ) DebugLoc = Debug
!    if (DebugLoc) call Logger%Entering( ProcName )

!    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

!    if ( NbNodes < 3 ) call Error%Raise( Line='Specified number of points lower than minimum of 3', ProcName=ProcName )

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='PDF_R2D', ProcName=ProcName, stat=StatLoc )

!    Mu_8 = real(This%Mu,8)
!    Sigma_8 = real(This%Sigma,8)

!    if ( This%TruncatedLeft ) then
!      A_8 = real(This%A,8)
!    else
!      A_8 = real(This%Mu-Five*This%Sigma,8)
!    end if

!    call normal_cdf ( A_8, Mu_8, Sigma_8, CDFLeft )

!    if ( This%TruncatedRight ) then
!      B_8 = real(This%B,8)
!    else
!      B_8 = real(This%Mu+Five*This%Sigma,8)
!    end if

!    call normal_cdf ( B_8, Mu_8, Sigma_8, CDFRight )

!    PDF_R2D(1,1) = A_8
!    call normal_pdf ( A_8, Mu_8, Sigma_8, VarR0D )
!    PDF_R2D(1,2) = real(VarR0D / ( CDFRight - CDFLeft ), rkp)

!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF( real((i-1),rkp)*BinMass )
!      PDF_R2D(i,2) = This%PDF( PDF_R2D(i,1) )
!    end do

!    PDF_R2D(NbNodes,1) = B_8
!    call normal_pdf ( B_8, Mu_8, Sigma_8, VarR0D )
!    PDF_R2D(NbNodes,2) = VarR0D / ( CDFRight - CDFLeft )

!    if (DebugLoc) call Logger%Exiting()

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( X, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    TripFlag = .false.

    if ( present(A) ) then
      if (X < A) then
        ComputePDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputePDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      CDFLeft = Zero
      if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = dexp( -( 0.5*dlogof2pi + dlog(Sigma) ) - 0.5*((X-Mu)/Sigma)**2 )
      ComputePDF = VarR0D / ( CDFRight - CDFLeft )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, Sigma=This%Sigma )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF( X, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    TripFlag = .false.

    if ( present(A) ) then
      if (X < A) then
        ComputeCDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputeCDF = One
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      CDFLeft = Zero
      if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = 0.5*(One+erf((X-Mu)/(Sigma*dsqrt(Two))))
      ComputeCDF = ( VarR0D - CDFLeft ) / ( CDFRight - CDFLeft )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeInvCDF( P, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputeInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    PLoc
    real(8)                                                           ::    Mu_8
    real(8)                                                           ::    Sigma_8

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    CDFLeft = 0.0
    if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
    CDFRight = 1.0
    if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))

    PLoc = real(CDFLeft+P*(CDFRight-CDFLeft),8)

    Mu_8 = real(Mu,8)
    Sigma_8 = real(Sigma,8)
    call normal_cdf_inv ( PLoc, Mu_8, Sigma_8, VarR0D )

    ComputeInvCDF = VarR0D

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMu( This, Debug )

    real(rkp)                                                         ::    GetMu

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMu'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetMu = This%Mu

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSigma( This, Debug )

    real(rkp)                                                         ::    GetSigma

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetSigma'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetSigma = This%Sigma

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean( This, Debug )

    real(rkp)                                                         ::    GetMean

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMean'
    real(rkp)                                                         ::    Phia
    real(rkp)                                                         ::    Phib
    real(rkp)                                                         ::    Za
    real(rkp)                                                         ::    Zb
    real(rkp)                                                         ::    K

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = This%ComputePhi( X=Za )
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma ) - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedLeft ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = Zero
      Phia = This%ComputePhi( X=Za )
      Phib = Zero
      K = One - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedRight ) then
      Za = Zero
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = Zero
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma )
    else
      Za = Zero
      Zb = Zero
      Phia = Zero
      Phib = Zero
      K = One
    end if

    GetMean = This%Mu + This%Sigma*( (Phia - Phib) / K )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance( This, Debug )

    real(rkp)                                                         ::    GetVariance

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'
    real(rkp)                                                         ::    Phia
    real(rkp)                                                         ::    Phib
    real(rkp)                                                         ::    Za
    real(rkp)                                                         ::    Zb
    real(rkp)                                                         ::    K

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = This%ComputePhi( X=Za )
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma ) - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedLeft ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = Zero
      Phia = This%ComputePhi( X=Za )
      Phib = Zero
      K = One - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedRight ) then
      Za = Zero
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = Zero
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma )
    else
      Za = Zero
      Zb = Zero
      Phia = Zero
      Phib = Zero
      K = One
    end if

    GetVariance = This%Sigma**2 * ( One + (Za*Phia-Zb*Phib)/K - ((Phia-Phib)/K)**2 )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment1( This, Debug )

    real(rkp)                                                         ::    ComputeMoment1

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMoment1'
    real(rkp)                                                         ::    Phia
    real(rkp)                                                         ::    Phib
    real(rkp)                                                         ::    Za
    real(rkp)                                                         ::    Zb
    real(rkp)                                                         ::    K

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = This%ComputePhi( X=Za )
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma ) - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedLeft ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = Zero
      Phia = This%ComputePhi( X=Za )
      Phib = Zero
      K = One - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedRight ) then
      Za = Zero
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = Zero
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma )
    else
      Za = Zero
      Zb = Zero
      Phia = Zero
      Phib = Zero
      K = One
    end if

    ComputeMoment1 = This%Mu + This%Sigma*( (Phia - Phib) / K )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment2( This, Debug )

    real(rkp)                                                         ::    ComputeMoment2

    class(DistNorm_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'
    real(rkp)                                                         ::    Phia
    real(rkp)                                                         ::    Phib
    real(rkp)                                                         ::    Za
    real(rkp)                                                         ::    Zb
    real(rkp)                                                         ::    K

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = This%ComputePhi( X=Za )
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma ) - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedLeft ) then
      Za = (This%A-This%Mu)/This%Sigma
      Zb = Zero
      Phia = This%ComputePhi( X=Za )
      Phib = Zero
      K = One - This%ComputeCDF( X=This%A, Mu=This%Mu, Sigma=This%Sigma )
    elseif ( This%TruncatedRight ) then
      Za = Zero
      Zb = (This%B-This%Mu)/This%Sigma
      Phia = Zero
      Phib = This%ComputePhi( X=Zb )
      K = This%ComputeCDF( X=This%B, Mu=This%Mu, Sigma=This%Sigma )
    else
      Za = Zero
      Zb = Zero
      Phia = Zero
      Phib = Zero
      K = One
    end if

    ComputeMoment2 = This%Mu**2 + Two*This%Mu*This%Sigma*(Phia-Phib)/K + This%Sigma**2 + This%Sigma**2*(Za*Phia-Zb*Phib)/K

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePhi( X, Debug )

    real(rkp)                                                         ::    ComputePhi

    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePhi'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ComputePhi = One/dsqrt(Two*pi)*dexp(-0.5*X**2)
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistNorm_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistNorm_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Mu = RHS%Mu
          LHS%Sigma = RHS%Sigma
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
          LHS%MuDependency = RHS%MuDependency
          LHS%SigmaDependency = RHS%SigmaDependency
          LHS%ADependency = RHS%ADependency
          LHS%BDependency = RHS%BDependency
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistNorm_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
