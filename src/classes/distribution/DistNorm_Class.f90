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
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    AdditionalConstruction
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputeNormalPDF
  procedure, public                                                   ::    CDF
  procedure, nopass, public                                           ::    ComputeNormalCDF
  procedure, public                                                   ::    InvCDF
  procedure, nopass, public                                           ::    ComputeNormalInvCDF
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetSigma
  procedure, public                                                   ::    GetMoment
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'mu'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true., Found=Found )
    This%Mu = VarR0D

    ParameterName = 'sigma'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true., Found=Found )
    This%Sigma = VarR0D

    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%A = VarR0D
      This%TruncatedLeft = .true.
    end if

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

    call This%AdditionalConstruction()

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

    if ( This%Sigma <= Zero ) call Error%Raise( Line='Standard deviation specified to be below minimum of 0', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    call This%AdditionalConstruction()

    This%Constructed = .true.

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

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AdditionalConstruction( This, Debug )
    
    class(DistNorm_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
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
      PDF_R0D = This%ComputeNormalPDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft ) then
      PDF_R0D = This%ComputeNormalPDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight ) then
      PDF_R0D = This%ComputeNormalPDF( X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      PDF_R0D = This%ComputeNormalPDF( X=X, Mu=This%Mu, Sigma=This%Sigma )
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
  function ComputeNormalPDF( X, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputeNormalPDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeNormalPDF'
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
        ComputeNormalPDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputeNormalPDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      CDFLeft = Zero
      if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = dexp( -( 0.5*dlogof2pi + dlog(Sigma) ) - 0.5*((X-Mu)/Sigma)**2 )
      ComputeNormalPDF = VarR0D / ( CDFRight - CDFLeft )
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
      CDF = This%ComputeNormalCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft ) then
      CDF = This%ComputeNormalCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight ) then
      CDF = This%ComputeNormalCDF( X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      CDF = This%ComputeNormalCDF( X=X, Mu=This%Mu, Sigma=This%Sigma )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNormalCDF( X, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputeNormalCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeNormalCDF'
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
        ComputeNormalCDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputeNormalCDF = One
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      CDFLeft = Zero
      if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = 0.5*(One+erf((X-Mu)/(Sigma*dsqrt(Two))))
      ComputeNormalCDF = ( VarR0D - CDFLeft ) / ( CDFRight - CDFLeft )
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
      InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft ) then
      InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight ) then
      InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNormalInvCDF( P, Mu, Sigma, A, B, Debug )

    real(rkp)                                                         ::    ComputeNormalInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeNormalInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    PLoc
    real(8)                                                           ::    Mu_8
    real(8)                                                           ::    Sigma_8
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    TripFlag = .false.

    if ( P == Zero ) then
      if ( present(A) ) then
        ComputeNormalInvCDF = A
      else
        ComputeNormalInvCDF = -huge(One)
      end if
      TripFlag=.true.
    end if

    if ( P == One ) then
      if ( present(B) ) then
        ComputeNormalInvCDF = B
      else
        ComputeNormalInvCDF = huge(One)
      end if
      TripFlag=.true.
    end if

    if ( .not. TripFlag ) then
      CDFLeft = 0.0
      if ( present(A) ) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = 1.0
      if ( present(B) ) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      PLoc = real(CDFLeft+P*(CDFRight-CDFLeft),8)
      Mu_8 = real(Mu,8)
      Sigma_8 = real(Sigma,8)
      call normal_cdf_inv ( PLoc, Mu_8, Sigma_8, VarR0D )
      ComputeNormalInvCDF = VarR0D
    end if

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
  ! Based on formulas given in :
  ! https://people.sc.fsu.edu/~%20jburkardt/presentations/truncated_normal.pdf
  function GetMoment( This, Moment, Debug )

    real(rkp)                                                         ::    GetMoment

    class(DistNorm_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Moment
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMoment'
    integer                                                           ::    i
    integer                                                           ::    imax
    real(rkp)                                                         ::    L
    real(rkp)                                                         ::    Lim1
    real(rkp)                                                         ::    Lim2
    real(rkp)                                                         ::    PDF_A
    real(rkp)                                                         ::    PDF_B
    real(rkp)                                                         ::    CDF_A
    real(rkp)                                                         ::    CDF_B
    real(rkp)                                                         ::    Alpha
    real(rkp)                                                         ::    Beta

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment == 0 ) then
      GetMoment = One
    elseif ( .not. ( This%TruncatedLeft .or. This%TruncatedRight ) ) then
      GetMoment = Zero
      i = 0
      imax = floor(real(Moment,rkp)/Two) 
      do i = 0, imax
        GetMoment = GetMoment + real(BinomialCoeff( Top=Moment , Bottom=2*i ),rkp) * real(DoubleFactorial( N=(2*i-1) ),rkp) *     &
                                                                                         This%Sigma**(2*i) * This%Mu**(Moment-2*i)
      end do
    else
      Alpha = One
      Beta = One
      PDF_A = Zero
      PDF_B = Zero
      CDF_A = Zero
      CDF_B = One
      if ( This%TruncatedLeft ) then
        Alpha = (This%A-This%Mu)/This%Sigma
        PDF_A = This%ComputeNormalPDF( X=Alpha, Mu=Zero, Sigma=One )
        CDF_A = This%ComputeNormalCDF( X=Alpha, Mu=Zero, Sigma=One )
      end if
      if ( This%TruncatedRight ) then
        Beta = (This%B-This%Mu)/This%Sigma
        PDF_B = This%ComputeNormalPDF( X=Beta, Mu=Zero, Sigma=One )
        CDF_B = This%ComputeNormalCDF( X=Beta, Mu=Zero, Sigma=One )
      end if
      GetMoment = Zero
      L = One
      Lim1 = One
      Lim2 = One
      i = 0
      do i = 0, Moment
        if ( i > 1 ) then
          L = - (Beta**(i-1)*PDF_B - Alpha**(i-1)*PDF_A)/(CDF_B - CDF_A) + real(i-1,rkp)*Lim2
        elseif ( i == 1 ) then
          L = - (PDF_B - PDF_A)/(CDF_B - CDF_A)
        else
          L = One
        end if
        GetMoment = GetMoment + real(BinomialCoeff( Top=Moment, Bottom=i ),rkp) * This%Sigma**i * This%Mu**(Moment-i) * L
        Lim2 = Lim1
        Lim1 = L
      end do

    end if

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
