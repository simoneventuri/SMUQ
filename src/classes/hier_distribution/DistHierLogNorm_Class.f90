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

module DistLogNorm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use DistNorm_Class                                                ,only:  DistNorm_Type
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error

implicit none

private

public                                                                ::    DistLogNorm_Type

type, extends(DistNorm_Type)                                          ::    DistLogNorm_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, private                                                  ::    ComputeMoment1
  procedure, private                                                  ::    ComputeMoment2
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

real(rkp), parameter                                                  ::    dlogof2pi=dlog(Two*pi)

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistLogNorm_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Write( "Initializing DistLogNorm object" )

    if ( .not. This%Initialized ) then
      This%Name = 'lognormal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Write( "Initialization Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(DistLogNorm_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = - huge(One)
    This%B = huge(One)
    This%Mu = Zero
    This%Sigma = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistLogNorm_Type), intent(in)                               ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      PDF_R0D = This%ComputePDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      PDF_R0D = This%ComputePDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D( This, NbNodes, Debug )

!    real(rkp), allocatable, dimension(:,:)                            ::    PDF_R2D

!    class(DistLogNorm_Type), intent(in)                               ::    This
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

!    if ( This%TruncatedLeft .and. This%A > This%NLargeA ) then
!      A_8 = real(This%A,8)
!    else
!      A_8 = real(This%Mu-Six*This%Sigma,8)
!    end if
!    call normal_cdf ( A_8, Mu_8, Sigma_8, CDFLeft )

!    if ( This%TruncatedRight ) then
!      B_8 = real(This%B,8)
!    else
!      B_8 = real(This%Mu+Six*This%Sigma,8)
!    end if
!    call normal_cdf ( B_8, Mu_8, Sigma_8, CDFRight )

!    PDF_R2D(1,1) = dexp(A_8)
!    call normal_pdf ( A_8, Mu_8, Sigma_8, VarR0D )
!    VarR0D = VarR0D / PDF_R2D(1,1)
!    PDF_R2D(1,2) = VarR0D / ( CDFRight - CDFLeft )

!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF( real((i-1),rkp)*BinMass )
!      PDF_R2D(i,2) = This%PDF( PDF_R2D(i,1) )
!    end do

!    PDF_R2D(NbNodes,1) = dexp(B_8)
!    call normal_pdf ( B_8, Mu_8, Sigma_8, VarR0D )
!    VarR0D = VarR0D / PDF_R2D(NbNodes,1)
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
      VarR0D = dexp( -( 0.5*dlogof2pi + dlog(Sigma) + X ) - 0.5*((X-Mu)/Sigma)**2 )
      ComputePDF = VarR0D / ( CDFRight - CDFLeft )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistLogNorm_Type), intent(in)                               ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      CDF = This%ComputeCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B )
    else
      CDF = This%ComputeCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistLogNorm_Type), intent(in)                               ::    This
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

    InvCDF = dexp(InvCDF)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean( This, Debug )

    real(rkp)                                                         ::    GetMean

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMean'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetMean = This%ComputeMoment1()

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance( This, Debug )

    real(rkp)                                                         ::    GetVariance

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'
    real(rkp)                                                         ::    Ex
    real(rkp)                                                         ::    Ex2

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetVariance = This%ComputeMoment2() - (This%ComputeMoment1())**2

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment1( This, Debug )

    real(rkp)                                                         ::    ComputeMoment1

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMoment1'
    real(rkp)                                                         ::    Mup
    real(rkp)                                                         ::    eA
    logical                                                           ::    eAg0
    real(rkp)                                                         ::    Pma
    real(rkp)                                                         ::    Pmb
    real(rkp)                                                         ::    Pa
    real(rkp)                                                         ::    Pb

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    Mup = This%Mu + This%Sigma**2
    eA = dexp(This%A)
    eAg0 = .false.
    if ( eA > tiny(One) ) eAg0  = .true.

    if ( eAg0 .and. This%TruncatedRight ) then
      Pa = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pb = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pma = This%ComputeCDF(X=This%A, Mu=Mup, Sigma=This%Sigma)
      Pmb = This%ComputeCDF(X=This%B, Mu=Mup, Sigma=This%Sigma)
    elseif ( eAg0 ) then
      Pa = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pb = One
      Pma = This%ComputeCDF(X=This%A, Mu=Mup, Sigma=This%Sigma)
      Pmb = One
    elseif ( This%TruncatedRight ) then
      Pa = Zero
      Pb = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pma = Zero
      Pmb = This%ComputeCDF(X=This%B, Mu=Mup, Sigma=This%Sigma)
    else
      Pa = Zero
      Pb = One
      Pma = Zero
      Pmb = One
    end if

    ComputeMoment1 = dexp((Two*This%Mu+This%Sigma**2)/Two) * (Pmb-Pma) / (Pb-Pa)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment2( This, Debug )

    real(rkp)                                                         ::    ComputeMoment2

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMoment2'
    real(rkp)                                                         ::    Mup
    real(rkp)                                                         ::    eA
    logical                                                           ::    eAg0
    real(rkp)                                                         ::    Pma
    real(rkp)                                                         ::    Pmb
    real(rkp)                                                         ::    Pa
    real(rkp)                                                         ::    Pb

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    Mup = This%Mu + Two*This%Sigma**2
    eA = dexp(This%A)
    eAg0 = .false.
    if ( eA > tiny(One) ) eAg0  = .true.

    if ( eAg0 .and. This%TruncatedRight ) then
      Pa = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pb = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pma = This%ComputeCDF(X=This%A, Mu=Mup, Sigma=This%Sigma)
      Pmb = This%ComputeCDF(X=This%B, Mu=Mup, Sigma=This%Sigma)
    elseif ( eAg0 ) then
      Pa = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pb = One
      Pma = This%ComputeCDF(X=This%A, Mu=Mup, Sigma=This%Sigma)
      Pmb = One
    elseif ( This%TruncatedRight ) then
      Pa = Zero
      Pb = This%ComputeCDF(X=This%A, Mu=This%Mu, Sigma=This%Sigma)
      Pma = Zero
      Pmb = This%ComputeCDF(X=This%B, Mu=Mup, Sigma=This%Sigma)
    else
      Pa = Zero
      Pb = One
      Pma = Zero
      Pmb = One
    end if

    ComputeMoment2 = dexp(Two*This%Mu+Two*This%Sigma**2) * (Pmb-Pma) / (Pb-Pa)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
