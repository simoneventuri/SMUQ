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
use DistProb_Class                                                ,only:    DistProb_Type
use DistNorm_Class                                                ,only:  DistNorm_Type
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error

implicit none

private

public                                                                ::    DistLogNorm_Type

type, extends(DistNorm_Type)                                          ::    DistLogNorm_Type
  logical                                                             ::    DoubleTruncatedLeft
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    AdditionalConstruction
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, private                                                  ::    PDF_R0D
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    Copy
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
    This%DoubleTruncatedLeft = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AdditionalConstruction( This, Debug )
    
    class(DistLogNorm_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%A > -huge(One) ) This%DoubleTruncatedLeft = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetA( This, Debug )

    real(rkp)                                                         ::    GetA

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetA'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%DoubleTruncatedLeft ) then
      GetA = dexp(This%A)
    else
      GetA = Zero
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetB( This, Debug )

    real(rkp)                                                         ::    GetB

    class(DistLogNorm_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetB'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%TruncatedRight ) call Error%Raise( Line='Distribution was never right truncated', ProcName=ProcName )

    GetB = dexp(This%B)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistLogNorm_Type), intent(in)                               ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    TripFlag = .false.

    if ( X <= Zero ) then
      PDF_R0D = Zero
      TripFlag = .true.
    end if

    if ( .not. TripFlag ) then
      if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
        PDF_R0D = This%ComputeNormalPDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
      else if ( This%DoubleTruncatedLeft ) then
        PDF_R0D = This%ComputeNormalPDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A )
      else if ( This%TruncatedRight ) then
        PDF_R0D = This%ComputeNormalPDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B )
      else
        PDF_R0D = This%ComputeNormalPDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma )
      end if
      PDF_R0D = One/X * PDF_R0D
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
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistLogNorm_Type), intent(in)                               ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    TripFlag = .false.

    if ( X <= Zero ) then
      CDF = Zero
      TripFlag = .true.
    end if

    if ( .not. TripFlag ) then
      if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
        CDF = This%ComputeNormalCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
      else if ( This%DoubleTruncatedLeft ) then
        CDF = This%ComputeNormalCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A )
      else if ( This%TruncatedRight ) then
        CDF = This%ComputeNormalCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B )
      else
        CDF = This%ComputeNormalCDF( X=dlog(X), Mu=This%Mu, Sigma=This%Sigma )
      end if
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
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    TripFlag = .false.

    if ( P == Zero ) then
      if ( .not. This%DoubleTruncatedLeft ) then
        InvCDF = tiny(One)
        TripFlag = .true.
      end if
    end if

    if ( .not. TripFlag ) then
      if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
        InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B )
      else if ( This%DoubleTruncatedLeft ) then
        InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A )
      else if ( This%TruncatedRight ) then
        InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B )
      else
        InvCDF = This%ComputeNormalInvCDF( P=P, Mu=This%Mu, Sigma=This%Sigma )
      end if
      InvCDF = dexp(InvCDF)
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment( This, Moment, Debug )

    real(rkp)                                                         ::    GetMoment

    class(DistLogNorm_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Moment
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMoment'
    real(rkp)                                                         ::    CDF_ma0
    real(rkp)                                                         ::    CDF_mb0
    real(rkp)                                                         ::    CDF_a0
    real(rkp)                                                         ::    CDF_b0
    real(rkp)                                                         ::    a0
    real(rkp)                                                         ::    b0
    real(rkp)                                                         ::    am0
    real(rkp)                                                         ::    bm0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment == 0 ) then
      GetMoment = One
    elseif ( .not. ( This%DoubleTruncatedLeft .or. This%TruncatedRight ) ) then
      GetMoment = dexp( real(Moment,rkp)*This%Mu + real(Moment,rkp)**2*This%Sigma**2/Two )
    else
      CDF_a0 = Zero
      CDF_b0 = One
      CDF_ma0 = One
      CDF_mb0 = Zero
      if ( This%DoubleTruncatedLeft ) then
        a0 = ( This%A - This%Mu ) / This%Sigma
        am0 = real(Moment,rkp)*This%Sigma-a0
        CDF_a0 = This%ComputeNormalCDF( Mu=Zero, Sigma=One, X=a0 )
        CDF_ma0 = This%ComputeNormalCDF( Mu=Zero, Sigma=One, X=am0 )
      end if
      if ( This%TruncatedRight ) then
        b0 = ( This%B - This%Mu ) / This%Sigma
        bm0 = real(Moment,rkp)*This%Sigma-b0
        CDF_b0 = This%ComputeNormalCDF( Mu=Zero, Sigma=One, X=b0 )
        CDF_mb0 = This%ComputeNormalCDF( Mu=Zero, Sigma=One, X=bm0 )
      end if

      GetMoment = dexp( real(Moment,rkp)*This%Mu + real(Moment,rkp)**2*This%Sigma**2/Two ) * (CDF_ma0-CDF_mb0) / (CDF_b0-CDF_a0)

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistLogNorm_Type), intent(out)                              ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistLogNorm_Type)
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
          LHS%DoubleTruncatedLeft = RHS%DoubleTruncatedLeft
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
