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
use StringConversion_Module
use DistProb_Class                                                ,only:    DistProb_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    DistLogNorm_Type

type, extends(DistNorm_Type)                                          ::    DistLogNorm_Type
  logical                                                             ::    DoubleTruncatedLeft
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    AdditionalConstruction
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, public                                                   ::    PDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

real(rkp), parameter                                                  ::    dlogof2pi=dlog(Two*pi)

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(DistLogNorm_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  This%A = - huge(One)
  This%B = huge(One)
  This%Mu = Zero
  This%Sigma = One
  This%TruncatedRight = .false.
  This%TruncatedLeft = .true.
  This%DoubleTruncatedLeft = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine AdditionalConstruction(This)
  
  class(DistLogNorm_Type), intent(inout)                              ::    This 

  character(*), parameter                                             ::    ProcName='ConstructCase1'

  if (This%A > -huge(One)) This%DoubleTruncatedLeft = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetA(This)

  real(rkp)                                                           ::    GetA

  class(DistLogNorm_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='GetA'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (This%DoubleTruncatedLeft) then
    GetA = dexp(This%A)
  else
    GetA = Zero
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetB(This)

  real(rkp)                                                           ::    GetB

  class(DistLogNorm_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='GetB'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (.not. This%TruncatedRight) call Error%Raise(Line='Distribution was never right truncated', ProcName=ProcName)

  GetB = dexp(This%B)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF(This, X)

  real(rkp)                                                           ::    PDF

  class(DistLogNorm_Type), intent(in)                                 ::    This
  real(rkp), intent(in)                                               ::    X

  character(*), parameter                                             ::    ProcName='PDF'
  logical                                                             ::    TripFlag

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  TripFlag = .false.

  if (X <= Zero) then
    PDF = Zero
    TripFlag = .true.
  end if

  if (.not. TripFlag) then
    if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
      PDF = This%ComputeNormalPDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%DoubleTruncatedLeft) then
      PDF = This%ComputeNormalPDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      PDF = This%ComputeNormalPDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      PDF = This%ComputeNormalPDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma)
    end if
    PDF = One/X * PDF
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D(This, NbNodes)

!    real(rkp), allocatable, dimension(:,:)                              ::    PDF_R2D

!    class(DistLogNorm_Type), intent(in)                                 ::    This
!    integer, intent(in)                                                 ::    NbNodes

!    character(*), parameter                                             ::    ProcName='PDF_R2D'
!    real(rkp)                                                           ::    BinMass
!    real(8)                                                             ::    CDFLeft
!    real(8)                                                             ::    CDFRight
!    real(8)                                                             ::    Mu_8
!    real(8)                                                             ::    Sigma_8
!    real(8)                                                             ::    A_8
!    real(8)                                                             ::    B_8
!    real(8)                                                             ::    VarR0D
!    integer                                                             ::    i
!    integer                                                             ::    StatLoc=0

!    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

!    if (NbNodes < 3) call Error%Raise(Line='Specified number of points lower than minimum of 3', ProcName=ProcName)

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='PDF_R2D', ProcName=ProcName, stat=StatLoc)

!    Mu_8 = real(This%Mu,8)
!    Sigma_8 = real(This%Sigma,8)

!    if (This%TruncatedLeft .and. This%A > This%NLargeA) then
!      A_8 = real(This%A,8)
!    else
!      A_8 = real(This%Mu-Six*This%Sigma,8)
!    end if
!    call normal_cdf (A_8, Mu_8, Sigma_8, CDFLeft)

!    if (This%TruncatedRight) then
!      B_8 = real(This%B,8)
!    else
!      B_8 = real(This%Mu+Six*This%Sigma,8)
!    end if
!    call normal_cdf (B_8, Mu_8, Sigma_8, CDFRight)

!    PDF_R2D(1,1) = dexp(A_8)
!    call normal_pdf (A_8, Mu_8, Sigma_8, VarR0D)
!    VarR0D = VarR0D / PDF_R2D(1,1)
!    PDF_R2D(1,2) = VarR0D / (CDFRight - CDFLeft)

!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF(real((i-1),rkp)*BinMass)
!      PDF_R2D(i,2) = This%PDF(PDF_R2D(i,1))
!    end do

!    PDF_R2D(NbNodes,1) = dexp(B_8)
!    call normal_pdf (B_8, Mu_8, Sigma_8, VarR0D)
!    VarR0D = VarR0D / PDF_R2D(NbNodes,1)
!    PDF_R2D(NbNodes,2) = VarR0D / (CDFRight - CDFLeft)

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function CDF(This, X)

  real(rkp)                                                           ::    CDF

  class(DistLogNorm_Type), intent(in)                                 ::    This
  real(rkp), intent(in)                                               ::    X

  character(*), parameter                                             ::    ProcName='CDF'
  logical                                                             ::    TripFlag

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  TripFlag = .false.

  if (X <= Zero) then
    CDF = Zero
    TripFlag = .true.
  end if

  if (.not. TripFlag) then
    if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
      CDF = This%ComputeNormalCDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%DoubleTruncatedLeft) then
      CDF = This%ComputeNormalCDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      CDF = This%ComputeNormalCDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      CDF = This%ComputeNormalCDF(X=dlog(X), Mu=This%Mu, Sigma=This%Sigma)
    end if
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function InvCDF(This, P)

  real(rkp)                                                           ::    InvCDF

  class(DistLogNorm_Type), intent(in)                                 ::    This
  real(rkp), intent(in)                                               ::    P

  character(*), parameter                                             ::    ProcName='InvCDF'
  logical                                                             ::    TripFlag

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  TripFlag = .false.

  if (P == Zero) then
    if (.not. This%DoubleTruncatedLeft) then
      InvCDF = tiny(One)
      TripFlag = .true.
    end if
  end if

  if (.not. TripFlag) then
    if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
      InvCDF = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%DoubleTruncatedLeft) then
      InvCDF = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      InvCDF = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      InvCDF = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma)
    end if
    InvCDF = dexp(InvCDF)
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetMoment(This, Moment)

  real(rkp)                                                           ::    GetMoment

  class(DistLogNorm_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Moment

  character(*), parameter                                             ::    ProcName='GetMoment'
  real(rkp)                                                           ::    CDF_ma0
  real(rkp)                                                           ::    CDF_mb0
  real(rkp)                                                           ::    CDF_a0
  real(rkp)                                                           ::    CDF_b0
  real(rkp)                                                           ::    a0
  real(rkp)                                                           ::    b0
  real(rkp)                                                           ::    am0
  real(rkp)                                                           ::    bm0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

  if (Moment == 0) then
    GetMoment = One
  elseif (.not. (This%DoubleTruncatedLeft .or. This%TruncatedRight)) then
    GetMoment = dexp(real(Moment,rkp)*This%Mu + real(Moment,rkp)**2*This%Sigma**2/Two)
  else
    if (This%TruncatedRight) then
      GetMoment = This%ComputeMomentNumerical(Moment=Moment)
    else
      CDF_a0 = Zero
      CDF_b0 = One
      CDF_ma0 = One
      CDF_mb0 = Zero
      GetMoment = dexp(real(Moment,rkp)*This%Mu + real(Moment,rkp)**2*This%Sigma**2/Two)
      if (This%DoubleTruncatedLeft) then
        a0 = (This%A - This%Mu) / This%Sigma
        am0 = real(Moment,rkp)*This%Sigma-a0
        CDF_a0 = This%ComputeNormalCDF(Mu=Zero, Sigma=One, X=a0)
        CDF_ma0 = This%ComputeNormalCDF(Mu=Zero, Sigma=One, X=am0)
      end if
      if (This%TruncatedRight) then
        b0 = (This%B - This%Mu) / This%Sigma
        bm0 = real(Moment,rkp)*This%Sigma-b0
        CDF_b0 = This%ComputeNormalCDF(Mu=Zero, Sigma=One, X=b0)
        CDF_mb0 = This%ComputeNormalCDF(Mu=Zero, Sigma=One, X=bm0)
      end if
      GetMoment = GetMoment * (CDF_ma0-CDF_mb0) / (CDF_b0-CDF_a0)
    end if

  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteInfo(This, File)

  class(DistLogNorm_Type), intent(in)                                 ::    This
  type(SMUQFile_Type), intent(inout)                                  ::    File

  character(*), parameter                                             ::    ProcName='WriteInfo'
  integer                                                             ::    i
  type(SMUQString_Type), dimension(5)                                 ::    Strings

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  Strings(1) = 'lognormal'
  Strings(2) = ConvertToString(Value=This%Mu)
  Strings(3) = ConvertToString(Value=This%Sigma)
  Strings(4) = '-Inf'
  if (This%DoubleTruncatedLeft) Strings(4) = ConvertToString(Value=This%A)
  Strings(5) = 'Inf'
  if (This%TruncatedRight) Strings(5) = ConvertToString(Value=This%B)

  call File%Append(Strings=Strings)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(DistLogNorm_Type), intent(out)                                ::    LHS
  class(DistProb_Type), intent(in)                                    ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (DistLogNorm_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%A = RHS%A
        LHS%B = RHS%B
        LHS%Mu = RHS%Mu
        LHS%Sigma = RHS%Sigma
        LHS%TruncatedLeft = RHS%TruncatedLeft
        LHS%TruncatedRight = RHS%TruncatedRight
        LHS%DoubleTruncatedLeft = RHS%DoubleTruncatedLeft
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(DistLogNorm_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
