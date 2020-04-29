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
use StringRoutines_Module
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

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
  procedure, public                                                   ::    CDF_R0D
  procedure, nopass, public                                           ::    ComputeNormalCDF
  procedure, public                                                   ::    InvCDF_R0D
  procedure, nopass, public                                           ::    ComputeNormalInvCDF
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetSigma
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.
real(rkp), parameter                                                  ::    dlogof2pi=dlog(Two*pi)
contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(DistNorm_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(DistNorm_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(DistNorm_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = One
    This%B = One
    This%Mu = Zero
    This%Sigma = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(DistNorm_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'mu'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true., Found=Found)
    This%Mu = VarR0D

    ParameterName = 'sigma'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true., Found=Found)
    This%Sigma = VarR0D

    ParameterName = 'a'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%A = VarR0D
      This%TruncatedLeft = .true.
    end if

    ParameterName = 'b'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%B = VarR0D
      This%TruncatedRight = .true.
    end if

    if (This%Sigma <= Zero) call Error%Raise(Line='Standard deviation specified to be at or below zero', ProcName=ProcName)

    if (This%TruncatedLeft .and. This%TruncatedRight) then
      if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)
    end if

    call This%AdditionalConstruction()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Mu, Sigma, A, B)
    
    class(DistNorm_Type), intent(inout)                               ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    This%Mu = Mu

    This%Sigma = Sigma

    if (present(A)) then
      This%A = A
      This%TruncatedLeft = .true.
    end if

    if (present(B)) then
      This%B = B
      This%TruncatedRight = .true.
    end if

    if (This%Sigma <= Zero) call Error%Raise(Line='Standard deviation specified to be at or below zero', ProcName=ProcName)

    if (This%TruncatedLeft .and. This%TruncatedRight) then
      if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)
    end if

    call This%AdditionalConstruction()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistNorm_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    call GetInput%AddParameter(Name='mu', Value=ConvertToString(Value=This%Mu))
    call GetInput%AddParameter(Name='sigma', Value=ConvertToString(Value=This%Sigma))
    if (This%TruncatedLeft) call GetInput%AddParameter(Name='a', Value=ConvertToString(Value=This%A))
    if (This%TruncatedRight) call GetInput%AddParameter(Name='b', Value=ConvertToString(Value=This%B))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AdditionalConstruction(This)
    
    class(DistNorm_Type), intent(inout)                               ::    This 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D(This, X)

    real(rkp)                                                         ::    PDF_R0D

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%TruncatedRight .and. This%TruncatedLeft) then
      PDF_R0D = This%ComputeNormalPDF(X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%TruncatedLeft) then
      PDF_R0D = This%ComputeNormalPDF(X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      PDF_R0D = This%ComputeNormalPDF(X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      PDF_R0D = This%ComputeNormalPDF(X=X, Mu=This%Mu, Sigma=This%Sigma)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D(This, NbNodes)

!    real(rkp), allocatable, dimension(:,:)                            ::    PDF_R2D

!    class(DistNorm_Type), intent(in)                                  ::    This
!    integer, intent(in)                                               ::    NbNodes

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

!    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

!    if (NbNodes < 3) call Error%Raise(Line='Specified number of points lower than minimum of 3', ProcName=ProcName)

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='PDF_R2D', ProcName=ProcName, stat=StatLoc)

!    Mu_8 = real(This%Mu,8)
!    Sigma_8 = real(This%Sigma,8)

!    if (This%TruncatedLeft) then
!      A_8 = real(This%A,8)
!    else
!      A_8 = real(This%Mu-Five*This%Sigma,8)
!    end if

!    call normal_cdf (A_8, Mu_8, Sigma_8, CDFLeft)

!    if (This%TruncatedRight) then
!      B_8 = real(This%B,8)
!    else
!      B_8 = real(This%Mu+Five*This%Sigma,8)
!    end if

!    call normal_cdf (B_8, Mu_8, Sigma_8, CDFRight)

!    PDF_R2D(1,1) = A_8
!    call normal_pdf (A_8, Mu_8, Sigma_8, VarR0D)
!    PDF_R2D(1,2) = real(VarR0D / (CDFRight - CDFLeft), rkp)

!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF(real((i-1),rkp)*BinMass)
!      PDF_R2D(i,2) = This%PDF(PDF_R2D(i,1))
!    end do

!    PDF_R2D(NbNodes,1) = B_8
!    call normal_pdf (B_8, Mu_8, Sigma_8, VarR0D)
!    PDF_R2D(NbNodes,2) = VarR0D / (CDFRight - CDFLeft)

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNormalPDF(X, Mu, Sigma, A, B)

    real(rkp)                                                         ::    ComputeNormalPDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeNormalPDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    TripFlag

    TripFlag = .false.

    if (present(A)) then
      if (X < A) then
        ComputeNormalPDF = Zero
        TripFlag=.true.
      end if
    end if

    if (present(B)) then
      if (X > B) then
        ComputeNormalPDF = Zero
        TripFlag=.true.
      end if
    end if

    if (.not. TripFlag) then
      CDFLeft = Zero
      if (present(A)) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if (present(B)) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = dexp(-(0.5*dlogof2pi + dlog(Sigma)) - 0.5*((X-Mu)/Sigma)**2)
      ComputeNormalPDF = VarR0D / (CDFRight - CDFLeft)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D(This, X)

    real(rkp)                                                         ::    CDF_R0D

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%TruncatedRight .and. This%TruncatedLeft) then
      CDF_R0D = This%ComputeNormalCDF(X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%TruncatedLeft) then
      CDF_R0D = This%ComputeNormalCDF(X=X, Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      CDF_R0D = This%ComputeNormalCDF(X=X, Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      CDF_R0D = This%ComputeNormalCDF(X=X, Mu=This%Mu, Sigma=This%Sigma)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNormalCDF(X, Mu, Sigma, A, B)

    real(rkp)                                                         ::    ComputeNormalCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeNormalCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    TripFlag

    TripFlag = .false.

    if (present(A)) then
      if (X < A) then
        ComputeNormalCDF = Zero
        TripFlag=.true.
      end if
    end if

    if (present(B)) then
      if (X > B) then
        ComputeNormalCDF = One
        TripFlag=.true.
      end if
    end if

    if (.not. TripFlag) then
      CDFLeft = Zero
      if (present(A)) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = One
      if (present(B)) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      VarR0D = 0.5*(One+erf((X-Mu)/(Sigma*dsqrt(Two))))
      ComputeNormalCDF = (VarR0D - CDFLeft) / (CDFRight - CDFLeft)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D(This, P)

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistNorm_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%TruncatedRight .and. This%TruncatedLeft) then
      InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
    else if (This%TruncatedLeft) then
      InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A)
    else if (This%TruncatedRight) then
      InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B)
    else
      InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNormalInvCDF(P, Mu, Sigma, A, B)

    real(rkp)                                                         ::    ComputeNormalInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeNormalInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    PLoc
    real(8)                                                           ::    Mu_8
    real(8)                                                           ::    Sigma_8
    logical                                                           ::    TripFlag

    if (P < Zero) call Error%Raise(Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName)
    if (P > One) call Error%Raise(Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName)

    TripFlag = .false.

    if (P == Zero) then
      if (present(A)) then
        ComputeNormalInvCDF = A
      else
        ComputeNormalInvCDF = -huge(One)
      end if
      TripFlag=.true.
    end if

    if (P == One) then
      if (present(B)) then
        ComputeNormalInvCDF = B
      else
        ComputeNormalInvCDF = huge(One)
      end if
      TripFlag=.true.
    end if

    if (.not. TripFlag) then
      CDFLeft = 0.0
      if (present(A)) CDFLeft = 0.5*(One+erf((A-Mu)/(Sigma*dsqrt(Two))))
      CDFRight = 1.0
      if (present(B)) CDFRight = 0.5*(One+erf((B-Mu)/(Sigma*dsqrt(Two))))
      PLoc = real(CDFLeft+P*(CDFRight-CDFLeft),8)
      Mu_8 = real(Mu,8)
      Sigma_8 = real(Sigma,8)
      call normal_cdf_inv (PLoc, Mu_8, Sigma_8, VarR0D)
      ComputeNormalInvCDF = VarR0D
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMu(This)

    real(rkp)                                                         ::    GetMu

    class(DistNorm_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetMu'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetMu = This%Mu

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetSigma(This)

    real(rkp)                                                         ::    GetSigma

    class(DistNorm_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetSigma'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetSigma = This%Sigma

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Based on formulas given in :
  ! https://people.sc.fsu.edu/~%20jburkardt/presentations/truncated_normal.pdf
  function GetMoment(This, Moment)

    real(rkp)                                                         ::    GetMoment

    class(DistNorm_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Moment

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

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

    if (Moment == 0) then
      GetMoment = One
    elseif (.not. (This%TruncatedLeft .or. This%TruncatedRight)) then
      GetMoment = Zero
      i = 0
      imax = floor(real(Moment,rkp)/Two) 
      do i = 0, imax
        GetMoment = GetMoment + real(BinomialCoeff(Top=Moment , Bottom=2*i),rkp) * real(DoubleFactorial(N=(2*i-1)),rkp) *     &
                                                                                         This%Sigma**(2*i) * This%Mu**(Moment-2*i)
      end do
    else
      Alpha = One
      Beta = One
      PDF_A = Zero
      PDF_B = Zero
      CDF_A = Zero
      CDF_B = One
      if (This%TruncatedLeft) then
        Alpha = (This%A-This%Mu)/This%Sigma
        PDF_A = This%ComputeNormalPDF(X=Alpha, Mu=Zero, Sigma=One)
        CDF_A = This%ComputeNormalCDF(X=Alpha, Mu=Zero, Sigma=One)
      end if
      if (This%TruncatedRight) then
        Beta = (This%B-This%Mu)/This%Sigma
        PDF_B = This%ComputeNormalPDF(X=Beta, Mu=Zero, Sigma=One)
        CDF_B = This%ComputeNormalCDF(X=Beta, Mu=Zero, Sigma=One)
      end if
      GetMoment = Zero
      L = One
      Lim1 = One
      Lim2 = One
      i = 0
      do i = 0, Moment
        if (i > 1) then
          L = - (Beta**(i-1)*PDF_B - Alpha**(i-1)*PDF_A)/(CDF_B - CDF_A) + real(i-1,rkp)*Lim2
        elseif (i == 1) then
          L = - (PDF_B - PDF_A)/(CDF_B - CDF_A)
        else
          L = One
        end if
        GetMoment = GetMoment + real(BinomialCoeff(Top=Moment, Bottom=i),rkp) * This%Sigma**i * This%Mu**(Moment-i) * L
        Lim2 = Lim1
        Lim1 = L
      end do

    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo(This, File)

    class(DistNorm_Type), intent(in)                                  ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    i
    type(String_Type), dimension(5)                                   ::    Strings

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    Strings(1) = 'normal'
    Strings(2) = ConvertToString(Value=This%Mu)
    Strings(3) = ConvertToString(Value=This%Sigma)
    Strings(4) = '-Inf'
    if (This%TruncatedLeft) Strings(4) = ConvertToString(Value=This%A)
    Strings(5) = 'Inf'
    if (This%TruncatedRight) Strings(5) = ConvertToString(Value=This%B)

    call File%Append(Strings=Strings)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(DistNorm_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (DistNorm_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Mu = RHS%Mu
          LHS%Sigma = RHS%Sigma
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)
  
    type(DistNorm_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
