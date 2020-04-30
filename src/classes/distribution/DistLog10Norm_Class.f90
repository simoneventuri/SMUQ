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

module DistLog10Norm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use StringRoutines_Module
use DistProb_Class                                                ,only:    DistProb_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    DistLog10Norm_Type

type, extends(DistNorm_Type)                                          ::    DistLog10Norm_Type
  logical                                                             ::    DoubleTruncatedLeft
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    AdditionalConstruction
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, private                                                  ::    PDF_R0D
  procedure, public                                                   ::    CDF_R0D
  procedure, public                                                   ::    InvCDF_R0D
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
end type

real(rkp), parameter                                                  ::    dlogof10=dlog(Ten)
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(DistLog10Norm_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'log10normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(DistLog10Norm_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

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
    
    class(DistLog10Norm_Type), intent(inout)                          ::    This 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if (This%A > -huge(One)) This%DoubleTruncatedLeft = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetA(This)

    real(rkp)                                                         ::    GetA

    class(DistLog10Norm_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetA'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%DoubleTruncatedLeft) then
      GetA = Ten**(This%A)
    else
      GetA = Zero
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetB(This)

    real(rkp)                                                         ::    GetB

    class(DistLog10Norm_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetB'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (.not. This%TruncatedRight) call Error%Raise(Line='Distribution was never right truncated', ProcName=ProcName)

    GetB = Ten**(This%B)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D(This, X)

    real(rkp)                                                         ::    PDF_R0D

    class(DistLog10Norm_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF_R0D'
    logical                                                           ::    TripFlag

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    TripFlag = .false.

    if (X <= Zero) then
      PDF_R0D = Zero
      TripFlag = .true.
    end if

    if (.not. TripFlag) then
      if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
        PDF_R0D = This%ComputeNormalPDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
      else if (This%DoubleTruncatedLeft) then
        PDF_R0D = This%ComputeNormalPDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A)
      else if (This%TruncatedRight) then
        PDF_R0D = This%ComputeNormalPDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B)
      else
        PDF_R0D = This%ComputeNormalPDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma)
      end if
      PDF_R0D = One/(X*dlogof10) * PDF_R0D
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D(This, X)

    real(rkp)                                                         ::    CDF_R0D

    class(DistLog10Norm_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF_R0D'
    logical                                                           ::    TripFlag

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    TripFlag = .false.

    if (X <= Zero) then
      CDF_R0D = Zero
      TripFlag = .true.
    end if
  
    if (.not. TripFlag) then
      if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
        CDF_R0D = This%ComputeNormalCDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
      else if (This%DoubleTruncatedLeft) then
        CDF_R0D = This%ComputeNormalCDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, A=This%A)
      else if (This%TruncatedRight) then
        CDF_R0D = This%ComputeNormalCDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma, B=This%B)
      else
        CDF_R0D = This%ComputeNormalCDF(X=dlog10(X), Mu=This%Mu, Sigma=This%Sigma)
      end if
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D(This, P)

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistLog10Norm_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF_R0D'
    logical                                                           ::    TripFlag

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    TripFlag = .false.

    if (P == Zero) then
      if (.not. This%DoubleTruncatedLeft) then
        InvCDF_R0D = tiny(One)
        TripFlag = .true.
      end if
    end if

    if (.not. TripFlag) then
      if (This%TruncatedRight .and. This%DoubleTruncatedLeft) then
        InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A, B=This%B)
      else if (This%DoubleTruncatedLeft) then
        InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, A=This%A)
      else if (This%TruncatedRight) then
        InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma, B=This%B)
      else
        InvCDF_R0D = This%ComputeNormalInvCDF(P=P, Mu=This%Mu, Sigma=This%Sigma)
      end if
      InvCDF_R0D = Ten**InvCDF_R0D
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment(This, Moment)

    real(rkp)                                                         ::    GetMoment

    class(DistLog10Norm_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

    if (Moment > 0) then
      GetMoment = This%ComputeMomentNumerical(Moment=Moment)
    else
      GetMoment = One
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo(This, File)

    class(DistLog10Norm_Type), intent(in)                             ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    i
    type(SMUQString_Type), dimension(5)                               ::    Strings

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    Strings(1) = 'log10normal'
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

    class(DistLog10Norm_Type), intent(out)                            ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (DistLog10Norm_Type)
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
          LHS%DoubleTruncatedLeft = RHS%DoubleTruncatedLeft
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
