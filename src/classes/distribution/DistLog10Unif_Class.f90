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

module DistLog10Unif_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use DistUnif_Class                                                ,only:    DistUnif_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    DistLog10Unif_Type

type, extends(DistUnif_Type)                                          ::    DistLog10Unif_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, public                                                   ::    PDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
end type

real(rkp), parameter                                                  ::    dlogof10=dlog(Ten)     
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(DistLog10Unif_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'log10uniform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetA(This)

    real(rkp)                                                         ::    GetA

    class(DistLog10Unif_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetA'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetA = Ten**(This%A)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetB(This)

    real(rkp)                                                         ::    GetB

    class(DistLog10Unif_Type), intent(in)                             ::    This

    character(*), parameter                                           ::    ProcName='GetB'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetB = Ten**(This%B)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF(This, X)

    real(rkp)                                                         ::    PDF

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF'
    logical                                                           ::    TripFlag

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    TripFlag = .false.

    if (X <= Zero) then
      PDF = Zero
      TripFlag = .true.
    end if

    if (.not. TripFlag) then
      PDF = This%ComputeUnifPDF(dlog10(X), This%A, This%B)
      PDF = One/(X*dlogof10) * PDF
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF(This, X)

    real(rkp)                                                         ::    CDF

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF'
    logical                                                           ::    TripFlag

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    TripFlag = .false.

    if (X <= Zero) then
      CDF = Zero
      TripFlag = .true.
    end if
  
    if (.not. TripFlag) then
      CDF = This%ComputeUnifCDF(dlog10(X), This%A, This%B)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF(This, P)

    real(rkp)                                                         ::    InvCDF

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    InvCDF = This%ComputeUnifInvCDF(P, This%A, This%B)
    InvCDF = Ten**InvCDF

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment(This, Moment)

    real(rkp)                                                         ::    GetMoment

    class(DistLog10Unif_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'
    real(rkp)                                                         ::    eA
    real(rkp)                                                         ::    eB

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

    if (Moment > 0) then
      eA = Ten**(This%A)
      eB = Ten**(This%B)
      GetMoment = (eB**Moment - eA**Moment) / (real(Moment,rkp)*(This%B-This%A)) * One/dlogof10
    else
      GetMoment = One
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo(This, File)

    class(DistLog10Unif_Type), intent(in)                             ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    i
    type(SMUQString_Type), dimension(3)                               ::    Strings

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    Strings(1) = 'log10uniform'
    Strings(2) = ConvertToString(Value=This%A)
    Strings(3) = ConvertToString(Value=This%B)

    call File%Append(Strings=Strings)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
