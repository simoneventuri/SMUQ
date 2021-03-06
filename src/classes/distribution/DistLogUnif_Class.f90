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

module DistLogUnif_Class

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

public                                                                ::    DistLogUnif_Type

type, extends(DistUnif_Type)                                          ::    DistLogUnif_Type
contains
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, public                                                   ::    PDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
function GetA(This)

  real(rkp)                                                           ::    GetA

  class(DistLogUnif_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='GetA'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetA = dexp(This%A)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetB(This)

  real(rkp)                                                           ::    GetB

  class(DistLogUnif_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='GetB'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetB = dexp(This%B)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF(This, X)

  real(rkp)                                                           ::    PDF

  class(DistLogUnif_Type), intent(in)                                 ::    This
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
    PDF = This%ComputeUnifPDF(dlog(X), This%A, This%B)
    PDF = One/X * PDF
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D(This, NbNodes)

!    real(rkp), allocatable, dimension(:,:)                              ::    PDF_R2D

!    class(DistLogUnif_Type), intent(in)                                 ::    This
!    integer, intent(in)                                                 ::    NbNodes

!    character(*), parameter                                             ::    ProcName='PDF_R2D'
!    real(rkp)                                                           ::    BinMass
!    integer                                                             ::    StatLoc=0
!    integer                                                             ::    i


!    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

!    if (NbNodes < 3) call Error%Raise(Line='Specified number of points lower than minimum of 3', ProcName=ProcName)

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='PDF_R2D', ProcName=ProcName, stat=StatLoc)

!    PDF_R2D(1,1) = dexp(This%A)
!    PDF_R2D(1,2) = One / (PDF_R2D(1,1)*(This%B-This%A))
!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF(real((i-1),rkp)*BinMass)
!      PDF_R2D(i,2) = This%PDF(PDF_R2D(i,1))
!    end do
!    PDF_R2D(NbNodes,1) = dexp(This%B)
!    PDF_R2D(NbNodes,2) = One / (PDF_R2D(NbNodes,1)*(This%B-This%A))

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function CDF(This, X)

  real(rkp)                                                           ::    CDF

  class(DistLogUnif_Type), intent(in)                                 ::    This
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
    CDF = This%ComputeUnifCDF(dlog(X), This%A, This%B)
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function InvCDF(This, P)

  real(rkp)                                                           ::    InvCDF

  class(DistLogUnif_Type), intent(in)                                 ::    This
  real(rkp), intent(in)                                               ::    P

  character(*), parameter                                             ::    ProcName='InvCDF'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  InvCDF = This%ComputeUnifInvCDF(P, This%A, This%B)
  InvCDF = dexp(InvCDF)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetMoment(This, Moment)

  real(rkp)                                                           ::    GetMoment

  class(DistLogUnif_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Moment

  character(*), parameter                                             ::    ProcName='GetMoment'
  real(rkp)                                                           ::    eA
  real(rkp)                                                           ::    eB

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

  if (Moment > 0) then
    eA = dexp(This%A)
    eB = dexp(This%B)
    GetMoment = (eB**Moment - eA**Moment) / (real(Moment,rkp)*(This%B-This%A))
  else
    GetMoment = One
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteInfo(This, File)

  class(DistLogUnif_Type), intent(in)                                 ::    This
  type(SMUQFile_Type), intent(inout)                                  ::    File

  character(*), parameter                                             ::    ProcName='WriteInfo'
  integer                                                             ::    i
  type(SMUQString_Type), dimension(3)                                 ::    Strings

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  Strings(1) = 'loguniform'
  Strings(2) = ConvertToString(Value=This%A)
  Strings(3) = ConvertToString(Value=This%B)

  call File%Append(Strings=Strings)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
