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
use DistUnif_Class                    ,only:  DistUnif_Type
use Logger_Class                      ,only:  Logger
use Error_Class                       ,only:  Error

implicit none

private

public                                                                ::    DistLog10Unif_Type

type, extends(DistUnif_Type)                                          ::    DistLog10Unif_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    GetMoment 
end type

real(rkp), parameter                                                  ::    dlogof10=dlog(Ten)     
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistLog10Unif_Type), intent(inout)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Write( "Initializing DistLog10Unif object" )
    if ( .not. This%Initialized ) then
      This%Name = 'log10uniform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if
    if (DebugLoc) call Logger%Write( "Initialization Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    PDF_R0D = This%ComputePDF( dlog10(X), This%A, This%B )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D( This, NbNodes, Debug )

!    real(rkp), allocatable, dimension(:,:)                            ::    PDF_R2D

!    class(DistLog10Unif_Type), intent(in)                             ::    This
!    integer, intent(in)                                               ::    NbNodes
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='PDF_R2D'
!    real(rkp)                                                         ::    BinMass
!    integer                                                           ::    StatLoc=0
!    integer                                                           ::    i

!    DebugLoc = DebugGlobal
!    if ( present(Debug) ) DebugLoc = Debug
!    if (DebugLoc) call Logger%Entering( ProcName )

!    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

!    if ( NbNodes < 3 ) call Error%Raise( Line='Specified number of points lower than minimum of 3', ProcName=ProcName )

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='PDF_R2D', ProcName=ProcName, stat=StatLoc )

!    PDF_R2D(1,1) = Ten**(This%A)
!    PDF_R2D(1,2) = One / ( dlogof10 * PDF_R2D(1,1) * (This%B-This%A) )
!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF( real((i-1),rkp)*BinMass )
!      PDF_R2D(i,2) = This%PDF( PDF_R2D(i,1) )
!    end do
!    PDF_R2D(NbNodes,1) = Ten**(This%B)
!    PDF_R2D(NbNodes,2) = One / ( dlogof10 * PDF_R2D(NbNodes,1) * (This%B-This%A) )

!    if (DebugLoc) call Logger%Exiting()

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( X, A, B, Debug )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < A .or. X > B ) then
      ComputePDF = Zero
    else
      ComputePDF = One / ( dlogof10*(Ten**(X))*(B-A) )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    CDF = This%ComputeCDF( dlog10(X), This%A, This%B )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistLog10Unif_Type), intent(in)                             ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    InvCDF = This%ComputeInvCDF( P, This%A, This%B )
    InvCDF = Ten**InvCDF

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment( This, Moment, Debug )

    real(rkp)                                                         ::    GetMoment

    class(DistLog10Unif_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Moment
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMoment'
    real(rkp)                                                         ::    eA
    real(rkp)                                                         ::    eB

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment > 0 ) then
      eA = Ten**(This%A)
      eB = Ten**(This%B)
      GetMoment = (eB**Moment - eA**Moment) / (real(Moment,rkp)*(This%B-This%A)) * One/dlogof10
    else
      GetMoment = One
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module DistLog10Unif_Class
