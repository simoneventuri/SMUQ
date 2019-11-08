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
! defined using shape and rate form
module DistGamma_Class

use CDF_Library
use Prob_Library
use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use InputDet_Class                                                ,only:    InputDet_Type
use SampleLHS_Class                                               ,only:    SampleLHS_Type

implicit none

private

public                                                                ::    DistGamma_Type

type, extends(DistProb_Type)                                          ::    DistGamma_Type
  real(rkp)                                                           ::    Alpha=One
  real(rkp)                                                           ::    Beta=One
  logical                                                             ::    DoubleTruncatedLeft
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF
  procedure, nopass, public                                           ::    ComputeCDF
  procedure, public                                                   ::    InvCDF
  procedure, nopass, public                                           ::    ComputeInvCDF
  procedure, public                                                   ::    GetAlpha
  procedure, public                                                   ::    GetBeta
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistGamma_Type), intent(inout)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'gamma'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistGamma_Type), intent(inout)                              ::    This
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

    class(DistGamma_Type), intent(inout)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = Zero
    This%B = One
    This%Alpha = One
    This%Beta = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .true.
    This%DoubleTruncatedLeft = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistGamma_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    PrefixLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'alpha'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Alpha = VarR0D

    ParameterName = 'beta'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Beta = VarR0D

    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%A = VarR0D
    if ( This%A > Zero ) This%DoubleTruncatedLeft = .true.

    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%B = VarR0D
      This%TruncatedRight = .true.
    end if

    if ( VarR0D <= Zero ) call Error%Raise( "Alpha parameter at or below zero" )
    if ( VarR0D <= Zero ) call Error%Raise( "Beta parameter at or below zero" )
    if ( This%A < Zero ) call Error%Raise( Line='Lower limit specified to be below minimum of 0', ProcName=ProcName )
    if ( This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    call This%AdditionalConstruction()

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Alpha, Beta, A, B, Debug )
    
    class(DistGamma_Type), intent(inout)                              ::    This
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
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

    if ( Alpha <= Zero ) call Error%Raise( "Alpha parameter at or below zero" )
    This%Alpha = Alpha

    if ( Beta <= Zero ) call Error%Raise( "Beta parameter at or below zero" )
    This%Beta = Beta

    if ( present(A) ) This%A = A
    if ( This%A > Zero ) This%DoubleTruncatedLeft = .true.
    if ( This%A < Zero ) call Error%Raise( Line='Lower limit specified to be below minimum of 0', ProcName=ProcName )

    if ( present(B) ) then
      This%B = B
      This%TruncatedRight = .true.
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

    class(DistGamma_Type), intent(in)                                 ::    This
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
    call GetInput%AddParameter( Name='alpha', Value=ConvertToString( Value=This%Alpha ) )
    call GetInput%AddParameter( Name='beta', Value=ConvertToString( Value=This%Beta ) )
    if ( This%DoubleTruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B )
    else if ( This%DoubleTruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A )
    else if ( This%TruncatedRight ) then
      PDF_R0D = This%ComputePDF( X=X, Alpha=This%Alpha, Beta=This%Beta, B=This%B )
    else
      PDF_R0D = This%ComputePDF( X=X, Alpha=This%Alpha, Beta=This%Beta )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D( This, NbNodes, Debug )

!    real(rkp), allocatable, dimension(:,:)                            ::    PDF_R2D

!    class(DistGamma_Type), intent(in)                                 ::    This
!    integer, intent(in)                                               ::    NbNodes
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='PDF_R2D'
!    real(rkp)                                                         ::    BinMass
!    real(8)                                                           ::    CDFLeft
!    real(8)                                                           ::    CDFRight
!    real(8)                                                           ::    k
!    real(8)                                                           ::    theta
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

!    k = real(This%Alpha,8)
!    theta = real(One/This%Beta,8)

!    if ( This%TruncatedLeft ) then
!      A_8 = This%A
!      call gamma_cdf( A_8, real(0,8), theta, k, CDFLeft )
!    else
!      A_8 = 1.E-8
!      CDFLeft = This%CDF( X=real(A_8,rkp) )
!    end if

!    if ( This%TruncatedRight ) then
!      B_8 = This%B
!      call gamma_cdf( B_8, real(0,8), theta, k, CDFRight )
!    else
!      CDFRight = One - real(1.E-6,8)
!      B_8 = This%InvCDF( P=CDFRight )
!    end if

!    PDF_R2D(1,1) = A_8
!    call gamma_pdf( A_8, real(0,8), theta, k, VarR0D )
!    PDF_R2D(1,2) = VarR0D / ( CDFRight - CDFLeft )

!    i = 2
!    do i = 2, NbNodes-1
!      PDF_R2D(i,1) = This%InvCDF( real((i-1),rkp)*BinMass )
!      PDF_R2D(i,2) = This%PDF( PDF_R2D(i,1) )
!    end do

!    PDF_R2D(NbNodes,1) = B_8
!    call gamma_pdf( B_8, real(0,8), theta, k, VarR0D )
!    PDF_R2D(NbNodes,2) = VarR0D / ( CDFRight - CDFLeft )

!    if (DebugLoc) call Logger%Exiting()

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputePDF( X, Alpha, Beta, A, B, Debug )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(8)                                                           ::    CDFLeft
    real(8)                                                           ::    CDFRight
    real(8)                                                           ::    k
    real(8)                                                           ::    theta
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    ALoc
    real(8)                                                           ::    BLoc
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    TripFlag = .false.

    if ( present(A) ) then
      if (X < A ) then
        ComputePDF = Zero
        TripFlag=.true.
      end if
    else
      if ( X <= Zero ) then
        ComputePDF = Zero
        TripFlag = .true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputePDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      k = real(Alpha,8)
      theta = real(One/Beta,8)
      CDFLeft = 0.
      if ( present(A) ) call gamma_cdf( real(A,8), real(0,8), theta, k, CDFLeft )
      CDFRight = 1.
      if ( present(B) ) call gamma_cdf( real(B,8), real(0,8), theta, k, CDFRight )
      call gamma_pdf( real(X,8), real(0,8), theta, k, VarR0D )
      ComputePDF = real(VarR0D / ( CDFRight - CDFLeft ),rkp) 
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B )
    else if ( This%DoubleTruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A )
    else if ( This%TruncatedRight ) then
      CDF = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, B=This%B )
    else
      CDF = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputeCDF( X, Alpha, Beta, A, B, Debug )

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(8)                                                           ::    CDFLeft
    real(8)                                                           ::    CDFRight
    real(8)                                                           ::    k
    real(8)                                                           ::    theta
    real(8)                                                           ::    VarR0D
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < Zero ) call Error%Raise( Line='Gamma distribution is defined on the positive part of the line', ProcName=ProcName)

    TripFlag = .false.

    if ( present(A) ) then
      if (X < A) then
        ComputeCDF = Zero
        TripFlag=.true.
      end if
    else
      if ( X <= Zero ) then
        ComputeCDF = Zero
        TripFlag = .true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputeCDF = One
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      k = real(Alpha,8)
      theta = real(One/Beta,8)
      CDFLeft = 0.
      if ( present(A) ) call gamma_cdf( real(A,8), real(0,8), theta, k, CDFLeft )
      CDFRight = 1.
      if ( present(B) ) call gamma_cdf( real(B,8), real(0,8), theta, k, CDFRight )
      call gamma_cdf( real(X,8), real(0,8), theta, k, VarR0D )
      ComputeCDF = real(( VarR0D - CDFLeft ) / ( CDFRight - CDFLeft ),rkp)
    end if 
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B )
    else if ( This%DoubleTruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, A=This%A )
    else if ( This%TruncatedRight ) then
      InvCDF = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, B=This%B )
    else
      InvCDF = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputeInvCDF( P, Alpha, Beta, A, B, Debug )

    use CDF_Library
    use StringRoutines_Module

    real(rkp)                                                         ::    ComputeInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(8)                                                           ::    CDFLeft
    real(8)                                                           ::    CDFRight
    real(8)                                                           ::    k
    real(8)                                                           ::    theta
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    PLoc
    real(8)                                                           ::    QLoc
    integer                                                           ::    StatLoc=0
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    TripFlag = .false.

    if ( P == Zero ) then
      if ( present(A) ) then
        ComputeInvCDF = A
      else
        ComputeInvCDF = tiny(One)
      end if
      TripFlag=.true.
    end if

    if ( P == One ) then
      if ( present(B) ) then
        ComputeInvCDF = B
      else
        ComputeInvCDF = huge(One)
      end if
      TripFlag=.true.
    end if

    if ( .not. TripFlag ) then
      k = real(Alpha,8)
      theta = real(One/Beta,8)
      CDFLeft = 0.
      if ( present(A) ) call gamma_cdf( real(A,8), real(0,8), theta, k, CDFLeft )
      CDFRight = 1.
      if ( present(B) ) call gamma_cdf( real(B,8), real(0,8), theta, k, CDFRight )
      PLoc = CDFLeft+real(P,8)*(CDFRight-CDFLeft)
      QLoc = real(1,8) - PLoc
      call gamma_inc_inv ( k, VarR0D, real(-1.,8), PLoc, QLoc, StatLoc )
      if ( StatLoc < 0 ) call Error%Raise( Line='Something went wrong with gamma_inc_inv where stat=' //                          &
                                                                                   ConvertToString( StatLoc ), ProcName=ProcName )
      ComputeInvCDF = real(VarR0D,rkp) / Beta
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAlpha( This, Debug )

    real(rkp)                                                         ::    GetAlpha

    class(DistGamma_Type), intent(in)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAlpha'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetAlpha = This%Alpha

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBeta( This, Debug )

    real(rkp)                                                         ::    GetBeta

    class(DistGamma_Type), intent(in)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetBeta'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetBeta = This%Beta

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment( This, Moment, Debug )

    real(rkp)                                                         ::    GetMoment

    class(DistLog10Norm_Type), intent(in)                             ::    This
    integer, intent(in)                                               ::    Moment
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMoment'
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment > 0 ) then
      if ( .not. ( ( This%A > tiny(One) ) .or. This%TruncatedRight ) then
        GetMoment = One
        i = 1
        do i = 1, Moment
          GetMoment = GetMoment * (-This%Alpha-real((i-1),rkp))*(-One/This%Beta)
        end do
      else
        GetMoment = This%ComputeMomentNumerical( Moment=Moment )
      end if
    else
      GetMoment = One
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistGamma_Type), intent(out)                                ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistGamma_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Alpha = RHS%Alpha
          LHS%Beta = RHS%Beta
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
          LHS%DoubleTruncatedLeft = RHS%DoubleTruncatedRight
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistGamma_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
