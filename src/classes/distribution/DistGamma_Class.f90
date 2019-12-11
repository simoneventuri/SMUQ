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
  procedure, public                                                   ::    CDF_R0D
  procedure, nopass, public                                           ::    ComputeCDF
  procedure, public                                                   ::    InvCDF_R0D
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
  subroutine Initialize( This )

    class(DistGamma_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'gamma'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(DistGamma_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(DistGamma_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = Zero
    This%B = One
    This%Alpha = One
    This%Beta = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .true.
    This%DoubleTruncatedLeft = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(DistGamma_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    PrefixLoc

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

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Alpha, Beta, A, B )
    
    class(DistGamma_Type), intent(inout)                              ::    This
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

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

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistGamma_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X )

    real(rkp)                                                         ::    PDF_R0D

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF_R0D'

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputePDF( X, Alpha, Beta, A, B )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(8)                                                           ::    CDFLeft
    real(8)                                                           ::    CDFRight
    real(8)                                                           ::    k
    real(8)                                                           ::    theta
    real(8)                                                           ::    VarR0D
    real(8)                                                           ::    ALoc
    real(8)                                                           ::    BLoc
    logical                                                           ::    TripFlag

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D( This, X )

    real(rkp)                                                         ::    CDF_R0D

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF_R0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
      CDF_R0D = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B )
    else if ( This%DoubleTruncatedLeft ) then
      CDF_R0D = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A )
    else if ( This%TruncatedRight ) then
      CDF_R0D = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta, B=This%B )
    else
      CDF_R0D = This%ComputeCDF( X=X, Alpha=This%Alpha, Beta=This%Beta )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputeCDF( X, Alpha, Beta, A, B )

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(8)                                                           ::    CDFLeft
    real(8)                                                           ::    CDFRight
    real(8)                                                           ::    k
    real(8)                                                           ::    theta
    real(8)                                                           ::    VarR0D
    logical                                                           ::    TripFlag

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D( This, P )

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistGamma_Type), intent(in)                                 ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF_R0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%DoubleTruncatedLeft ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B )
    else if ( This%DoubleTruncatedLeft ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, A=This%A )
    else if ( This%TruncatedRight ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta, B=This%B )
    else
      InvCDF_R0D = This%ComputeInvCDF( P=P, Alpha=This%Alpha, Beta=This%Beta )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputeInvCDF( P, Alpha, Beta, A, B )

    use CDF_Library
    use StringRoutines_Module

    real(rkp)                                                         ::    ComputeInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAlpha( This )

    real(rkp)                                                         ::    GetAlpha

    class(DistGamma_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetAlpha'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetAlpha = This%Alpha

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBeta( This )

    real(rkp)                                                         ::    GetBeta

    class(DistGamma_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetBeta'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetBeta = This%Beta

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment( This, Moment )

    real(rkp)                                                         ::    GetMoment

    class(DistGamma_Type), intent(in)                                 ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment > 0 ) then
      if ( .not. ( This%DoubleTruncatedLeft .or. This%TruncatedRight ) ) then
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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistGamma_Type), intent(out)                                ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

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
          LHS%DoubleTruncatedLeft = RHS%DoubleTruncatedLeft
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistGamma_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
