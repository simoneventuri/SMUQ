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

module DistLogistic_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use StringRoutines_Module
use QuadPack_Library
use DistProb_Class
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use SampleLHS_Class                                               ,only:    SampleLHS_Type

implicit none

private

public                                                                ::    DistLogistic_Type

type, extends(DistProb_Type)                                          ::    DistLogistic_Type
  real(rkp)                                                           ::    Mu=Zero
  real(rkp)                                                           ::    S=One
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, public                                                   ::    ComputePDF
  procedure, public                                                   ::    CDF_R0D
  procedure, public                                                   ::    ComputeCDF
  procedure, public                                                   ::    InvCDF_R0D
  procedure, public                                                   ::    ComputeInvCDF
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetS
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(DistLogistic_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(DistLogistic_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(DistLogistic_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = Zero
    This%B = Zero
    This%Mu = Zero
    This%S = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(DistLogistic_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
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

    ParameterName = 'mu'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Mu = VarR0D

    ParameterName = 's'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.true. )
    This%S = VarR0D

    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%A = VarR0D
      This%TruncatedLeft = .true.
    end if

    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%B = VarR0D
      This%TruncatedRight = .true.
    end if

    if ( This%S < Zero ) call Error%Raise( Line='Standard deviation specified to be below minimum of 0', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Mu, S, A, B )
    
    class(DistLogistic_Type), intent(inout)                           ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B 

    character(*), parameter                                           ::    ProcName='ConstructCase1'


    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%Mu = Mu

    This%S = S

    if ( present(A) ) then
      This%A = A
      This%TruncatedLeft = .true.
    end if

    if ( present(B) ) then
      This%B = B
      This%TruncatedRight = .true.
    end if

    if ( This%S < Zero ) call Error%Raise( Line='Standard deviation specified to be below minimum of 0', ProcName=ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistLogistic_Type), intent(in)                              ::    This
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
    call GetInput%AddParameter( Name='mu', Value=ConvertToString( Value=This%Mu ) )
    call GetInput%AddParameter( Name='s', Value=ConvertToString( Value=This%S ) )
    if ( This%TruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X )

    real(rkp)                                                         ::    PDF_R0D

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF_R0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft  ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight  ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, B=This%B )
    else
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( This, X, Mu, S, A, B )

    real(rkp)                                                         ::    ComputePDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    logical                                                           ::    TripFlag

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
      if ( present(A) ) CDFLeft = This%ComputeCDF( X=A, Mu=Mu, S=S )
      CDFRight = One
      if ( present(B) ) CDFRight = This%ComputeCDF( X=B, Mu=Mu, S=S )
      ComputePDF = dexp(-(X-Mu)/S) / (S*( One + dexp(-(X-Mu)/S) )**2)
      if ( present(A) .or. present(B) ) ComputePDF = ComputePDF / ( CDFRight - CDFLeft )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D( This, X )

    real(rkp)                                                         ::    CDF_R0D

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF_R0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      CDF_R0D = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft ) then
      CDF_R0D = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight ) then
      CDF_R0D = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, B=This%B )
    else
      CDF_R0D = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF( This, X, Mu, S, A, B )

    real(rkp)                                                         ::    ComputeCDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    logical                                                           ::    TripFlag

    TripFlag = .false.

    if ( present(A) ) then
      if (X < A) then
        ComputeCDF = Zero
        TripFlag=.true.
      end if
    end if

    if ( present(B) ) then
      if (X > B) then
        ComputeCDF = One
        TripFlag=.true.
      end if
    end if

    if ( .not. TripFlag ) then
      CDFLeft = 0.
      if ( present(A) ) CDFLeft = This%ComputeCDF( X=A, Mu=Mu, S=S )
      CDFRight = 1.
      if ( present(B) ) CDFRight = This%ComputeCDF( X=B, Mu=Mu, S=S )
      ComputeCDF = One / ( One + dexp(-(X-Mu)/S) )
      ComputeCDF = ( ComputeCDF - CDFLeft ) / ( CDFRight - CDFLeft )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D( This, P )

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF_R0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight ) then
      InvCDF_R0D = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, B=This%B )
    else
      InvCDF_R0D = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S )
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeInvCDF( This, P, Mu, S, A, B )

    real(rkp)                                                         ::    ComputeInvCDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    PLoc
    logical                                                           ::    TripFlag

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    TripFlag = .false.

    if ( P == Zero ) then
      if ( present(A) ) then
        ComputeInvCDF = A
      else
        ComputeInvCDF = -huge(One)
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
      CDFLeft = 0.
      if ( present(A) ) CDFLeft = This%ComputeCDF ( X=A, Mu=Mu, S=S )
      CDFRight = 1.
      if ( present(B) ) CDFRight = This%ComputeCDF ( X=B, Mu=Mu, S=S )

      PLoc = CDFLeft+P*(CDFRight-CDFLeft)

      ComputeInvCDF = Mu + S * dlog(PLoc/(1-PLoc))
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMu( This )

    real(rkp)                                                         ::    GetMu

    class(DistLogistic_Type), intent(in)                              ::    This

    character(*), parameter                                           ::    ProcName='GetMu'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetMu = This%Mu

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetS( This )

    real(rkp)                                                         ::    GetS

    class(DistLogistic_Type), intent(in)                              ::    This

    character(*), parameter                                           ::    ProcName='GetS'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetS = This%S

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment( This, Moment )

    real(rkp)                                                         ::    GetMoment

    class(DistLogistic_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    BNumbers
    integer                                                           ::    i
    real(rkp)                                                         ::    ZMoment


    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Moment < 0 ) call Error%Raise( "Requested a distribution moment below 0", ProcName=ProcName )

    if ( Moment > 0 ) then
      if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) call Error%Raise( "DistLogistic module currently cant compute" // &
                                                  " moments where lower bound is infinite while upper is not", ProcName=ProcName )
      if ( .not. ( THis%TruncatedLeft .or. This%TruncatedRight ) ) then
        BNumbers = BernoulliNumbers( P=Moment )
        GetMoment = Zero
        i = 0
        do i = 0, Moment
          if ( i == 0 ) then
            ZMoment = One
          elseif ( mod(i,2) /= 0 ) then
            ZMoment = Zero
          else
            ZMoment = (Two**i-Two)*Pi**i*abs(BNumbers(i+1))
          end if
          GetMoment = GetMoment + real(BinomialCoeff(Top=Moment, Bottom=i),rkp) * This%Mu**(Moment-i) * This%S**i*ZMoment
        end do
        deallocate(BNumbers, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='BNumbers', ProcName=ProcName, stat=StatLoc )
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

    class(DistLogistic_Type), intent(out)                             ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (DistLogistic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Mu = RHS%Mu
          LHS%S = RHS%S
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistLogistic_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
