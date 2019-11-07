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
use DistProb_Class                                                ,only:    DistProb_Type
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
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    ComputeCDF
  procedure, public                                                   ::    InvCDF
  procedure, public                                                   ::    ComputeInvCDF
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetS
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, private                                                  ::    ComputeMoment1
  procedure, private                                                  ::    ComputeMoment2
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistLogistic_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistLogistic_Type), intent(inout)                           ::    This
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

    class(DistLogistic_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = Zero
    This%B = Zero
    This%Mu = Zero
    This%S = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistLogistic_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Mu, S, A, B, Debug )
    
    class(DistLogistic_Type), intent(inout)                           ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistLogistic_Type), intent(in)                              ::    This
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
    call GetInput%AddParameter( Name='mu', Value=ConvertToString( Value=This%Mu ) )
    call GetInput%AddParameter( Name='s', Value=ConvertToString( Value=This%S ) )
    if ( This%TruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S, B=This%B )
    else
      PDF_R0D = This%ComputePDF( X=X, Mu=This%Mu, S=This%S )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( This, X, Mu, S, A, B, Debug )

    real(rkp)                                                         ::    ComputePDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S, B=This%B )
    else
      CDF = This%ComputeCDF( X=X, Mu=This%Mu, S=This%S )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF( This, X, Mu, S, A, B, Debug )

    real(rkp)                                                         ::    ComputeCDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    logical                                                           ::    TripFlag

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .and. This%TruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, A=This%A, B=This%B )
    else if ( This%TruncatedLeft .and. .not. This%TruncatedRight ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, A=This%A )
    else if ( This%TruncatedRight .and. .not. This%TruncatedLeft ) then
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S, B=This%B )
    else
      InvCDF = This%ComputeInvCDF( P=P, Mu=This%Mu, S=This%S )
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeInvCDF( This, P, Mu, S, A, B, Debug )

    real(rkp)                                                         ::    ComputeInvCDF

    class(DistLogistic_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    PLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    CDFLeft = 0.
    if ( present(A) ) CDFLeft = This%ComputeCDF ( X=A, Mu=Mu, S=S )
    CDFRight = 1.
    if ( present(B) ) CDFRight = This%ComputeCDF ( X=B, Mu=Mu, S=S )

    PLoc = CDFLeft+P*(CDFRight-CDFLeft)

    ComputeInvCDF = Mu + S * dlog(PLoc/(1-PLoc))

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMu( This, Debug )

    real(rkp)                                                         ::    GetMu

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMu'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetMu = This%Mu

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetS( This, Debug )

    real(rkp)                                                         ::    GetS

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetS'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetS = This%S

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean( This, Debug )

    real(rkp)                                                         ::    GetMean

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMean'
    integer                                                           ::    StatLoc=0
    type(SampleLHS_Type)                                              ::    Sampler 
    real(rkp), allocatable, dimension(:)                              ::    Samples   
    integer                                                           ::    i 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .or. This%A>tiny(One) ) then
      allocate(Samples(1000), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Samples', ProcName=ProcName, stat=StatLoc )
      call Sampler%Construct( NbSamples=1000 )
      Samples = Sampler%Draw()

      i = 1
      do i = 1, size(Samples,1)
        Samples(i) = This%InvCDF(P=Samples(i))
      end do

      GetMean = ComputeMean( Values=Samples )
      deallocate(Samples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Samples', ProcName=ProcName, stat=StatLoc )
    else
      GetMean = This%Mu
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance( This, Debug )

    real(rkp)                                                         ::    GetVariance

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'
    integer                                                           ::    StatLoc=0
    type(SampleLHS_Type)                                              ::    Sampler 
    real(rkp), allocatable, dimension(:)                              ::    Samples   
    integer                                                           ::    i 

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .or. This%A>tiny(One) ) then
      allocate(Samples(1000), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Samples', ProcName=ProcName, stat=StatLoc )
      call Sampler%Construct( NbSamples=1000 )
      Samples = Sampler%Draw()
      i = 1
      do i = 1, size(Samples,1)
        Samples(i) = This%InvCDF(P=Samples(i))
      end do
      GetVariance = ComputeSampleVar( Values=Samples )
      deallocate(Samples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Samples', ProcName=ProcName, stat=StatLoc )
    else
      GetVariance = This%S**2*pi**2/Three
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment1( This, Debug )

    real(rkp)                                                         ::    ComputeMoment1

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMoment1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .or. This%A>tiny(One) ) then
      ComputeMoment1 = This%GetMean()
    else
      ComputeMoment1 = This%Mu
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment2( This, Debug )

    real(rkp)                                                         ::    ComputeMoment2

    class(DistLogistic_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%TruncatedRight .or. This%A>tiny(One) ) then
      ComputeMoment2 = This%GetVariance() + This%GetMean()**2
    else
      ComputeMoment2 = This%S**2*pi**2/Three + This%Mu**2
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistLogistic_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistLogistic_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
