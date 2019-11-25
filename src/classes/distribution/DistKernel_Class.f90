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

module DistKernel_Class

use Prob_Library
use Input_Library
use Parameters_Library
use Brent_Library
use ComputingRoutines_Module
use StringRoutines_Module
use StatisticsRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use DistProb_Class
use DistNorm_Class                                                ,only:    DistNorm_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use KernelDist_Factory_Class                                      ,only:    KernelDist_Factory

implicit none

private

public                                                                ::    DistKernel_Type

type, extends(DistProb_Type)                                          ::    DistKernel_Type
  class(DistProb_Type), allocatable                                   ::    Kernel
  real(rkp), allocatable, dimension(:)                                ::    TransformedSamples
  real(rkp), allocatable, dimension(:)                                ::    XCDFSamples
  real(rkp), allocatable, dimension(:)                                ::    CDFSamples
  integer                                                             ::    NbCDFSamples
  real(rkp)                                                           ::    Bandwidth
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
  procedure, nopass, private                                          ::    ComputeRoTBandwidth
  generic, private                                                    ::    Transform               =>    Transform_0D,           &
                                                                                                          Transform_1D
  procedure, private                                                  ::    Transform_0D
  procedure, private                                                  ::    Transform_1D
  generic, private                                                    ::    InvTransform            =>    InvTransform_0D,        &
                                                                                                          InvTransform_1D
  procedure, private                                                  ::    InvTransform_0D
  procedure, private                                                  ::    InvTransform_1D
  generic, private                                                    ::    fInvTransform           =>    fInvTransform_0D
  procedure, private                                                  ::    fInvTransform_0D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistKernel_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'kernel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistKernel_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%TransformedSamples) ) deallocate(This%TransformedSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TransformedSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XCDFSamples) ) deallocate(This%XCDFSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XCDFSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CDFSamples) ) deallocate(This%CDFSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CDFSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Kernel) ) deallocate(This%Kernel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Kernel', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(DistKernel_Type), intent(inout)                             ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = One
    This%B = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.
    This%Bandwidth = Zero
    This%NbCDFSamples = 101

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistKernel_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    type(DistNorm_Type)                                               ::    DistNorm
    real(rkp)                                                         ::    SampleMin
    real(rkp)                                                         ::    SampleMax
    real(rkp), allocatable, dimension(:)                              ::    Samples
    integer                                                           ::    NbSamples

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

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

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    ParameterName = 'kernel'
    call Input%GetValue( VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      call KernelDist_Factory%Construct( Object=This%Kernel, DesiredType=VarC0D )
    else
      call DistNorm%Construct( Mu=Zero, Sigma=One )
      allocate(This%Kernel, source=DistNorm, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Kernel', ProcName=ProcName, stat=StatLoc )
    end if

    SectionName = 'samples'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=Samples, Prefix=PrefixLoc )
    nullify( InputSection )
    
    NbSamples = size(Samples)

    call DLASRT( 'I', NbSamples, Samples, StatLoc )
    if ( StatLoc /= 0 ) call Error%Raise( 'Something went wrong in DLASRT', ProcName=ProcName )

    SampleMin = Samples(1)
    SampleMax = Samples(NbSamples)

    if ( This%TruncatedLeft .and. SampleMin < This%A ) call Error%Raise( "Sample data below minimum", ProcName=ProcName )
    if ( This%TruncatedRight .and. SampleMax > This%B ) call Error%Raise( "Sample data above maximum", ProcName=ProcName)

    call This%Transform( Values=Samples )
    call move_alloc( Samples, This%TransformedSamples )

    ParameterName = 'bandwidth'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Bandwidth = VarR0D
    else
      This%Bandwidth = ComputeRoTBandwidth( Samples=This%TransformedSamples )
    end if

    if ( This%Bandwidth <= Zero ) call Error%Raise( Line='Bandwidth specified to be at or below zero', ProcName=ProcName )

    allocate(This%XCDFSamples(This%NbCDFSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%XCDFSamples', ProcName=ProcName, stat=StatLoc )
    allocate(This%CDFSamples(This%NbCDFSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CDFSamples', ProcName=ProcName, stat=StatLoc )
    This%CDFSamples = Zero

    This%XCDFSamples = LinSpace( InterMin=SampleMin, InterMax=SampleMax, NbNodes=This%NbCDFSamples )
    call This%Transform( Values=This%XCDFSamples )

    i = 1
    do i = 1, This%NbCDFSamples
      This%CDFSamples(i) = This%ComputeCDF( X=This%XCDFSamples(i), Samples=This%TransformedSamples, Bandwidth=This%Bandwidth,     &
                                                                                                              Kernel=This%Kernel )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Samples, Kernel, Bandwidth, A, B, Debug )

    class(DistKernel_Type), intent(inout)                             ::    This
    real(rkp), dimension(:), intent(in)                               ::    Samples
    class(DistProb_Type), optional, intent(in)                        ::    Kernel
    real(rkp), optional, intent(in)                                   ::    Bandwidth
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    type(DistNorm_Type)                                               ::    DistNorm
    real(rkp)                                                         ::    SampleMin
    real(rkp)                                                         ::    SampleMax
    real(rkp), allocatable, dimension(:)                              ::    SamplesLoc
    integer                                                           ::    NbSamples

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(A) ) then
      This%A = A
      This%TruncatedLeft = .true.
    end if

    if ( present(B) ) then
      This%B = B
      This%TruncatedRight = .true.
    end if

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )
    end if

    if ( present(Kernel) ) then
      allocate(This%Kernel, source=Kernel, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Kernel', ProcName=ProcName, stat=StatLoc )
    else
      call DistNorm%Construct( Mu=Zero, Sigma=One )
      allocate(This%Kernel, source=DistNorm, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Kernel', ProcName=ProcName, stat=StatLoc )
    end if

    allocate(SamplesLoc, source=Samples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SamplesLoc', ProcName=ProcName, stat=StatLoc )
    
    NbSamples = size(SamplesLoc)

    call DLASRT( 'I', NbSamples, SamplesLoc, StatLoc )
    if ( StatLoc /= 0 ) call Error%Raise( 'Something went wrong in DLASRT', ProcName=ProcName )

    SampleMin = SamplesLoc(1)
    SampleMax = SamplesLoc(NbSamples)

    if ( This%TruncatedLeft .and. SampleMin < This%A ) call Error%Raise( "Sample data below minimum", ProcName=ProcName )
    if ( This%TruncatedRight .and. SampleMax > This%B ) call Error%Raise( "Sample data above maximum", ProcName=ProcName)

    call This%Transform( Values=SamplesLoc )
    call move_alloc( SamplesLoc, This%TransformedSamples )

    if ( present(Bandwidth) ) then
      This%Bandwidth = Bandwidth
    else
      This%Bandwidth = ComputeRoTBandwidth( Samples=This%TransformedSamples )
    end if

    if ( This%Bandwidth <= Zero ) call Error%Raise( Line='Bandwidth specified to be at or below zero', ProcName=ProcName )

    allocate(This%XCDFSamples(This%NbCDFSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%XCDFSamples', ProcName=ProcName, stat=StatLoc )
    allocate(This%CDFSamples(This%NbCDFSamples), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CDFSamples', ProcName=ProcName, stat=StatLoc )
    This%CDFSamples = Zero

    This%XCDFSamples = LinSpace( InterMin=SampleMin, InterMax=SampleMax, NbNodes=This%NbCDFSamples )
    call This%Transform( Values=This%XCDFSamples )

    i = 1
    do i = 1, This%NbCDFSamples
      This%CDFSamples(i) = This%ComputeCDF( X=This%XCDFSamples(i), Samples=This%TransformedSamples, Bandwidth=This%Bandwidth,     &
                                                                                                              Kernel=This%Kernel )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(DistKernel_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File

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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    call GetInput%AddParameter( Name='kernel', Value=KernelDist_Factory%GetOption( Object=This%Kernel ) )
    call GetInput%AddParameter( Name='bandwidth', Value=ConvertToString(Value=This%Bandwidth) )
    if ( This%TruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )

    allocate(VarR1D, source=This%TransformedSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    call This%InvTransform( Values=VarR1D )

    if ( ExternalFlag ) then
        SectionName = 'samples'
        call GetInput%AddSection( SectionName=SectionName )
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/samples.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=VarR1D, File=File )
        nullify(InputSection)
    else
        SectionName = 'samples'
        call GetInput%AddSection( SectionName=SectionName )
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call ExportArray( Input=InputSection, Array=VarR1D )
        nullify(InputSection)
    end if

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    XLoc = X
    call This%Transform( Value=XLoc )

    PDF_R0D = This%ComputePDF( X=XLoc, Samples=This%TransformedSamples, Bandwidth=This%Bandwidth, Kernel=This%Kernel )

    call This%fInvTransform( Value=PDF_R0D, X=X )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( X, Samples, Bandwidth, Kernel, Debug )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), dimension(:), intent(in)                               ::    Samples
    real(rkp), intent(in)                                             ::    Bandwidth
    class(DistProb_Type), intent(in)                                  ::    Kernel
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'
    integer                                                           ::    NbSamples
    integer                                                           ::    i
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( Bandwidth <= Zero ) call Error%Raise( "Specified bandwidth at or below zero", ProcName=ProcName )

    NbSamples = size(Samples)

    ComputePDF = Zero

    i = 1
    do i = 1, NbSamples
      XLoc = (X - Samples(i))/Bandwidth
      ComputePDF = ComputePDF + Kernel%PDF( X=XLoc )
    end do
    ComputePDF = ComputePDF * One/(real(NbSamples,rkp)*Bandwidth)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    CDF_R0D

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF_R0D'
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    XLoc = X
    call This%Transform( Value=XLoc )

    CDF_R0D = This%ComputeCDF( X=XLoc, Samples=This%TransformedSamples, Bandwidth=This%Bandwidth, Kernel=This%Kernel )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF( X, Samples, Bandwidth, Kernel, Debug )

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), dimension(:), intent(in)                               ::    Samples
    real(rkp), intent(in)                                             ::    Bandwidth
    class(DistProb_Type), intent(in)                                  ::    Kernel
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCDF'
    integer                                                           ::    NbSamples
    integer                                                           ::    i
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( Bandwidth <= Zero ) call Error%Raise( "Specified bandwidth at or below zero", ProcName=ProcName )

    NbSamples = size(Samples)

    ComputeCDF = Zero

    i = 1
    do i = 1, NbSamples
      XLoc = (X - Samples(i))/Bandwidth
      ComputeCDF = ComputeCDF + Kernel%CDF( X=XLoc )
    end do
    ComputeCDF = ComputeCDF * One/(real(NbSamples,rkp))

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D( This, P, Debug )

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF_R0D'
    real(rkp)                                                         ::    LeftBound
    real(rkp)                                                         ::    RightBound
    integer                                                           ::    i
    integer                                                           ::    istop
    real(rkp)                                                         ::    dx
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    XLoc
    real(rkp)                                                         ::    MachEp
    real(rkp)                                                         ::    Tol
    procedure(Fun), pointer                                           ::    PMinFun=>null()
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    InvCDF_R0D = Zero

    if ( P == Zero ) then
      if ( This%TruncatedLeft ) then
        InvCDF_R0D = This%A
      else
        InvCDF_R0D = -huge(One)
      end if
    elseif ( P == One ) then
      if ( This%TruncatedRight ) then
        InvCDF_R0D = This%B
      else
        InvCDF_R0D = huge(One)
      end if
    else
      ! finds a suitable range for Brent root finder
      if ( P < This%CDFSamples(1) ) then
        RightBound = This%XCDFSamples(1)
        dx = This%XCDFSamples(2)-This%XCDFSamples(1)
        LeftBound = RightBound
        do
          VarR0D = LeftBound - dx
          if ( VarR0D < -huge(One) ) then
            dx = dx / Two
            cycle
          end if
          if ( This%ComputeCDF( X=VarR0D, Samples=This%TransformedSamples, Bandwidth=This%Bandwidth, Kernel=This%Kernel ) < P )   &
                                                                                                                              exit
          LeftBound = VarR0D
        end do
        RightBound = LeftBound
        LeftBound = VarR0D
        
      elseif ( P > This%CDFSamples(This%NbCDFSamples) ) then
        LeftBound = This%XCDFSamples(This%NbCDFSamples)
        dx = This%XCDFSamples(This%NbCDFSamples)-This%XCDFSamples(This%NbCDFSamples-1)
        RightBound = LeftBound
        do
          VarR0D = RightBound + dx
          if ( VarR0D > huge(One) ) then
            dx = dx / Two
            cycle
          end if
          if ( This%ComputeCDF( X=VarR0D, Samples=This%TransformedSamples, Bandwidth=This%Bandwidth, Kernel=This%Kernel ) > P )   &
                                                                                                                              exit
        end do
        LeftBound = RightBound
        RightBound = VarR0D
      else
        if ( any( This%CDFSamples == P ) ) then
          istop = 0
          i = 1
          do i = 1, This%NbCDFSamples
            if ( This%CDFSamples(i) == P ) then
              istop = i
              exit
            end if
          end do
          if ( istop == 0 ) call Error%Raise( 'Something went wrong', ProcName=ProcName )
          InvCDF_R0D = This%CDFSamples(istop)
        else
          istop = 0
          i = 2
          do i = 2, This%NbCDFSamples
            if ( This%CDFSamples(i) > P .or. i == This%NbCDFSamples) then
              istop = i
              exit
            end if
          end do
          if ( istop == 0 ) call Error%Raise( 'Something went wrong', ProcName=ProcName )
          LeftBound = This%XCDFSamples(istop-1)
          RightBound = This%XCDFSamples(istop)
          MachEp = epsilon(MachEp)
          Tol = 1.d-8
          PMinFun => MinFun
          InvCDF_R0D = real(Brent_Zero( LeftBound, RightBound, MachEp, Tol, PMinFun ),rkp)
        end if
        call This%InvTransform( Value=InvCDF_R0D )
      end if
    end if
      
    if (DebugLoc) call Logger%Exiting()

    contains

      !!--------------------------------------------------------------------------------------------------------------------------
      function MinFun( X )  

        real(8)                                                           ::    MinFun

        real(8), intent(in)                                               ::    X

        MinFun = This%ComputeCDF( X=X, Samples=This%TransformedSamples, Bandwidth=This%Bandwidth, Kernel=This%Kernel ) - P

      end function
      !!--------------------------------------------------------------------------------------------------------------------------

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeRoTBandwidth( Samples, Debug )

    real(rkp)                                                         ::    ComputeRoTBandwidth

    real(rkp), dimension(:), intent(in)                               ::    Samples
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeRoTBandwidth'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbSamples
    real(rkp)                                                         ::    Median
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbSamples = size(Samples)

    if ( mod(NbSamples,2) == 0 ) then
      Median = (Samples(NbSamples/2) + Samples(NbSamples/2+1))/Two
    else
      Median = Samples(NbSamples/2+1)
    end if

    VarR1D = dabs(Samples-Median)

    call DLASRT( 'I', NbSamples, VarR1D, StatLoc )
    if ( StatLoc /= 0 ) call Error%Raise( 'Something went wrong in DLASRT', ProcName=ProcName )

    if ( mod(NbSamples,2) == 0 ) then
      Median = (VarR1D(NbSamples/2) + VarR1D(NbSamples/2+1))/Two
    else
      Median = VarR1D(NbSamples/2+1)
    end if

    ComputeRoTBandwidth = (Four/(Three*NbSamples))**(One/Five)*(Median/0.6745_rkp)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_0D( This, Value, Debug )

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(inout)                                          ::    Value
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Transform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Value = dlog((Value-This%A)/(This%B-Value))
    elseif ( This%TruncatedLeft ) then
      Value = dlog(Value-This%A)
    elseif ( This%TruncatedRight ) then
      Value = dlog(One/(This%B-Value))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_1D( This, Values, Debug )

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Transform_1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Values = dlog((Values-This%A)/(This%B-Values))
    elseif ( This%TruncatedLeft ) then
      Values = dlog(Values-This%A)
    elseif ( This%TruncatedRight ) then
      Values = dlog(One/(This%B-Values))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_0D( This, Value, Debug )

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(inout)                                          ::    Value
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvTransform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Value = (This%B*dexp(Value)+This%A) / ( One+dexp(Value) )
    elseif ( This%TruncatedLeft ) then
      Value = dexp(Value)+This%A
    elseif ( This%TruncatedRight ) then
      Value = This%B - One/dexp(Value)
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_1D( This, Values, Debug )

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvTransform_1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Values = (This%B*dexp(Values)+This%A) / ( One+dexp(Values) )
    elseif ( This%TruncatedLeft ) then
      Values = dexp(Values)+This%A
    elseif ( This%TruncatedRight ) then
      Values = This%B - One/dexp(Values)
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine fInvTransform_0D( This, Value, X, Debug )

    class(DistKernel_Type), intent(in)                                ::    This
    real(rkp), intent(inout)                                          ::    Value
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='fInvTransform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%TruncatedLeft .and. This%TruncatedRight ) then
      Value = Value * dabs((This%B-This%A)/((X-This%A)*(This%B-X)))
    elseif ( This%TruncatedLeft ) then
      Value = Value * dabs(One / (X-This%A))
    elseif ( This%TruncatedRight ) then
      Value = Value * dabs(One / (This%B-X))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistKernel_Type), intent(out)                               ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistKernel_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
          LHS%Bandwidth = RHS%Bandwidth
          LHS%NbCDFSamples = RHS%NbCDFSamples
          allocate(LHS%TransformedSamples, source=RHS%TransformedSamples, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TransformedSamples', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%XCDFSamples, source=RHS%XCDFSamples, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%XCDFSamples', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%CDFSamples, source=RHS%CDFSamples, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CDFSamples', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Kernel, source=RHS%Kernel, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Kernel', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistKernel_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%TransformedSamples) ) deallocate(This%TransformedSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TransformedSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XCDFSamples) ) deallocate(This%XCDFSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XCDFSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CDFSamples) ) deallocate(This%CDFSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CDFSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Kernel) ) deallocate(This%Kernel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Kernel', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
