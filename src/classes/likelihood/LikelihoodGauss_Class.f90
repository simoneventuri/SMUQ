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

module LikelihoodGauss_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use Response_Class                                                ,only:    Response_Type
use Output_Class                                                  ,only:    Output_Type
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type
use CovarianceConstructor_Class                                   ,only:    CovarianceConstructor_Type
use CovarianceConstructor_Factory_Class                           ,only:    CovarianceConstructor_Factory
use List2DAllocReal_Class                                         ,only:    List2DAllocReal_Type

implicit none

private

public                                                                ::    LikelihoodGauss_Type

type, extends(LikelihoodFunction_Type)                                ::    LikelihoodGauss_Type
  logical                                                             ::    MultiplicativeError
  real(rkp)                                                           ::    Scalar=Zero
  class(CovarianceConstructor_Type), allocatable                      ::    CovarianceConstructor
  real(rkp), allocatable, dimension(:,:)                              ::    L
  real(rkp), allocatable, dimension(:,:)                              ::    XmMean
  logical                                                             ::    StochCovFlag
  real(rkp)                                                           ::    lnPreExp
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    EvaluateDet
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LikelihoodGauss'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%CovarianceConstructor) ) deallocate(This%CovarianceConstructor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XmMean) ) deallocate(This%XmMean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%MultiplicativeError = .false.
    This%Scalar = Zero
    This%Label = ''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'multiplicative_error'
    call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%MultiplicativeError = VarL0D
    
    ParameterName = 'scalar'
    call Input%GetValue( Value=VarR0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%Scalar = VarR0D

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    This%Label = VarC0D

    SectionName = 'covariance'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call CovarianceConstructor_Factory%Construct( Object=This%CovarianceConstructor, Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='multiplicative_error', Value=ConvertToString(Value=This%MultiplicativeError) )
    call GetInput%AddParameter( Name='scalar', Value=ConvertToString(Value=This%Scalar) )
    call GetInput%AddParameter( Name='label', Value=This%Label )
  
    SectionName = 'covariance'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/covariance'
    call GetInput%AddSection( Section=CovarianceConstructor_Factory%GetObjectInput( Object=This%CovarianceConstructor,            &
                              MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EvaluateDet( This, Response, Input, Output, LogValue, Debug )

    real(rkp)                                                         ::    EvaluateDet

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional, intent(in)                                     ::    LogValue
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='EvaluateDet'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDataSets
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    real(rkp), pointer, dimension(:,:)                                ::    OutputPtr=>null()
    real(rkp), pointer, dimension(:,:)                                ::    DataPtr=>null()
    logical                                                           ::    IsDiagonalFlag
    logical                                                           ::    Found
    integer                                                           ::    NbDegen
    real(rkp)                                                         ::    lnPreExp
    real(rkp)                                                         ::    lnExp
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D
    logical                                                           ::    ExitFlag
    integer                                                           ::    NbNodes
    real(rkp)                                                         ::    ln2pi
    logical                                                           ::    LogValueLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    LogValueLoc = .false.
    if ( present(LogValue) ) LogValueLoc = LogValue

    ln2pi = dlog(Two*pi)

    NbOutputs = size(Output)
    NbResponses = size(Response)

    EvaluateDet = Zero

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    ExitFlag = .false.

    NbNodes = 0
  
    ! computing ln(likelihood) values first before transforming them back to linear scale
    lnPreExp = Zero
    lnExp = Zero

    if ( .not. Response%IsDataDefined() ) call Error%Raise( Line='Data not defined for the response', ProcName=ProcName )

    NbNodes = Response%GetNbNodes()

    if ( allocated(This%L) ) then
      if ( size(This%L,1) /= NbNodes ) then
        deallocate(This%L, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )
        deallocate(This%XmMean, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if ( .not. allocated(This%L) ) then
      allocate(This%L(NbNodes,NbNodes), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%L', ProcName=ProcName, stat=StatLoc )
      This%L = Zero
      allocate(This%XmMean(NbNodes,1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )
      This%XmMean = Zero
    end if

    OutputPtr => Output%GetValuesPointer()
    DataPtr => Response%GetDataPointer()
    NbDegen = size(OutputPtr,2)
    NbDataSets = size(DataPtr,2)

    call This%CovarianceConstructor%AssembleCov( Input=Input, Coordinates=Response%GetCoordinatesPointer(),                       &
                                                                     CoordinateLabels=Response%GetCoordinateLabels(), Cov=This%L )

    IsDiagonalFlag = IsDiagonal( Array=This%L )

    if ( .not. IsDiagonalFlag ) then
      call DPOTRF( 'L', NbNodes, This%L, NbNodes, StatLoc )
      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DPOTRF', ProcName=ProcName )
    else
      This%L = dsqrt(This%L)
    end if

    VarR0D = Zero
    ii = 1
    do ii = 1, NbNodes
      VarR0D = VarR0D + dlog(This%L(ii,ii))
    end do
    lnPreExp = (-real(NbNodes,rkp)/Two*ln2pi - VarR0D)*NbDegen*NbDataSets + lnPreExp

    VarR0D = Zero
    ii = 1
    do ii = 1, NbDataSets
      iii = 1
      do iii = 1, NbDegen
        if ( This%MultiplicativeError ) then
          This%XmMean(:,1) = DataPtr(:,ii) / OutputPtr(:,iii)
          This%XmMean(:,1) = dlog(This%XmMean(:,1))
        else
          This%XmMean(:,1) = DataPtr(:,ii) - OutputPtr(:,iii)
        end if
        call DTRTRS( 'L', 'N', 'N', NbNodes, 1, This%L, NbNodes, This%XmMean(:,:), NbNodes, StatLoc )
        if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DTRTRS with code: '//ConvertToString(Value=StatLoc),&
                                                                                                             ProcName=ProcName )
        VarR0D = VarR0D - 0.5 * dot_product(This%XmMean(:,1), This%XmMean(:,1))
      end do
    end do
    lnExp = VarR0D

    EvaluateDet = lnPreExp + lnExp + This%Scalar

    if ( .not. LogValueLoc ) then
      if ( EvaluateDet > TVarR0D .and. EvaluateDet < HVarR0D ) then
        EvaluateDet = dexp(EvaluateDet)
      elseif (EvaluateDet < TVarR0D ) then
        EvaluateDet = Zero
      else
        call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
             ConvertToString(Value=EvaluateDet) // '. Consider changing value of the scalar modifier and rerun for response: ' // &
                                                                                                   This%Label, ProcName=ProcName )
      end if
    end if

    nullify(OutputPtr)
    nullify(DataPtr)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LikelihoodGauss_Type), intent(out)                          ::    LHS
    class(LikelihoodFunction_Type), intent(in)                        ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (LikelihoodGauss_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Scalar = RHS%Scalar
          LHS%MultiplicativeError = RHS%MultiplicativeError
          LHS%Label = RHS%Label
          allocate(LHS%CovarianceConstructor, source=RHS%CovarianceConstructor, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(LikelihoodGauss_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%CovarianceConstructor) ) deallocate(This%CovarianceConstructor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XmMean) ) deallocate(This%XmMean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
