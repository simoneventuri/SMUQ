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
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
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
  logical                                                             ::    DebugFlag
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Evaluate_0D
  procedure, private                                                  ::    Evaluate_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'LikelihoodGauss'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%CovarianceConstructor) ) deallocate(This%CovarianceConstructor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XmMean) ) deallocate(This%XmMean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MultiplicativeError = .false.
    This%Scalar = Zero
    This%Label = ''
    This%DebugFlag = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'debug'
    call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%DebugFlag = VarL0D

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

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='debug', Value=ConvertToString(Value=This%DebugFlag) )
    call GetInput%AddParameter( Name='multiplicative_error', Value=ConvertToString(Value=This%MultiplicativeError) )
    call GetInput%AddParameter( Name='scalar', Value=ConvertToString(Value=This%Scalar) )
    call GetInput%AddParameter( Name='label', Value=This%Label )
  
    SectionName = 'covariance'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/covariance'
    call GetInput%AddSection( Section=CovarianceConstructor_Factory%GetObjectInput( Object=This%CovarianceConstructor,            &
                              MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_1D( This, Responses, Input, Output, LogValue )

    real(rkp)                                                         ::    Evaluate_1D

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional, intent(in)                                     ::    LogValue

    character(*), parameter                                           ::    ProcName='Evaluate_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    iOutput
    integer                                                           ::    iResponse
    integer                                                           ::    NbOutputs
    integer                                                           ::    NbResponses
    integer                                                           ::    i

    iOutput = 0
    iResponse = 0

    NbOutputs = size(Output,1)
    NbResponses = size(Responses,1)

    i = 1
    do i = 1, NbOutputs
      if ( Output(i)%GetLabel() == This%Label ) then
        iOutput = i
        exit
      end if
    end do

    if ( iOutput == 0 ) call Error%Raise( Line='Did not find required output : ' // This%Label, ProcName=ProcName )

    i = 1
    do i = 1, NbResponses
      if ( Responses(i)%GetLabel() == This%Label ) then
        iResponse = i
        exit
      end if
    end do

    if ( iResponse == 0 ) call Error%Raise( Line='Did not find required response : ' // This%Label, ProcName=ProcName )

    if ( present(LogValue) ) then
      Evaluate_1D = This%Evaluate( Response=Responses(iResponse), Input=Input, Output=Output(iOutput), Logvalue=LogValue )
    else
      Evaluate_1D = This%Evaluate( Response=Responses(iResponse), Input=Input, Output=Output(iOutput) )
    end if

    if ( This%DebugFlag ) then
      write(*,*)
      write(*,*) 'Likelihood value : ' // ConvertToString(Value=Evaluate_1D)
      write(*,*)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_0D( This, Response, Input, Output, LogValue )

    real(rkp)                                                         ::    Evaluate_0D

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(Response_Type), intent(in)                                   ::    Response
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), intent(in)                                     ::    Output
    logical, optional, intent(in)                                     ::    LogValue

    character(*), parameter                                           ::    ProcName='Evaluate_0D'
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
    logical                                                           ::    ZeroExit

    if ( Response%GetLabel() /= This%Label ) call Error%Raise( 'Passed incorrect response', ProcName=ProcName )
    if ( Output%GetLabel() /= This%Label ) call Error%Raise( 'Passed incorrect output', ProcName=ProcName )

    if ( This%DebugFlag ) then
      write(*,*) '*****************************************************************************'
      write(*,*) 'Debug information for likelihood function of response ' // Response%GetLabel()
      write(*,*) '*****************************************************************************'
    end if

    LogValueLoc = .false.
    if ( present(LogValue) ) LogValueLoc = LogValue

    ln2pi = dlog(Two*pi)

    Evaluate_0D = Zero

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

    ZeroExit = .false.
    if ( .not. IsDiagonalFlag ) then
      call DPOTRF( 'L', NbNodes, This%L, NbNodes, StatLoc )
      ZeroExit = .true.
!      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DPOTRF', ProcName=ProcName )
    else
      This%L = dsqrt(This%L)
    end if

    if ( This%DebugFlag ) then
      write(*,*)
      write(*,*) 'Is covariance not innvertible? : ' // ConvertToString(Value=ZeroExit)
      write(*,*)
    end if

    if ( ZeroExit ) then
      if ( LogValueLoc ) then
        Evaluate_0D = -huge(One)
      else
        Evaluate_0D = Zero
      end if
    else
      VarR0D = Zero
      ii = 1
      do ii = 1, NbNodes
        VarR0D = VarR0D + dlog(This%L(ii,ii))
      end do
      lnPreExp = (-real(NbNodes,rkp)/Two*ln2pi - VarR0D)*NbDegen*NbDataSets + lnPreExp

      VarR0D = Zero
      ii = 1
      do ii = 1, NbDataSets
        if ( This%DebugFlag ) then
          write(*,*)
          write(*,*) 'Data set # ' // ConvertToString(Value=ii)
          write(*,*)
          write(*,*) DataPtr(:,ii)
        end if
        iii = 1
        do iii = 1, NbDegen
          if ( This%DebugFlag ) then
            write(*,*)
            write(*,*) 'Output set # ' // ConvertToString(Value=ii)
            write(*,*)
            write(*,*) OutputPtr(:,iii)
          end if
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

      Evaluate_0D = lnPreExp + lnExp + This%Scalar

      if ( This%DebugFlag ) then
        write(*,*)
        write(*,*) 'Log likelihood value : ' // ConvertToString(Value=Evaluate_0D)
        write(*,*)
      end if

      if ( .not. LogValueLoc ) then
        if ( Evaluate_0D > TVarR0D .and. Evaluate_0D < HVarR0D ) then
          Evaluate_0D = dexp(Evaluate_0D)
        elseif (Evaluate_0D < TVarR0D ) then
          Evaluate_0D = Zero
        else
          call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                          &
               ConvertToString(Value=Evaluate_0D) // '. Consider changing value of the scalar modifier and rerun for response: '//&
                                                                                                   This%Label, ProcName=ProcName )
        end if
      end if

      nullify(OutputPtr)
      nullify(DataPtr)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(LikelihoodGauss_Type), intent(out)                          ::    LHS
    class(LikelihoodFunction_Type), intent(in)                        ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (LikelihoodGauss_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%DebugFlag = RHS%DebugFlag
          LHS%Scalar = RHS%Scalar
          LHS%MultiplicativeError = RHS%MultiplicativeError
          LHS%Label = RHS%Label
          allocate(LHS%CovarianceConstructor, source=RHS%CovarianceConstructor, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(LikelihoodGauss_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0
  
    if ( allocated(This%CovarianceConstructor) ) deallocate(This%CovarianceConstructor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CovarianceConstructor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%XmMean) ) deallocate(This%XmMean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%XmMean', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
