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
  type(Observations_Type), allocatable, dimension(:)                  ::    Observations
  integer                                                             ::    NbObservations
  logical                                                             ::    MultiplicativeError
  real(rkp)                                                           ::    Scalar=Zero
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    EvaluateDet
  procedure, public                                                   ::    EvaluateStoch
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

type                                                                  ::    Observations_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  class(CovarianceConstructor_Type), allocatable                      ::    CovarianceConstructor
  character(:), allocatable                                           ::    Label
  real(rkp), allocatable, dimension(:,:)                              ::    L
  real(rkp), allocatable, dimension(:,:)                              ::    XmMean
  logical                                                             ::    StochCovFlag
  integer                                                             ::    iOutput
  integer                                                             ::    iResponse
  integer                                                             ::    NbDegenOutput
  real(rkp)                                                           ::    lnPreExp
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Obs
  procedure, public                                                   ::    Reset                   =>    Reset_Obs
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Obs
  generic, public                                                     ::    Construct               =>    ConstructInput_Obs
  procedure, private                                                  ::    ConstructInput_Obs
  procedure, public                                                   ::    GetInput                =>    GetInput_Obs
  procedure, public                                                   ::    Copy                    =>    Copy_Obs
  final                                                               ::    Finalizer_Obs
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

    if ( allocated(This%Observations) ) deallocate(This%Observations, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations', ProcName=ProcName, stat=StatLoc )
    This%NbObservations = 0

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

    SectionName = 'observations'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbObservations = InputSection%GetNumberOfSubSections()
    if ( This%NbObservations <= 0 ) call Error%Raise( Line='Specified 0 or below observation sets', ProcName=ProcName )
    nullify(InputSection)

    allocate(This%Observations(This%NbObservations), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Observations', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbObservations
      SubSectionName = SectionName // '>observations' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Observations(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

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

    SectionName = 'observations'
    call GetInput%AddSection(SectionName=SectionName)
    i = 1
    do i = 1, size(This%Observations)
      SubSectionName = 'observations' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/' // SubSectionName
      call GetInput%AddSection( Section=This%Observations(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,           &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EvaluateDet( This, Response, Input, Output, Debug )

    real(rkp)                                                         ::    EvaluateDet

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='EvaluateDet'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDataSets
    integer                                                           ::    NbResponses
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    real(rkp), pointer, dimension(:)                                  ::    VarR1DPtr=>null()
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
    integer                                                           ::    AbscissaSize
    real(rkp)                                                         ::    ln2pi

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ln2pi = dlog(Two*pi)

    NbOutputs = size(Output)
    NbResponses = size(Response)

    EvaluateDet = Zero

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    ExitFlag = .false.

    AbscissaSize = 0
  
    ! computing ln(likelihood) values first before transforming them back to linear scale
    lnPreExp = Zero
    lnExp = Zero

    i = 1
    do i = 1, This%NbObservations

      This%Observations(i)%iOutput = 0
      This%Observations(i)%iResponse = 0

      ii = 1
      do ii = 1, NbOutputs
        if ( Output(ii)%GetLabel() == This%Observations(i)%Label ) then
          This%Observations(i)%iOutput = ii
          exit
        end if
      end do

      if ( This%Observations(i)%iOutput == 0 ) call Error%Raise( Line='Did not find required output : ' //                        &
                                                                                   This%Observations(i)%Label, ProcName=ProcName )

      ii = 1
      do ii = 1, NbResponses
        if ( Response(ii)%GetLabel() == This%Observations(i)%Label ) then
          This%Observations(i)%iResponse = ii
          exit
        end if
      end do

      if ( This%Observations(i)%iResponse == 0 ) call Error%Raise( Line='Did not find required response : ' //                    &
                                                                                   This%Observations(i)%Label, ProcName=ProcName )

      if ( .not. Response(This%Observations(i)%iResponse)%IsDataDefined() )                                                       &
                                                   call Error%Raise( Line='Data not defined for the response', ProcName=ProcName )

      AbscissaSize = size(Response(i)%GetAbscissaPointer(),1)

      if ( allocated(This%Observations(i)%L) ) then
        if ( size(This%Observations(i)%L,1) /= AbscissaSize ) then
          deallocate(This%Observations(i)%L, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations(i)%L', ProcName=ProcName, stat=StatLoc )
          deallocate(This%Observations(i)%XmMean, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations(i)%XmMean', ProcName=ProcName, stat=StatLoc )
        end if
      end if

      if ( .not. allocated(This%Observations(i)%L) ) then
        allocate(This%Observations(i)%L(AbscissaSize,AbscissaSize), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Observations(i)%L', ProcName=ProcName, stat=StatLoc )
        This%Observations(i)%L = Zero
        allocate(This%Observations(i)%XmMean(AbscissaSize,1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Observations(i)%XmMean', ProcName=ProcName, stat=StatLoc )
        This%Observations(i)%XmMean = Zero
      end if

      OutputPtr => Output(This%Observations(i)%iOutput)%GetOrdinatePointer()
      DataPtr => Response(This%Observations(i)%iResponse)%GetDataPointer()
      NbDegen = size(OutputPtr,2)
      NbDataSets = size(DataPtr,2)

      call This%Observations(i)%CovarianceConstructor%AssembleCov( Input=Input,                                                   &
                              Abscissa=Response(This%Observations(i)%iResponse)%GetAbscissaPointer(), Cov=This%Observations(i)%L )

      IsDiagonalFlag = IsDiagonal( Array=This%Observations(i)%L )

      if ( .not. IsDiagonalFlag ) then
        call DPOTRF( 'L', AbscissaSize, This%Observations(i)%L, AbscissaSize, StatLoc )
        if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DPOTRF', ProcName=ProcName )
      else
        This%Observations(i)%L = dsqrt(This%Observations(i)%L)
      end if

      VarR0D = Zero
      ii = 1
      do ii = 1, AbscissaSize
        VarR0D = VarR0D + dlog(This%Observations(i)%L(ii,ii))
      end do
      lnPreExp = (-real(AbscissaSize,rkp)/Two*ln2pi - VarR0D)*NbDegen*NbDataSets + lnPreExp

      VarR0D = Zero
      ii = 1
      do ii = 1, NbDataSets
        iii = 1
        do iii = 1, NbDegen
          if ( This%MultiplicativeError ) then
            This%Observations(i)%XmMean(:,1) = DataPtr(:,ii) / OutputPtr(:,iii)
            This%Observations(i)%XmMean(:,1) = dlog(This%Observations(i)%XmMean(:,1))
          else
            This%Observations(i)%XmMean(:,1) = DataPtr(:,ii) - OutputPtr(:,iii)
          end if
          call DTRTRS( 'L', 'N', 'N', AbscissaSize, 1, This%Observations(i)%L, AbscissaSize, This%Observations(i)%XmMean(:,:),    &
                                                                                                           AbscissaSize, StatLoc )
          if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DTRTRS with code: '//ConvertToString(Value=StatLoc),&
                                                                                                               ProcName=ProcName )
          VarR0D = VarR0D - 0.5 * dot_product(This%Observations(i)%XmMean(:,1), This%Observations(i)%XmMean(:,1))
        end do
      end do
      lnExp = lnExp + VarR0D
    end do

    EvaluateDet = lnPreExp + lnExp + This%Scalar

    if ( EvaluateDet > TVarR0D .and. EvaluateDet < HVarR0D ) then
      EvaluateDet = dexp(EvaluateDet)
    elseif (EvaluateDet < HVarR0D ) then
      EvaluateDet = Zero
    else
      call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                             &
           ConvertToString(Value=EvaluateDet) // '. Consider changing value of the scalar modifier and rerun', ProcName=ProcName )
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EvaluateStoch( This, Response, Input, Output, Debug )

    real(rkp)                                                         ::    EvaluateStoch

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    type(InputStoch_Type), intent(in)                                 ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='EvaluateStoch'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDataSets
    integer                                                           ::    NbResponses
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    real(rkp), pointer, dimension(:,:)                                ::    OutputPtr=>null()
    real(rkp), pointer, dimension(:,:)                                ::    DataPtr=>null()
    logical                                                           ::    IsDiagonalFlag
    logical                                                           ::    Found
    integer                                                           ::    NbDegenInput
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D
    integer                                                           ::    imin
    integer                                                           ::    imax
    integer                                                           ::    AbscissaSize
    real(rkp)                                                         ::    lnExp
    real(rkp)                                                         ::    ln2pi

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ln2pi = dlog(Two*pi)

    NbResponses = size(Response,1)

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    NbDegenInput = Input%GetNbDegen()

    EvaluateStoch = Zero

    i = 1
    do i = 1, This%NbObservations
      This%Observations(i)%iOutput = 0
      This%Observations(i)%iResponse = 0
      ii = 1
      do ii = 1, NbOutputs
        if ( Output(ii)%GetLabel() == This%Observations(i)%Label ) then
          This%Observations(i)%iOutput = ii
          exit
        end if
      end do
      if ( This%Observations(i)%iOutput == 0 ) call Error%Raise( Line='Did not find required output : ' //                        &
                                                                                   This%Observations(i)%Label, ProcName=ProcName )
      ii = 1
      do ii = 1, NbResponses
        if ( Response(ii)%GetLabel() == This%Observations(i)%Label ) then
          This%Observations(i)%iResponse = ii
          exit
        end if
      end do
      if ( This%Observations(i)%iResponse == 0 ) call Error%Raise( Line='Did not find required response : ' //                    &
                                                                                   This%Observations(i)%Label, ProcName=ProcName )

      if ( .not. Response(This%Observations(i)%iResponse)%IsDataDefined() )                                                       &
                                                   call Error%Raise( Line='Data not defined for the response', ProcName=ProcName )

      This%Observations(i)%StochCovFlag = This%Observations(i)%CovarianceConstructor%IsStochastic( Input=Input )
      if ( mod(Output(This%Observations(i)%iOutput)%GetOrdinateNbDegen(),NbDegenInput) /= 0 )                                     &
                     call Error%Raise(Line= 'An unequal number of realizations per each stochastic input realization for output: '&
                                                            // Output(This%Observations(i)%iOutput)%GetName(), ProcName=ProcName )
      This%Observations(i)%NbDegenOutput = Output(This%Observations(i)%iOutput)%GetOrdinateNbDegen() / NbDegenInput

      AbscissaSize = Zero
      AbscissaSize = size(Response(This%Observations(i)%iResponse)%GetAbscissaPointer(),1)

      if ( allocated(This%Observations(i)%L) ) then
        if ( size(This%Observations(i)%L,1) /= AbscissaSize ) then
          deallocate(This%Observations(i)%L, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations(i)%L', ProcName=ProcName, stat=StatLoc )
          deallocate(This%Observations(i)%XmMean, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations(i)%XmMean', ProcName=ProcName, stat=StatLoc )
        end if
      end if

      if ( .not. allocated(This%Observations(i)%L) ) then
        allocate(This%Observations(i)%L(AbscissaSize,AbscissaSize), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Observations(i)%L', ProcName=ProcName, stat=StatLoc )
        This%Observations(i)%L = Zero
        allocate(This%Observations(i)%XmMean(AbscissaSize,1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Observations(i)%XmMean', ProcName=ProcName, stat=StatLoc )
        This%Observations(i)%XmMean = Zero
      end if
    end do

    ! computing ln(likelihood) values first before transforming them back to linear scale

    EvaluateStoch = Zero

    i = 1
    do i = 1, NbDegenInput

      lnExp = Zero

      ii = 1
      do ii = 1, This%NbObservations
        OutputPtr => Output(This%Observations(ii)%iOutput)%GetOrdinatePointer()
        DataPtr => Response(This%Observations(ii)%iResponse)%GetDataPointer()
        AbscissaSize = size(OutputPtr,1)
        NbDataSets = size(DataPtr,2)

        if ( This%Observations(ii)%StochCovFlag .or. i == 1 ) then
          call This%Observations(ii)%CovarianceConstructor%AssembleCov( Input=Input%GetDetInput(Num=i),                           &
                           Abscissa=Response(This%Observations(ii)%iResponse)%GetAbscissaPointer(),  Cov=This%Observations(ii)%L )

          IsDiagonalFlag = IsDiagonal( Array=This%Observations(ii)%L )

          if ( .not. IsDiagonalFlag ) then
            call DPOTRF( 'L', AbscissaSize, This%Observations(ii)%L, AbscissaSize, StatLoc )
            if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DPOTRF', ProcName=ProcName )
          else
            This%Observations(ii)%L = dsqrt(This%Observations(ii)%L)
          end if

          This%Observations(ii)%lnPreExp = Zero
          iii = 1
          do iii = 1, AbscissaSize
            This%Observations(ii)%lnPreExp = This%Observations(ii)%lnPreExp + dlog(This%Observations(ii)%L(iii,iii))
          end do
          This%Observations(ii)%lnPreExp = (-real(AbscissaSize,rkp)/Two*ln2pi - This%Observations(ii)%lnPreExp ) *                &
                                                                                    This%Observations(ii)%NbDegenOutput*NbDataSets
        end if

        imin = (i-1)*This%Observations(ii)%NbDegenOutput+1
        imax = i*This%Observations(ii)%NbDegenOutput

        VarR0D = Zero
        do iii = 1, NbDataSets
          iv = imin
          do iv = imin, imax
            if ( This%MultiplicativeError ) then
              This%Observations(ii)%XmMean(:,1) = DataPtr(:,iii) / OutputPtr(:,iv)
              This%Observations(ii)%XmMean(:,1) = dlog(This%Observations(ii)%XmMean(:,1))
            else
              This%Observations(ii)%XmMean(:,1) = DataPtr(:,iii) - OutputPtr(:,iv)
            end if

            call DTRTRS( 'L', 'N', 'N', AbscissaSize, 1, This%Observations(ii)%L, AbscissaSize, This%Observations(ii)%XmMean(:,:),&
                                                                                                           AbscissaSize, StatLoc )
            if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DTRTRS with code: ' //                            &
                                                                               ConvertToString(Value=StatLoc), ProcName=ProcName )
            VarR0D = VarR0D - 0.5 * dot_product(This%Observations(ii)%XmMean(:,1), This%Observations(ii)%XmMean(:,1))
          end do
        end do

        lnExp = lnExp + VarR0D

      end do

      VarR0D = sum(This%Observations(:)%lnPreExp) + lnExp + This%Scalar

      if ( VarR0D > TVarR0D .and. VarR0D < HVarR0D ) then
        VarR0D = dexp(VarR0D)
      elseif (VarR0D < HVarR0D ) then
        VarR0D = Zero
      else
        call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
                ConvertToString(Value=VarR0D) // '. Consider changing value of the scalar modifier and rerun', ProcName=ProcName )
      end if

      EvaluateStoch = EvaluateStoch + VarR0D

      nullify(OutputPtr)

      nullify(DataPtr)

    end do

    EvaluateStoch = EvaluateStoch / NbDegenInput

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
          LHS%NbObservations = RHS%NbObservations
          LHS%Scalar = RHS%Scalar
          LHS%MultiplicativeError = RHS%MultiplicativeError
          allocate(LHS%Observations, source=RHS%Observations, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Observations', ProcName=ProcName, stat=StatLoc )
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
  
    if ( allocated(This%Observations) ) deallocate(This%Observations, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Observations', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------


  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Obs( This, Debug )

    class(Observations_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize_Obs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'observation'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Obs( This, Debug )

    class(Observations_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset_Obs'
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
  subroutine SetDefaults_Obs( This, Debug )

    class(Observations_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults_Obs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Label = ''
    This%iOutput = 0
    This%iResponse = 0
    This%NbDegenOutput = 0
    This%StochCovFlag = .false.
    This%lnPreExp = Zero

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Obs( This, Input, Prefix, Debug )

    class(Observations_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput_Obs'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

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
  function GetInput_Obs( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput_Obs

    class(Observations_Type), intent(inout)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput_Obs'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName

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

    call GetInput_Obs%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Obs%AddParameter( Name='label', Value=This%Label )
  
    SectionName = 'covariance'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/covariance'
    call GetInput_Obs%AddSection( Section=CovarianceConstructor_Factory%GetObjectInput( Object=This%CovarianceConstructor,        &
                              MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_Obs( LHS, RHS )

    class(Observations_Type), intent(out)                             ::    LHS
    class(Observations_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy_Obs'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (Observations_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
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
  subroutine Finalizer_Obs( This )

    type(Observations_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Obs'
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
