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
use CovFunction_Class                                             ,only:    CovFunction_Type
use HierCovFunction_Class                                         ,only:    HierCovFunction_Type
use HierCovFunction_Factory_Class                                 ,only:    HierCovFunction_Factory
use List2DAllocReal_Class                                         ,only:    List2DAllocReal_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarFixed_Class                                            ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    LikelihoodGauss_Type

type, extends(LikelihoodFunction_Type)                                ::    LikelihoodGauss_Type
  logical                                                             ::    MultiplicativeError
  real(rkp)                                                           ::    Scalar=Zero
  class(HierCovFunction_Type), allocatable                            ::    HierCovFunction
  class(IScalarValue_Type), allocatable                               ::    Multiplier
  logical                                                             ::    PredefinedCov
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
  procedure, private                                                  ::    Evaluate_0D
  procedure, private                                                  ::    Evaluate_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'LikelihoodGauss'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    if (allocated(This%HierCovFunction)) deallocate(This%HierCovFunction, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%HierCovFunction', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%XmMean)) deallocate(This%XmMean, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%XmMean', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Multiplier)) deallocate(This%Multiplier, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Multiplier', ProcName=ProcName, stat=StatLoc)

    This%PredefinedCov = .false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(LikelihoodGauss_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MultiplicativeError = .false.
    This%Scalar = Zero
    This%Label = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

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
    type(IScalarFixed_Type)                                           ::    FixedScalar

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'multiplicative_error'
    call Input%GetValue(Value=VarL0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
    if (Found) This%MultiplicativeError = VarL0D
    
    ParameterName = 'scalar'
    call Input%GetValue(Value=VarR0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
    if (Found) This%Scalar = VarR0D

    ParameterName = 'response'
    call Input%GetValue(Value=VarC0D, ParameterName=Parametername, Mandatory=.true.)
    This%Label = VarC0D

    SectionName = 'multiplier'
    if(Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call IScalarValue_Factory%Construct(Object=This%Multiplier, Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    else
      call FixedScalar%Construct(Value=One)
      allocate(This%Multiplier, source=FixedScalar, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Multiplier', ProcName=ProcName, stat=StatLoc)
    end if

    SectionName = 'predefined_covariance'
    if(Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%L, Prefix=PrefixLoc)
      nullify(InputSection)
      This%PredefinedCov = .true.
      if (size(This%L,1) /= size(This%L,2)) call Error%Raise('Predefined covariance array not square', ProcName=ProcName)
      if (.not. IsDiagonal(Array=This%L)) then
        i = size(This%L,1)
        call DPOTRF('L', i, This%L, i, StatLoc)
        if (StatLoc /= 0) call Error%Raise(Line='Predefined covariance not invertible', ProcName=ProcName)
      else
        This%L = dsqrt(This%L)
      end if
      allocate(This%XmMean(i,1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%XmMean', ProcName=ProcName, stat=StatLoc)
      This%XmMean = Zero
    end if

    SectionName = 'covariance_function'
    if(Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      if (This%PredefinedCov) call Error%Raise("Can't specify both predefined covariance and a " //                            &
                                                                                        "covariance function", ProcName=ProcName)
      call HierCovFunction_Factory%Construct(Object=This%HierCovFunction, Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    else
      if (.not. This%PredefinedCov) call Error%Raise('Must define either a predefined covariance or a ' //                     &
                                                                                        'covariance function', ProcName=ProcName)
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(LikelihoodGauss_Type), intent(inout)                        ::    This
    character(*), intent(in)                                          ::    Name
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
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='multiplicative_error', Value=ConvertToString(Value=This%MultiplicativeError))
    call GetInput%AddParameter(Name='scalar', Value=ConvertToString(Value=This%Scalar))
    call GetInput%AddParameter(Name='response', Value=This%Label)

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/multiplier'
    SectionName = 'multiplier'
    call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Multiplier, Name='multiplier',               &
                                                                         Prefix=PrefixLoc, Directory=DirectorySub))
  
    if (This%PredefinedCov) then
      i = size(This%L,1)
      allocate(VarR2D(i,i), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
      VarR2D = Zero
      call DGEMM('N', 'T', i, i, i, One, This%L, i, This%L, i, Zero, VarR2D, i)
      if (ExternalFlag) then
          SectionName = 'predefined_covariance'
          call GetInput%AddSection(SectionName=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
          FileName = DirectoryLoc // '/predefined_covariance.dat'
          call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
          call ExportArray(Input=InputSection, Array=VarR2D, File=File)
          nullify(InputSection)
      else
          SectionName = 'predefined_covariance'
          call GetInput%AddSection(SectionName=SectionName)
          call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
          call ExportArray(Input=InputSection, Array=VarR2D)
          nullify(InputSection)
      end if
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    end if

    if (allocated(This%HierCovFunction)) then
      SectionName = 'covariance_function'
      if (ExternalFlag) DirectorySub = DirectoryLoc // '/covariance_function'
      call GetInput%AddSection(Section=HierCovFunction_Factory%GetObjectInput(Object=This%HierCovFunction,                      &
                              Name=SectionName, Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_1D(This, Responses, Input, Output, LogValue)

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
      if (Output(i)%GetLabel() == This%Label) then
        iOutput = i
        exit
      end if
    end do

    if (iOutput == 0) call Error%Raise(Line='Did not find required output : ' // This%Label, ProcName=ProcName)

    i = 1
    do i = 1, NbResponses
      if (Responses(i)%GetLabel() == This%Label) then
        iResponse = i
        exit
      end if
    end do

    if (iResponse == 0) call Error%Raise(Line='Did not find required response : ' // This%Label, ProcName=ProcName)

    if (present(LogValue)) then
      Evaluate_1D = This%Evaluate(Response=Responses(iResponse), Input=Input, Output=Output(iOutput), Logvalue=LogValue)
    else
      Evaluate_1D = This%Evaluate(Response=Responses(iResponse), Input=Input, Output=Output(iOutput))
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_0D(This, Response, Input, Output, LogValue)

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
    real(rkp)                                                         ::    MultiplierLoc
    class(CovFunction_Type), allocatable                              ::    CovFunction
    type(SMUQString_Type), allocatable, dimension(:)                  ::    Labels
    real(rkp), pointer, dimension(:,:)                                ::    VarR2DPtr=>null()

    if (Response%GetLabel() /= This%Label) call Error%Raise('Passed incorrect response', ProcName=ProcName)
    if (Output%GetLabel() /= This%Label) call Error%Raise('Passed incorrect output', ProcName=ProcName)

    LogValueLoc = .false.
    if (present(LogValue)) LogValueLoc = LogValue

    ln2pi = dlog(Two*pi)

    Evaluate_0D = Zero

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    ExitFlag = .false.

    NbNodes = 0
  
    ! computing ln(likelihood) values first before transforming them back to linear scale
    lnPreExp = Zero
    lnExp = Zero

    if (.not. Response%IsDataDefined()) call Error%Raise(Line='Data not defined for the response', ProcName=ProcName)

    NbNodes = Response%GetNbNodes()

    if (allocated(This%L)) then
      if (size(This%L,1) /= NbNodes) then
        if (This%PredefinedCov) call Error%Raise('Predefined covariance array dimension mismatch with data',                   &
                                                                                                               ProcName=ProcName)
        deallocate(This%L, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)
        deallocate(This%XmMean, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='This%XmMean', ProcName=ProcName, stat=StatLoc)
      end if
    end if

    if (.not. allocated(This%L)) then
      allocate(This%L(NbNodes,NbNodes), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%L', ProcName=ProcName, stat=StatLoc)
      This%L = Zero
      allocate(This%XmMean(NbNodes,1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%XmMean', ProcName=ProcName, stat=StatLoc)
      This%XmMean = Zero
    end if

    OutputPtr => Output%GetValuesPointer()
    DataPtr => Response%GetDataPointer()
    NbDegen = size(OutputPtr,2)
    NbDataSets = size(DataPtr,2)

    MultiplierLoc = This%Multiplier%GetValue(Input=Input)
    VarR2DPtr => Response%GetCoordinatesPointer()
    allocate(Labels(Response%GetNbIndCoordinates()), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)
    call Response%GetCoordinateLabels(Labels=Labels)

    call This%HierCovFunction%Generate(Input=Input, CovFunction=CovFunction)
    call CovFunction%Evaluate(Coordinates=VarR2DPtr, CoordinateLabels=Labels, Covariance=This%L)
    deallocate(CovFunction, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='CovFunction', ProcName=ProcName, stat=StatLoc)

    IsDiagonalFlag = IsDiagonal(Array=This%L)

    if (.not. IsDiagonalFlag) then
      call DPOTRF('L', NbNodes, This%L, NbNodes, StatLoc)
      if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DPOTRF', ProcName=ProcName)
    else
      This%L = dsqrt(This%L)
    end if
    This%L = This%L * dsqrt(MultiplierLoc)

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
        if (This%MultiplicativeError) then
          This%XmMean(:,1) = DataPtr(:,ii) / OutputPtr(:,iii)
          This%XmMean(:,1) = dlog(This%XmMean(:,1))
        else
          This%XmMean(:,1) = DataPtr(:,ii) - OutputPtr(:,iii)
        end if
        call DTRTRS('L', 'N', 'N', NbNodes, 1, This%L, NbNodes, This%XmMean(:,:), NbNodes, StatLoc)
        if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DTRTRS with code: '// ConvertToString(Value=StatLoc), &
                                                                                                             ProcName=ProcName)
        VarR0D = VarR0D - 0.5 * dot_product(This%XmMean(:,1), This%XmMean(:,1))
      end do
    end do
    lnExp = VarR0D

    Evaluate_0D = lnPreExp + lnExp + This%Scalar

    if (.not. LogValueLoc) then
      if (Evaluate_0D > TVarR0D .and. Evaluate_0D < HVarR0D) then
        Evaluate_0D = dexp(Evaluate_0D)
      elseif (Evaluate_0D < TVarR0D) then
        write(*,'(A)') 'Warning: Likelihood value below machine precision and made 0 where ln(likelihood) is : ' //               &
             ConvertToString(Value=Evaluate_0D)
        Evaluate_0D = Zero
      else
        call Error%Raise(Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
             ConvertToString(Value=Evaluate_0D) // '. Consider changing value of the scalar modifier and rerun for response: ' // &
                                                                                                 This%Label, ProcName=ProcName)
      end if
    end if

    nullify(OutputPtr)
    nullify(DataPtr)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

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

        if (RHS%Constructed) then
          LHS%Scalar = RHS%Scalar
          LHS%MultiplicativeError = RHS%MultiplicativeError
          LHS%Label = RHS%Label
          allocate(LHS%Multiplier, source=RHS%Multiplier, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Multiplier', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%HierCovFunction, source=RHS%HierCovFunction, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%CovarianceConstructor', ProcName=ProcName, stat=StatLoc)
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(LikelihoodGauss_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%HierCovFunction)) deallocate(This%HierCovFunction, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%HierCovFunction', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%XmMean)) deallocate(This%XmMean, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%XmMean', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Multiplier)) deallocate(This%Multiplier, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Multiplier', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
