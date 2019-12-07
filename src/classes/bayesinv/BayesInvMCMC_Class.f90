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

module BayesInvMCMC_Class

use Input_Library
use Parameters_Library
use String_Library
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use Output_Class                                                  ,only:    Output_Type
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use MCMCMethod_Class
use MCMCMethod_Factory_Class                                      ,only:    MCMCMethod_Factory
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type
use LikelihoodFunction_Vec_Class                                  ,only:    LikelihoodFunction_Vec_Type
use LikelihoodGauss_Class                                         ,only:    LikelihoodGauss_Type
use LikelihoodFunction_Factory_Class                              ,only:    LikelihoodFunction_Factory
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use DistProb_Class                                                ,only:    DistProb_Type 
use BayesInvMethod_Class                                          ,only:    BayesInvMethod_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use Model_Class                                                   ,only:    Model_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use SpaceHierParam_Class                                          ,only:    SpaceHierParam_Type

implicit none

private

public                                                                ::    BayesInvMCMC_Type

type, extends(BayesInvMethod_Type)                                    ::    BayesInvMCMC_Type
  logical                                                             ::    Silent=.false.
  type(LikelihoodFunction_Vec_Type), allocatable, dimension(:)        ::    LikelihoodFunctionVec
  class(MCMCMethod_Type), allocatable                                 ::    MCMC
  type(SpaceHierParam_Type)                                           ::    HierarchicalSpace
  type(SpaceSampler_Type)                                             ::    HierarchicalSampler
  logical                                                             ::    Hierarchical
  logical                                                             ::    TransformBounded
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Calibrate
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'BayesInvMCMC'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%LikelihoodFunctionVec) ) deallocate(This%LikelihoodFunctionVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LikelihoodFunctionVec', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MCMC) ) deallocate(This%MCMC, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MCMC', ProcName=ProcName, stat=StatLoc )

    call This%HierarchicalSpace%Reset()
    call This%HierarchicalSampler%Reset()

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Hierarchical = .false.
    This%TransformBounded = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    class(LikelihoodFunction_Type), allocatable                       ::    LikelihoodFunction
    integer                                                           ::    NbLikelihoods


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName = 'transform_bounded_distributions'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%TransformBounded = VarL0D

    SectionName = 'mcmc'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call MCMCMethod_Factory%Construct( Object=This%MCMC, Input=InputSection, SectionChain=SectionChain // '>mcmc',                &
                                                                                                                Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'likelihood'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    NbLikelihoods = InputSection%GetNumberofSubSections()
    nullify( InputSection )
    if ( NbLikelihoods <= 0 ) call Error%Raise( 'Must specify at least one likelihood function', ProcName=ProcName )
    allocate(This%LikelihoodFunctionVec(NbLikelihoods), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%LikelihoodFunctionVec', ProcName=ProcName, stat=StatLoc )
    i = 1
    do i = 1, NbLikelihoods
      SubSectionName = SectionName // '>likelihood' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call LikelihoodFunction_Factory%Construct( Object=LikelihoodFunction, Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%LikelihoodFunctionVec(i)%Set( Object=LikelihoodFunction )
    end do

    deallocate(LikelihoodFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LikelihoodFunction', ProcName=ProcName, stat=StatLoc )

    SectionName = 'hierarchical'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      SubSectionName = SectionName // '>sampler'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%HierarchicalSampler%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )

      SubSectionName = SectionName // '>parameter_space'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%HierarchicalSpace%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
      if ( This%HierarchicalSpace%IsCorrelated() ) call Error%Raise( Line='Correlated spaces not supported', ProcName=ProcName )
  
      This%Hierarchical = .true.
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
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
    integer                                                           ::    NbLikelihoods
    integer                                                           ::    i
    class(LikelihoodFunction_Type), pointer                           ::    LikelihoodPtr=>null()

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

    call GetInput%AddParmaeter( Name='transform_bounded_distributions', Value=ConvertToString(Value=This%TransformBounded) )

    SectionName = 'mcmc'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/mcmc'
    call GetInput%AddSection( Section=MCMCMethod_Factory%GetObjectInput(Object=This%MCMC, MainSectionName=SectionName,            &
                                                                                       Prefix=PrefixLoc, Directory=DirectorySub) )

    SectionName = 'likelihood'
    call GetInput%AddSection( SectionName=SectionName )
    NbLikelihoods = size(This%LikelihoodFunctionVec,1)
    i = 1
    do i = 1, NbLikelihoods
      SubSectionName = 'likelihood' // ConvertToString(Value=i) 
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/likelihood' // ConvertToString(Value=i) 
      LikelihoodPtr => This%LikelihoodFunctionVec(i)%GetPointer()
      call GetInput%AddSection( Section=LikelihoodFunction_Factory%GetObjectInput(Object=LikelihoodPtr,                           &
                            MainSectionName=SubSectionName, Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName )
      nullify(LikelihoodPtr)
    end do

    if ( This%Hierarchical ) then
      call GetInput%AddSection( SectionName='hierarchical' )

      SubSectionName = 'sampler'
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
      call GetInput%AddSection( Section=This%HierarchicalSampler%GetInput( MainSectionName=SubSectionName, Prefix=PrefixLoc,      &
                                                                             Directory=DirectorySub ), To_SubSection=SectionName )

      SubSectionName = 'parameter_space'
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/parameter_space'
      call GetInput%AddSection( Section=This%HierarchicalSpace%GetInput( MainSectionName=SubSectionName, Prefix=PrefixLoc,        &
                                                                             Directory=DirectorySub ), To_SubSection=SectionName )

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Calibrate( This, Model, SpaceInput, Responses, OutputDirectory, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(SpaceInput_Type), intent(in)                                ::    SpaceInput
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Calibrate'
    integer                                                           ::    StatLoc=0
    type(ModelInterface_Type)                                         ::    ModelInterface
    character(:), allocatable                                         ::    OutputDirectoryLoc
    procedure(MCMCSamplingTarget), pointer                            ::    Posterior=>null()
    real(rkp), allocatable, dimension(:)                              ::    PosteriorChain
    real(rkp), allocatable, dimension(:,:)                            ::    ParamChain
    real(rkp), allocatable, dimension(:,:)                            ::    MiscChain
    type(String_Type), allocatable, dimension(:)                      ::    Labels
    integer                                                           ::    i
    type(Output_Type), allocatable, dimension(:)                      ::    Output
    type(Output_Type), allocatable, dimension(:,:)                    ::    HierOutput
    real(rkp), allocatable, dimension(:,:)                            ::    HierSamples
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    type(SpaceParam_Type)                                             ::    SpaceParamRealization
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D
    class(SpaceTransf_Type), allocatable                              ::    TargetSpace
    type(ModelTransf_Type)                                            ::    TargetModel
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    if ( SpaceInput%IsCorrelated() ) call Error%Raise( Line='Bayesian inference does not support correlated spaces',              &
                                                                                                               ProcName=ProcName )    

    ! transforming target space from bounded to unbounded if specified
    if ( .not. This%TransformBounded ) then
      call TargetSpace%Construct( Distributions=SpaceInput%GetDistribution(), CorrMat=SpaceInput%GetCorrMat(),                    &
                                                                   Labels=SpaceInput%GetLabel(), Names=SpaceInput%GetParamName() )
    else
      if ( SpaceInput%IsCorrelated() ) call Error%Raise( 'Bounded to unbounded domain transformation not available for ' //       &
                                                                                          'correlated spaces', ProcName=ProcName )
      allocate(DistVecTarget(SpaceInput%GetNbDim()), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='DistVecTarget', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, SpaceInput%GetNbDim()
        DistProbTarget => SpaceInput%GetDistributionPointer( Num=i )
        if ( DistProbTarget%IsTruncatedLeft() .or. DistProbTarget%IsTruncatedRight() ) then
          call DistTransf%Construct( Distribution=DistProbTarget )
          call DistVecTarget(i)%Set(Object=DistTransf)
          call DistTransf%Reset()
        else
          call DistVecTarget(i)%Set(Object=DistProbTarget)
        end if
      end do
      call TargetSpace%Construct( Distributions=DistVecTarget, CorrMat=SpaceInput%GetCorrMat(), Labels=SpaceInput%GetLabel(),     &
                                                                                                 Names=SpaceInput%GetParamName() )
      deallocate(DistVecTarget, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistVecTarget', ProcName=ProcName, stat=StatLoc )
    end if

    ! setting up posterior pointer for either hierarchical or non-hierarchical problem
    if ( This%Hierarchical ) then
      Posterior => MCMCPosteriorHier
      allocate(Labels(This%HierarchicalSpace%GetNbDim()+TargetSpace%GetNbDim()), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Labels', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, size(Labels)
        if ( i <= TargetSpace%GetNbDim() ) then
          Labels(i) = TargetSpace%GetLabel(Num=i)
        else
          Labels(i) = This%HierarchicalSpace%GetLabel(Num=i-TargetSpace%GetNbDim())
        end if
      end do
    else
      Posterior => MCMCPosterior
    end if

    call ModelInterface%Construct( Model=Model, Responses=Responses )

    if ( present(OutputDirectory) ) OutputDirectoryLoc = OutputDirectory // '/posterior_sampler'

    call This%MCMC%GenerateChain( SamplingTarget=Posterior, SpaceInput=TargetSpace, ParameterChain=ParamChain,                    &
                                             TargetChain=PosteriorChain, MiscChain=MiscChain, OutputDirectory=OutputDirectoryLoc )

    deallocate(Output, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )

    if ( allocated(HierSamples) ) then
      deallocate(HierSamples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='HierSamples', ProcName=ProcName, stat=StatLoc )
    end if

    if ( allocated(VarR1D) ) then
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    if ( present(OutputDirectory) ) call This%WriteOutput( SpaceInput=SpaceInput, PosteriorChain=PosteriorChain,                  &
                                                           ParamChain=ParamChain, MiscChain=MiscChain, Directory=OutputDirectory )

    deallocate(PosteriorChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PosteriorChain', ProcName=ProcName, stat=StatLoc )

    deallocate(ParamChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PosteriorParam', ProcName=ProcName, stat=StatLoc )

    deallocate(MiscChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='MiscChain', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

    contains

      !!--------------------------------------------------------------------------------------------------------------------------
      subroutine MCMCPosterior( Input, Value, MiscValues )

        type(InputDet_Type), intent(in)                                   ::    Input
        real(rkp), intent(out)                                            ::    Value
        real(rkp), allocatable, dimension(:), intent(inout)               ::    MiscValues

        logical                                                           ::    DebugLoc
        character(*), parameter                                           ::    ProcName='MCMCPosterior'
        integer                                                           ::    i
        real(rkp)                                                         ::    Likelihood
        real(rkp)                                                         ::    Prior
        class(DistProb_Type), pointer                                     ::    DistProb
        real(rkp)                                                         ::    VarR0DLoc
        integer                                                           ::    RunStat
        integer                                                           ::    iLoc
        class(LikelihoodFunction_Type), pointer                           ::    LikelihoodPtr=>null()
        
        DebugLoc = DebugGlobal
        if ( present(Debug) ) DebugLoc = Debug
        if (DebugLoc) call Logger%Entering( ProcName )

        Likelihood = Zero
        Prior = Zero

        if ( allocated(MiscValues) ) then
          if ( size(MiscValues) /= 2 ) then
            deallocate(MiscValues, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='MiscValues', ProcName=ProcName, stat=StatLoc )
          end if
        end if
        if ( .not. allocated(MiscValues) ) then
          allocate(MiscValues(2), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='MiscValues', ProcName=ProcName, stat=StatLoc )
        end if

        Prior = One
        iLoc = 1
        do iLoc = 1, TargetSpace%GetNbDim()
          DistProb => TargetSpace%GetDistributionPointer( Num=iLoc )
          call Input%GetValue( Value=VarR0DLoc, Label=TargetSpace%GetLabel(Num=iLoc) )
          Prior = Prior * DistProb%PDF( X=VarR0DLoc )
        end do
        nullify( DistProb )

        MiscValues(1) = Prior

        if ( Prior > Zero ) then
          call ModelInterface%Run( Input=Input, Output=Output, Stat=RunStat )
          if ( RunStat == 0 ) then
            Likelihood = Zero
            iLoc = 1
            do iLoc = 1, size(This%LikelihoodFunctionVec,1)
              LikelihoodPtr => This%LikelihoodFunctionVec(iLoc)%GetPointer()
              Likelihood = Likelihood + LikelihoodPtr%Evaluate( Responses=Responses, Input=Input, Output=Output, LogValue=.true. )
              nullify(LikelihoodPtr)
            end do
            if ( Likelihood > TVarR0D .and. Likelihood < HVarR0D ) then
              Likelihood = dexp(Likelihood)
            elseif (Likelihood < TVarR0D ) then
              Likelihood = Zero
            else
              call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                     &
                   ConvertToString(Value=Likelihood) // '. Consider changing value of the scalar modifier of responses' )
            end if
            MiscValues(2) = Likelihood
            Value = Likelihood * Prior
          else
            MiscValues(2) = Zero
            Value = Zero
            write(*,*) ''
            write(*,'(A)') 'Model execution failed.'
          end if
        else
          MiscValues(2) = Zero
          Value = Zero
        end if

        if (DebugLoc) call Logger%Exiting()

      end subroutine
      !!--------------------------------------------------------------------------------------------------------------------------

      !!--------------------------------------------------------------------------------------------------------------------------
      subroutine MCMCPosteriorHier( Input, Value, MiscValues )

        type(InputDet_Type), intent(in)                                   ::    Input
        real(rkp), intent(out)                                            ::    Value
        real(rkp), allocatable, dimension(:), intent(inout)               ::    MiscValues

        logical                                                           ::    DebugLoc
        character(*), parameter                                           ::    ProcName='MCMCPosterior'
        real(rkp)                                                         ::    Prior
        class(DistProb_Type), pointer                                     ::    DistProb
        type(InputDet_Type), allocatable, dimension(:)                    ::    HierInput
        real(rkp)                                                         ::    VarR0DLoc
        integer                                                           ::    RunStat
        real(rkp)                                                         ::    Likelihood
        integer                                                           ::    iLoc
        integer                                                           ::    iiLoc
        class(LikelihoodFunction_Type), pointer                           ::    LikelihoodPtr=>null()
        integer                                                           ::    NbDimOrig
        integer                                                           ::    NbDimHier
        integer                                                           ::    NbHierSamples

        DebugLoc = DebugGlobal
        if ( present(Debug) ) DebugLoc = Debug
        if (DebugLoc) call Logger%Entering( ProcName )

        Likelihood = Zero
        Prior = Zero

        NbDimOrig = Input%GetNbInputs()

        if ( allocated(MiscValues) ) then
          if ( size(MiscValues) /= 2 ) then
            deallocate(MiscValues, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='MiscValues', ProcName=ProcName, stat=StatLoc )
          end if
        end if
        if ( .not. allocated(MiscValues) ) then
          allocate(MiscValues(2), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='MiscValues', ProcName=ProcName, stat=StatLoc )
        end if

        Prior = One
        iLoc = 1
        do iLoc = 1, SpaceInput%GetNbDim()
          DistProb => SpaceInput%GetDistributionPointer( Num=iLoc )
          call Input%GetValue( Value=VarR0DLoc, Label=SpaceInput%GetLabel(Num=iLoc) )
          Prior = Prior * DistProb%PDF( X=VarR0DLoc )
        end do
        nullify( DistProb )

        MiscValues(1) = Prior

        if ( Prior > Zero ) then

          call This%HierarchicalSpace%Generate( Input=Input, SpaceParam=SpaceParamRealization )
          NbDimHier = SpaceParamRealization%GetNbDim()
          HierSamples = This%HierarchicalSampler%Draw( SpaceInput=SpaceParamRealization )
          NbHierSamples = size(HierSamples,2)

          if ( allocated(VarR1D) ) then
            if ( size(VarR1D,1) /= NbDimOrig + NbDimHier ) then
              deallocate(VarR1D, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
            end if
          end if

          if ( .not. allocated(VarR1D) ) then
            allocate(VarR1D(NbDimOrig + NbDimHier), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
          end if

          iLoc = 1
          do iLoc = 1, NbDimOrig
            call Input%GetValue( Value=VarR1D(iLoc), Label=Labels(iLoc)%GetValue() )
          end do

          allocate(HierInput(NbHierSamples), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='HierInput', ProcName=ProcName, stat=StatLoc )

          iLoc = 1
          do iLoc = 1, NbHierSamples
            VarR1D(NbDimOrig+1:) = HierSamples(:,iLoc)
            call HierInput(iLoc)%Construct( Input=VarR1D, Labels=Labels )
          end do

          call ModelInterface%Run( Input=HierInput, Output=HierOutput, Stat=RunStat )

          if ( RunStat == 0 ) then
            VarR0DLoc = Zero
            iiLoc = 1
            do iiLoc = 1, NbHierSamples
              Likelihood = Zero
              iLoc = 1
              do iLoc = 1, size(Responses,1)
                LikelihoodPtr => This%LikelihoodFunctionVec(iLoc)%GetPointer()
                Likelihood = Likelihood + LikelihoodPtr%Evaluate( Responses=Responses, Input=HierInput(iiLoc),                      &
                                                                                     Output=HierOutput(:,iiLoc), LogValue=.true. )
                nullify(LikelihoodPtr)
              end do
              if ( Likelihood > TVarR0D .and. Likelihood < HVarR0D ) then
                Likelihood = dexp(Likelihood)
              elseif (Likelihood < TVarR0D ) then
                Likelihood = Zero
              else
                call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                     &
                     ConvertToString(Value=Likelihood) // '. Consider changing value of the scalar modifier of responses' )
              end if
              VarR0DLoc = VarR0DLoc + Likelihood
            end do
            Likelihood = VarR0DLoc / real(NbHierSamples,rkp)
            MiscValues(2) = Likelihood
            Value = Likelihood * Prior
          else
            MiscValues(2) = Zero
            Value = Zero
            write(*,*) ''
            write(*,'(A)') 'Model execution failed.'
          end if

        else
          MiscValues(2) = Zero
          Value = Zero
        end if

        if (DebugLoc) call Logger%Exiting()

      end subroutine
      !!--------------------------------------------------------------------------------------------------------------------------

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, SpaceInput, PosteriorChain, ParamChain, MiscChain, Directory, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    class(SpaceInput_Type), intent(in)                                ::    SpaceInput
    real(rkp), dimension(:,:),  intent(in)                            ::    ParamChain
    real(rkp), dimension(:,:),  intent(in)                            ::    MiscChain
    real(rkp),dimension(:), optional, intent(in)                      ::    PosteriorChain
    character(*), intent(in)                                          ::    Directory
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='WriteOutput'
    type(InputSection_Type)                                           ::    Input
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    logical                                                           ::    SilentLoc
    type(SMUQFile_Type)                                               ::    File

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      if ( .not. SilentLoc ) then
        write(*,'(A)') ''
        write(*,'(A)') 'Writing Bayesian inference data to the output folder'
      end if

      PrefixLoc = Directory

      FileName = '/parameter_names.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=SpaceInput%GetParamName(), File=File )

      FileName = '/prior_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=MiscChain(1,:), File=File )

      FileName = '/likelihood_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=MiscChain(2,:), File=File )

      FileName = '/parameter_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=ParamChain, File=File )

      FileName = '/posterior_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=PosteriorChain, File=File )

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(BayesInvMCMC_Type), intent(out)                             ::    LHS
    class(BayesInvMethod_Type), intent(in)                            ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (BayesInvMCMC_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Hierarchical = RHS%Hierarchical
          allocate(LHS%LikelihoodFunctionVec, source=RHS%LikelihoodFunctionVec, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%LikelihoodFunctionVec', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%MCMC, source=RHS%MCMC, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MCMC', ProcName=ProcName, stat=StatLoc )
          if ( LHS%Hierarchical ) then
            LHS%HierarchicalSpace = RHS%HierarchicalSpace
            LHS%HierarchicalSampler = RHS%HierarchicalSampler
          end if
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(BayesInvMCMC_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%LikelihoodFunctionVec) ) deallocate(This%LikelihoodFunctionVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LikelihoodFunctionVec', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MCMC) ) deallocate(This%MCMC, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MCMC', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


end module
