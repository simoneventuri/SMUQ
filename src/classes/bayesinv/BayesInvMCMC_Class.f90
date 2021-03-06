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
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringConversion_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Output_Class                                                  ,only:    Output_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility, RestartTarget
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use MCMCMethod_Class
use MCMCMethod_Factory_Class                                      ,only:    MCMCMethod_Factory
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use SampleMethod_Factory_Class                                    ,only:    SampleMethod_Factory
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use DistProb_Class                                                ,only:    DistProb_Type 
use BayesInvMethod_Class                                          ,only:    BayesInvMethod_Type
use Input_Class                                                   ,only:    Input_Type
use Model_Class                                                   ,only:    Model_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use HierParamSpace_Class                                          ,only:    HierParamSpace_Type
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use TransfSampleSpaceUnbound_Class                                ,only:    TransfSampleSpaceUnbound_Type
use TransfSampleSpaceNone_Class                                   ,only:    TransfSampleSpaceNone_Type
use MultiVarDist_Class                                            ,only:    MultiVarDist_Type
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    BayesInvMCMC_Type

type, extends(BayesInvMethod_Type)                                    ::    BayesInvMCMC_Type
  logical                                                             ::    Silent=.false.
  class(MCMCMethod_Type), allocatable                                 ::    MCMC
  type(HierParamSpace_Type)                                           ::    HierarchicalSpace
  class(SampleMethod_Type), allocatable                               ::    HierarchicalSampler
  integer                                                             ::    HierarchicalNbSamples
  logical                                                             ::    Hierarchical
  logical                                                             ::    TransformBounded
  logical                                                             ::    DebugStop
contains
  procedure, public                                                   ::    Reset
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
subroutine Reset(This)

  class(BayesInvMCMC_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  if (allocated(This%MCMC)) deallocate(This%MCMC, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%MCMC', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%HierarchicalSampler)) deallocate(This%HierarchicalSampler, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%HierarchicalSampler', ProcName=ProcName, stat=StatLoc)

  call This%HierarchicalSpace%Reset()

  This%Hierarchical = .false.
  This%TransformBounded = .false.
  This%DebugStop = .false.
  This%HierarchicalNbSamples = 0
  This%SectionChain = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, SectionChain, Prefix)

  class(BayesInvMCMC_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), intent(in)                                            ::    SectionChain
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  class(HierDistProb_Type), pointer                                   ::    HierDistProbPointer=>null()
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%SectionChain = SectionChain

  call InputVerifier%Construct()

  ParameterName = 'debug_stop'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%DebugStop = VarL0D

  ParameterName = 'transform_bounded_distributions'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%TransformBounded = VarL0D

  SectionName = 'mcmc'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call MCMCMethod_Factory%Construct(Object=This%MCMC, Input=InputSection, SectionChain=SectionChain // '>mcmc',                &
                                                                                                              Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'hierarchical'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then

    ParameterName = 'nb_samples'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    This%HierarchicalNbSamples = VarI0D

    if (This%HierarchicalNbSamples <= 0) call Error%Raise('Must specify number of hierarchical samples above 0',             &
                                                                                                              ProcName=ProcName)

    SubSectionName = SectionName // '>sampler'
    call InputVerifier%AddSection(Section='sampler', ToSubSection=SectionName)
    if (Input%HasSection(SubSectionName=SubSectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call SampleMethod_Factory%Construct(Object=This%HierarchicalSampler, Input=InputSection, Prefix=PrefixLoc)
    else
      allocate(SampleLHS_Type   :: This%HierarchicalSampler)
      select type (Object => This%HierarchicalSampler)
        type is (SampleLHS_Type)
          call Object%Construct()
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    SubSectionName = SectionName // '>parameter_space'
    call InputVerifier%AddSection(Section='parameter_space', ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call This%HierarchicalSpace%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    if (This%HierarchicalSpace%IsCorrelated()) call Error%Raise(Line='Correlated spaces not supported', ProcName=ProcName)

    i = 1
    do i = 1, This%HierarchicalSpace%GetNbDim()
      HierDistProbPointer => This%HierarchicalSpace%GetDistributionPointer(Num=i)
      if (.not. (HierDistProbPointer%IsTruncatedLeft() .and. HierDistProbPointer%IsTruncatedRight())) then
        call Error%Raise('Can only treat hierarchical spaces with truncated marginal distributions', ProcName=ProcName)
      end if
      nullify(HierDistProbPointer)
    end do
    This%Hierarchical = .true.
  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(BayesInvMCMC_Type), intent(inout)                             ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    NbLikelihoods
  integer                                                             ::    i
  class(LikelihoodFunction_Type), pointer                             ::    LikelihoodPtr=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='debug_stop', Value=ConvertToString(Value=This%DebugStop))
  call GetInput%AddParameter(Name='transform_bounded_distributions', Value=ConvertToString(Value=This%TransformBounded))

  SectionName = 'mcmc'
  if (ExternalFlag) DirectorySub = DirectoryLoc // 'mcmc/'
  call GetInput%AddSection(Section=MCMCMethod_Factory%GetObjectInput(Object=This%MCMC, Name=SectionName,            &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  if (This%Hierarchical) then
    call GetInput%AddSection(SectionName='hierarchical')

    call GetInput%AddParameter(Name='nb_samples', Value=ConvertToString(Value=This%HierarchicalNbSamples),                     &
                                                                                                        SectionName=SectionName)

    if (ExternalFlag) DirectorySub = DirectoryLoc // 'sampler/'
    call GetInput%AddSection(Section=SampleMethod_Factory%GetObjectInput(Object=This%HierarchicalSampler,                     &
                              Name='sampler', Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName)

    SubSectionName = 'parameter_space'
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'parameter_space/'
    call GetInput%AddSection(Section=This%HierarchicalSpace%GetInput(Name=SubSectionName, Prefix=PrefixLoc,        &
                                                                            Directory=DirectorySub), To_SubSection=SectionName)
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Calibrate(This, Model, SampleSpace, Responses, LikelihoodFunction, OutputDirectory)

  class(BayesInvMCMC_Type), intent(inout)                             ::    This
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  class(Model_Type), intent(inout)                                    ::    Model
  class(LikelihoodFunction_Type), intent(inout)                       ::    LikelihoodFunction
  character(*), optional, intent(in)                                  ::    OutputDirectory

  character(*), parameter                                             ::    ProcName='Calibrate'
  integer                                                             ::    StatLoc=0
  type(ModelInterface_Type)                                           ::    ModelInterface
  character(:), allocatable                                           ::    OutputDirectoryLoc
  procedure(MCMCSamplingTarget), pointer                              ::    Posterior=>null()
  real(rkp), allocatable, dimension(:)                                ::    PosteriorChain
  real(rkp), allocatable, dimension(:,:)                              ::    ParamChain
  real(rkp), allocatable, dimension(:,:)                              ::    MiscChain
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Labels
  integer                                                             ::    i
  type(Output_Type), allocatable, dimension(:)                        ::    Output
  type(Output_Type), allocatable, dimension(:,:)                      ::    HierOutput
  integer, allocatable, dimension(:)                                  ::    HierRunStat
  real(rkp), allocatable, dimension(:,:)                              ::    HierSamples
  real(rkp), allocatable, dimension(:)                                ::    HierSamplesRealization
  real(rkp), allocatable, dimension(:)                                ::    OrigInputValues
  type(ParamSpace_Type)                                               ::    ParamSpaceRealization
  real(rkp)                                                           ::    VarR0D
  real(rkp)                                                           ::    HVarR0D
  real(rkp)                                                           ::    TVarR0D
  class(TransfSampleSpace_Type), allocatable                          ::    TargetSpace
  integer                                                             ::    NbDimOrig
  integer                                                             ::    NbDimHier
  type(MultiVarDist_Type)                                             ::    PriorDistribution

  HVarR0D = dlog(huge(VarR0D))
  TVarR0D = dlog(tiny(VarR0D))

  ! transforming target space from bounded to unbounded if specified
  if (This%TransformBounded) then
    allocate(TransfSampleSpaceUnBound_Type   :: TargetSpace)
  else
    allocate(TransfSampleSpaceNone_Type   :: TargetSpace)
  end if

  allocate(OrigInputValues(SampleSpace%GetNbDim()), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='OrigInputValues', ProcName=ProcName, stat=StatLoc)
  OrigInputValues = Zero

  select type(TargetSpace)
    type is (TransfSampleSpaceNone_Type) 
      call TargetSpace%Construct(OriginalSampleSpace=SampleSpace)
    type is (TransfSampleSpaceUnbound_Type)
      call TargetSpace%Construct(OriginalSampleSpace=SampleSpace)
    class default
      call Error%Raise('Something went wrong', ProcName=ProcName)
  end select

  call PriorDistribution%Construct(SampleSpace=TargetSpace)

  NbDimOrig = TargetSpace%GetNbDim()
  NbDimHier = 0

  ! setting up posterior pointer for either hierarchical or non-hierarchical problem
  if (This%Hierarchical) then
    NbDimHier = This%HierarchicalSpace%GetNbDim()
    allocate(HierSamplesRealization(NbDimOrig + NbDimHier), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='HierSamplesRealization', ProcName=ProcName, stat=StatLoc)
    Posterior => MCMCPosteriorHier
  else
    Posterior => MCMCPosterior
  end if

  allocate(Labels(NbDimHier+NbDimOrig), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(Labels)
    if (i <= NbDimOrig) then
      Labels(i) = TargetSpace%GetLabel(Num=i)
    else
      Labels(i) = This%HierarchicalSpace%GetLabel(Num=i-TargetSpace%GetNbDim())
    end if
  end do

  call ModelInterface%Construct(Model=Model, Responses=Responses)

  allocate(Output(ModelInterface%GetNbResponses()), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Output', ProcName=ProcName, stat=StatLoc)

  if (present(OutputDirectory)) OutputDirectoryLoc = OutputDirectory // 'posterior_sampler/'

  call This%MCMC%GenerateChain(SamplingTarget=Posterior, SampleSpace=TargetSpace, ParameterChain=ParamChain,                   &
                                            TargetChain=PosteriorChain, MiscChain=MiscChain, OutputDirectory=OutputDirectoryLoc)

  call TargetSpace%InvTransform(Z=ParamChain)

  if (allocated(Output)) deallocate(Output, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Output', ProcName=ProcName, stat=StatLoc)

  if (allocated(HierOutput)) deallocate(HierOutput, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='HierOutput', ProcName=ProcName, stat=StatLoc)

  if (allocated(HierSamples)) deallocate(HierSamples, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='HierSamples', ProcName=ProcName, stat=StatLoc)

  if (present(OutputDirectory)) call This%WriteOutput(SampleSpace=SampleSpace, PosteriorChain=PosteriorChain,                &
                                                          ParamChain=ParamChain, MiscChain=MiscChain, Directory=OutputDirectory)

  deallocate(PosteriorChain, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='PosteriorChain', ProcName=ProcName, stat=StatLoc)

  deallocate(ParamChain, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='PosteriorParam', ProcName=ProcName, stat=StatLoc)

  deallocate(MiscChain, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='MiscChain', ProcName=ProcName, stat=StatLoc)

  deallocate(OrigInputValues, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='OrigInputValues', ProcName=ProcName, stat=StatLoc)

  contains

    !!--------------------------------------------------------------------------------------------------------------------------
    subroutine MCMCPosterior(Input, Value, MiscValues)

      type(Input_Type), intent(in)                                        ::    Input
      real(rkp), intent(out)                                              ::    Value
      real(rkp), allocatable, dimension(:), intent(inout)                 ::    MiscValues

      character(*), parameter                                             ::    ProcName='MCMCPosterior'
      real(rkp)                                                           ::    Likelihood
      real(rkp)                                                           ::    Prior
      integer                                                             ::    RunStat
      type(Input_Type)                                                    ::    InputLoc

      NbDimOrig = Input%GetNbInputs()

      Likelihood = Zero
      Prior = Zero

      if (allocated(MiscValues)) then
        if (size(MiscValues) /= 2) then
          deallocate(MiscValues, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='MiscValues', ProcName=ProcName, stat=StatLoc)
        end if
      end if
      if (.not. allocated(MiscValues)) then
        allocate(MiscValues(2), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='MiscValues', ProcName=ProcName, stat=StatLoc)
      end if

      call Input%GetValue(Values=OrigInputValues) 

      Prior = PriorDistribution%PDF(X=OrigInputValues)

      call TargetSpace%InvTransform(Z=OrigInputValues)

      call InputLoc%Construct(Input=OrigInputValues, Labels=Labels(1:NbDimOrig))

      MiscValues(1) = Prior

      if (Prior > Zero) then
        call ModelInterface%Run(Input=InputLoc, Output=Output, Stat=RunStat)
        if (RunStat == 0) then
          Likelihood = LikelihoodFunction%Evaluate(Responses=Responses, Input=InputLoc, Output=Output)
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

      if (This%DebugStop) stop

    end subroutine
    !!--------------------------------------------------------------------------------------------------------------------------

    !!--------------------------------------------------------------------------------------------------------------------------
    subroutine MCMCPosteriorHier(Input, Value, MiscValues)

      type(Input_Type), intent(in)                                        ::    Input
      real(rkp), intent(out)                                              ::    Value
      real(rkp), allocatable, dimension(:), intent(inout)                 ::    MiscValues

      character(*), parameter                                             ::    ProcName='MCMCPosterior'
      real(rkp)                                                           ::    Prior
      type(Input_Type), allocatable, dimension(:)                         ::    HierInput
      real(rkp)                                                           ::    Likelihood
      integer                                                             ::    iLoc
      integer                                                             ::    NbHierSamples
      type(Input_Type)                                                    ::    InputLoc

      Likelihood = Zero
      Prior = Zero

      if (allocated(MiscValues)) then
        if (size(MiscValues) /= 2) then
          deallocate(MiscValues, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='MiscValues', ProcName=ProcName, stat=StatLoc)
        end if
      end if
      if (.not. allocated(MiscValues)) then
        allocate(MiscValues(2), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='MiscValues', ProcName=ProcName, stat=StatLoc)
      end if

      call Input%GetValue(Values=OrigInputValues) 

      Prior = PriorDistribution%PDF(X=OrigInputValues)

      call InputLoc%Construct(Input=OrigInputValues, Labels=Labels(1:NbDimOrig))

      MiscValues(1) = Prior

      if (Prior > Zero) then

        call This%HierarchicalSpace%Generate(Input=InputLoc, ParamSpace=ParamSpaceRealization)
        call ParamSpaceRealization%Draw(Samples=HierSamples, Sampler=This%HierarchicalSampler, &
                                        NbSamples=This%HierarchicalNbSamples)
        NbHierSamples = This%HierarchicalNbSamples

        if (allocated(HierOutput)) then
          if (size(HierOutput,2) /= NbHierSamples .or. size(HierOutput,1) /= ModelInterface%GetNbResponses()) then
            deallocate(HierOutput, stat=StatLoc)
            if (StatLoc /= 0) call Error%Deallocate(Name='HierOutput', ProcName=ProcName, stat=StatLoc)
          end if
        end if

        if (.not. allocated(HierOutput)) then
          allocate(HierOutput(ModelInterface%GetNbResponses(),NbHierSamples), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='HierOutput', ProcName=ProcName, stat=StatLoc)
        end if

        if (allocated(HierRunStat)) then
          if (size(HierRunStat,1) /= NbHierSamples) then
            deallocate(HierRunStat, stat=StatLoc)
            if (StatLoc /= 0) call Error%Deallocate(Name='HierRunStat', ProcName=ProcName, stat=StatLoc)
          end if
        end if

        if (.not. allocated(HierRunStat)) then
          allocate(HierRunStat(NbHierSamples), stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='HierRunStat', ProcName=ProcName, stat=StatLoc)
        end if
        HierRunStat = 1

        HierSamplesRealization(1:NbDimOrig) = OrigInputValues

        allocate(HierInput(NbHierSamples), stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='HierInput', ProcName=ProcName, stat=StatLoc)

        iLoc = 1
        do iLoc = 1, NbHierSamples
          HierSamplesRealization(NbDimOrig+1:) = HierSamples(:,iLoc)
          call HierInput(iLoc)%Construct(Input=HierSamplesRealization, Labels=Labels)
        end do

        call ModelInterface%Run(Input=HierInput, Output=HierOutput, Stat=HierRunStat)

        if (count(HierRunStat == 0) >= ceiling(0.75_rkp*real(NbHierSamples,rkp))) then
          Likelihood = Zero
          iLoc = 1
          do iLoc = 1, NbHierSamples
            if (HierRunStat(i) /= 0) cycle
            Likelihood = Likelihood + LikelihoodFunction%Evaluate(Responses=Responses, Input=HierInput(iLoc),                  &
                                                                                                      Output=HierOutput(:,iLoc))
          end do
          Likelihood = Likelihood / real(count(HierRunStat == 0),rkp)
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

      if (This%DebugStop) stop

    end subroutine
    !!--------------------------------------------------------------------------------------------------------------------------

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteOutput(This, SampleSpace, PosteriorChain, ParamChain, MiscChain, Directory)

  class(BayesInvMCMC_Type), intent(inout)                             ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  real(rkp), dimension(:,:),  intent(in)                              ::    ParamChain
  real(rkp), dimension(:,:),  intent(in)                              ::    MiscChain
  real(rkp),dimension(:), optional, intent(in)                        ::    PosteriorChain
  character(*), intent(in)                                            ::    Directory

  character(*), parameter                                             ::    ProcName='WriteOutput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type)                                             ::    Input
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  logical                                                             ::    SilentLoc
  type(SMUQFile_Type)                                                 ::    File
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Names 

  if (len_trim(Directory) /= 0) then

    call MakeDirectory(Path=Directory, Options='-p')

    SilentLoc = This%Silent

    if (.not. SilentLoc) then
      write(*,'(A)') ''
      write(*,'(A)') 'Writing Bayesian inference data to the output folder'
    end if

    PrefixLoc = Directory

    allocate(Names(SampleSpace%GetNbDim()), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Names', ProcName=ProcName, stat=StatLoc)
    call SampleSpace%GetNames(Names=Names)
    FileName = 'parameter_names.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=Names, File=File)
    deallocate(Names, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Names', ProcName=ProcName, stat=StatLoc)

    FileName = 'prior_chain.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=MiscChain(1,:), File=File)

    FileName = 'likelihood_chain.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=MiscChain(2,:), File=File)

    FileName = 'parameter_chain.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=ParamChain, File=File)

    FileName = 'posterior_chain.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=PosteriorChain, File=File)

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(BayesInvMCMC_Type), intent(out)                               ::    LHS
  class(BayesInvMethod_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (BayesInvMCMC_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%DebugStop = RHS%DebugStop
        LHS%Hierarchical = RHS%Hierarchical
        allocate(LHS%MCMC, source=RHS%MCMC, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%MCMC', ProcName=ProcName, stat=StatLoc)
        if (LHS%Hierarchical) then
          LHS%HierarchicalSpace = RHS%HierarchicalSpace
          LHS%HierarchicalSampler = RHS%HierarchicalSampler
        end if
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(BayesInvMCMC_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%MCMC)) deallocate(This%MCMC, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%MCMC', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------


end module
