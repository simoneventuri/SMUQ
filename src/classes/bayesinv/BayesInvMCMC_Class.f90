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
  class(LikelihoodFunction_Type), allocatable                         ::    LikelihoodFunction
  class(MCMCMethod_Type), allocatable                                 ::    MCMC
  type(SpaceHierParam_Type)                                           ::    HierarchicalSpace
  type(SpaceSampler_Type)                                             ::    HierarchicalSampler
  logical                                                             ::    Hierarchical
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

    if ( allocated(This%LikelihoodFunction) ) deallocate(This%LikelihoodFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LikelihoodFunction', ProcName=ProcName, stat=StatLoc )

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


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    SectionName = 'mcmc'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call MCMCMethod_Factory%Construct( Object=This%MCMC, Input=InputSection, SectionChain=SectionChain // '>mcmc',                &
                                                                                                                Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'likelihood'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call LikelihoodFunction_Factory%Construct( Object=This%LikelihoodFunction, Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

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

    SectionName = 'mcmc'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/mcmc'
    call GetInput%AddSection( Section=MCMCMethod_Factory%GetObjectInput(Object=This%MCMC, MainSectionName=SectionName,            &
                                                                                       Prefix=PrefixLoc, Directory=DirectorySub) )

    SectionName = 'likelihood'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/likelihood'
    call GetInput%AddSection( Section=LikelihoodFunction_Factory%GetObjectInput(Object=This%LikelihoodFunction,                   &
                                                          MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub) )

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
  subroutine Calibrate( This, Model, SpaceInput, Response, OutputDirectory, Debug )

    class(BayesInvMCMC_Type), intent(inout)                           ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Response
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
    real(rkp), allocatable, dimension(:,:)                            ::    HierSamples
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(SpaceParam_Type)                                             ::    SpaceParamRealization

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( SpaceInput%IsCorrelated() ) call Error%Raise( Line='Bayesian inference does not support correlated spaces',              &
                                                                                                               ProcName=ProcName )    

    if ( This%Hierarchical ) then
      Posterior => MCMCPosteriorHier
      allocate(Labels(This%HierarchicalSpace%GetNbDim()+SpaceInput%GetNbDim()), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Labels', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, size(Labels)
        if ( i <= SpaceInput%GetNbDim() ) then
          Labels(i) = SpaceInput%GetLabel(Num=i)
        else
          Labels(i) = This%HierarchicalSpace%GetLabel(Num=i-SpaceInput%GetNbDim())
        end if
      end do
    else
      Posterior => MCMCPosterior
    end if

    call ModelInterface%Construct( Model=Model, Response=Response )

    if ( present(OutputDirectory) ) OutputDirectoryLoc = OutputDirectory // '/posterior_sampler'

    call This%MCMC%GenerateChain( SamplingTarget=Posterior, SpaceInput=SpaceInput, ParameterChain=ParamChain,                     &
                                             TargetChain=PosteriorChain, MiscChain=MiscChain, OutputDirectory=OutputDirectoryLoc )

    deallocate(Output, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )

    if ( allocated(HierSamples) ) then
      deallocate(HierSamples, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='HierSamples', ProcName=ProcName, stat=StatLoc )
    end if

    if ( allocated(VarR2D) ) then
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
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
        real(rkp)                                                         ::    VarR0D
        integer                                                           ::    RunStat
        
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
        i = 1
        do i = 1, SpaceInput%GetNbDim()
          DistProb => SpaceInput%GetDistributionPointer( Num=i )
          call Input%GetValue( Value=VarR0D, Label=SpaceInput%GetLabel(Num=i) )
          Prior = Prior * DistProb%PDF( X=VarR0D )
        end do
        nullify( DistProb )

        MiscValues(1) = Prior

        if ( Prior > Zero ) then
          call ModelInterface%Run( Input=Input, Output=Output, Stat=RunStat )
          if ( RunStat == 0 ) then
            Likelihood = This%LikelihoodFunction%Evaluate( Response=Response, Input=Input, Output=Output )
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
        integer                                                           ::    i
        real(rkp)                                                         ::    Prior
        class(DistProb_Type), pointer                                     ::    DistProb
        type(InputStoch_Type)                                             ::    HierInput
        real(rkp)                                                         ::    VarR0D
        integer                                                           ::    RunStat
        real(rkp)                                                         ::    Likelihood

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
        i = 1
        do i = 1, SpaceInput%GetNbDim()
          DistProb => SpaceInput%GetDistributionPointer( Num=i )
          call Input%GetValue( Value=VarR0D, Label=SpaceInput%GetLabel(Num=i) )
          Prior = Prior * DistProb%PDF( X=VarR0D )
        end do
        nullify( DistProb )

        MiscValues(1) = Prior

        if ( Prior > Zero ) then

          call This%HierarchicalSpace%Generate( Input=Input, SpaceParam=SpaceParamRealization )

          HierSamples = This%HierarchicalSampler%Draw( SpaceInput=SpaceParamRealization )

          if ( allocated(VarR2D) ) then
            if ( size(VarR2D,1) /= SpaceParamRealization%GetNbDim()+SpaceInput%GetNbDim() .or.                                   &
                                                                                      size(VarR2D,2) /= size(HierSamples,2) ) then
              deallocate(VarR2D, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
            end if
          end if

          if ( .not. allocated(VarR2D) ) then
            allocate(VarR2D(SpaceParamRealization%GetNbDim()+SpaceInput%GetNbDim(),size(HierSamples,2)), stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
          end if

          i = 1
          do i = 1, SpaceInput%GetNbDim()
            call Input%GetValue( Value=VarR2D(i,1), Label=Labels(i)%GetValue() )
            VarR2D(i,:) = VarR2D(i,1)
          end do
          VarR2D(SpaceInput%GetNbDim()+1:,:) = HierSamples

          call HierInput%Construct( Input=VarR2D, Labels=Labels )

          call ModelInterface%Run( Input=HierInput, Output=Output, Stat=RunStat )

          if ( RunStat == 0 ) then
            Likelihood = This%LikelihoodFunction%Evaluate( Response=Response, Input=HierInput, Output=Output )
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
          allocate(LHS%LikelihoodFunction, source=RHS%LikelihoodFunction, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%LikelihoodFunction', ProcName=ProcName, stat=StatLoc )
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
  
    if ( allocated(This%LikelihoodFunction) ) deallocate(This%LikelihoodFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%LikelihoodFunction', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MCMC) ) deallocate(This%MCMC, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MCMC', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


end module
