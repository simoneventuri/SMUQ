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

module MCMCDRAM_Class

use Input_Library
use Parameters_Library
use CommandRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringRoutines_Module
use String_Library
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MHProposalMethod_Class                                        ,only:    MHProposalMethod_Type
use MHProposalMethod_Factory_Class                                ,only:    MHProposalMethod_Factory
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use InputDet_Class                                                ,only:    InputDet_Type
use MCMCMethod_Class
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use Restart_Class                                                 ,only:    RestartUtility

implicit none

private

public                                                                ::    MCMCDRAM_Type

type, extends(MCMCMethod_Type)                                        ::    MCMCDRAM_Type
  real(rkp), allocatable, dimension(:)                                ::    TargetChain
  real(rkp), allocatable, dimension(:,:)                              ::    ParameterChain
  real(rkp), allocatable, dimension(:,:)                              ::    MiscChain
  integer                                                             ::    Step=0
  character(:), allocatable                                           ::    ProposalType
  integer                                                             ::    FilterFreq
  integer                                                             ::    BurnIn
  integer                                                             ::    ChainLength
  real(rkp), allocatable, dimension(:)                                ::    IniMu
  real(rkp), allocatable, dimension(:,:)                              ::    IniCov
  real(rkp), allocatable, dimension(:,:)                              ::    Cov
  real(rkp), allocatable, dimension(:,:)                              ::    L
  real(rkp), allocatable, dimension(:,:)                              ::    StartCov
  real(rkp), allocatable, dimension(:)                                ::    StartMu
  integer                                                             ::    NbSteps_DR
  real(rkp)                                                           ::    Factor_DR
  integer, allocatable, dimension(:)                                  ::    Accepted_DR
  integer, allocatable, dimension(:)                                  ::    Step_DR
  integer                                                             ::    UpdateFreq_AM
  integer                                                             ::    BurnIn_AM
  real(rkp)                                                           ::    Epsilon_AM
  type(RandPseudo_Type)                                               ::    RNG
  integer                                                             ::    Accepted
  integer                                                             ::    AcceptedPostBurnIn
  integer                                                             ::    CheckpointFreq
  logical                                                             ::    Silent
  real(rkp)                                                           ::    StartThreshold=1.0D-200
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GenerateChain
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'MCMCDRAM'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%IniMu) ) deallocate(This%IniMu, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IniMu', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%IniCov) ) deallocate(This%IniCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IniCov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TargetChain) ) deallocate(This%TargetChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TargetChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParameterChain) ) deallocate(This%ParameterChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParameterChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Step_DR) ) deallocate(This%Step_DR, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Step_DR', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Accepted_DR) ) deallocate(This%Accepted_DR, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Accepted_DR', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MiscChain) ) deallocate(This%MiscChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MiscChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Cov) ) deallocate(This%Cov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StartCov) ) deallocate(This%StartCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartCov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StartMu) ) deallocate(This%StartMu, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartMu', ProcName=ProcName, stat=StatLoc )

    call This%RNG%Reset()

    This%Step = 0
    This%Accepted = 0
    This%AcceptedPostBurnIn = 0

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%SectionChain = ''


    This%NbSteps_DR = 3
    This%Factor_DR = 0.5
    This%UpdateFreq_AM = 5
    This%BurnIn_AM = 100
    This%Epsilon_AM = 1.0E-30
    This%FilterFreq = 1
    This%BurnIn = 0
    This%ChainLength = 0
    This%CheckpointFreq = -1
    This%Silent = .false.
    This%Accepted = 0
    This%AcceptedPostBurnIn = 0
    This%StartThreshold = 1.0D-200

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix, Debug )

    use StringRoutines_Module

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    logical                                                           ::    Found
    integer                                                           ::    NbResponses
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName= 'silent'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Silent=VarL0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    ParameterName = "start_threshold"
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%StartThreshold = VarR0D

    ParameterName = 'initial_start'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%IniMu = ConvertToReals( String=VarC0D )
      VarR1D = ConvertToReals( String=VarC0D )
    end if

    SectionName = 'initial_covariance'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'format'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      SubSectionName = SectionName // '>format'
      select case ( LowerCase(VarC0D) )
        case ( 'diagonals' )
          ParameterName = 'values'
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          VarR1D = ConvertToReals( String=VarC0D )
          allocate(This%IniCov(size(VarR1D,1),size(VarR1D,1)), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%IniCov', ProcName=ProcName, stat=StatLoc )
          This%IniCov = Zero
          i = 1
          do i = 1, size(VarR1D,1)
            This%IniCov(i,i) = VarR1D(i)
          end do
        case ('source' )
          call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
          call ImportArray( Input=InputSection, Array=This%IniCov, Prefix=PrefixLoc )
          allocate(This%IniCov, source=VarR2D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%IniCov', ProcName=ProcName, stat=StatLoc )
        case default
          call Error%Raise( Line='Specified unknown format for covariance matrix', ProcName=ProcName )
      end select
    end if

    SectionName = 'dr'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'nb_attempts'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%NbSteps_DR = VarI0D

      ParameterName = 'factor'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%Factor_DR = VarR0D
    end if

    allocate(This%Accepted_DR(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Accepted_DR', ProcName=ProcName, stat=StatLoc )
    This%Accepted_DR = 0    

    allocate(This%Step_DR(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Step_DR', ProcName=ProcName, stat=StatLoc )
    This%Step_DR = 0

    SectionName = 'am'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'burn_in_length'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%BurnIn_AM = VarI0D

      if ( This%BurnIn_AM > 0 .and. This%BurnIn_AM <= 5 ) call Error%Raise( Line='AM Burn in must be above 5', ProcName=ProcName )

      ParameterName = 'update_frequency'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%UpdateFreq_AM = VarI0D
    end if

    ParameterName = 'chain_length'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%ChainLength = VarI0D

    ParameterName = 'filter_frequency'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%FilterFreq = VarI0D

    ParameterName = 'burn_in_length'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BurnIn = VarI0D

    SectionName = 'rng'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if ( Found ) then
      call This%RNG%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    else
      call This%RNG%Construct()
    end if

    SectionName = 'preload'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then

      ParameterName = 'nb_accepted'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Accepted = VarI0D

      ParameterName = 'nb_accepted_post_burn_in'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%AcceptedPostBurnIn = VarI0D

      ParameterName = 'nb_accepted_dr'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Accepted_DR = ConvertToIntegers( String=VarC0D )

      ParameterName = 'nb_steps_dr'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%Step_DR = ConvertToIntegers( String=VarC0D )

      SubSectionName = SectionName // '>parameter_chain'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
      allocate(This%ParameterChain(size(VarR2D,1),This%ChainLength), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParameterChain', ProcName=ProcName, stat=StatLoc )
      if ( size(VarR2D,2) > This%ChainLength ) call Error%Raise( Line='Incompatible parameter chain history', ProcName=ProcName )
      This%ParameterChain = Zero
      This%Step = size(VarR2D,2)
      This%ParameterChain(:,1:size(VarR2D,2)) = VarR2D
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )    
      
      SubSectionName = SectionName // '>target_chain'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
      allocate(This%TargetChain(This%ChainLength), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TargetChain', ProcName=ProcName, stat=StatLoc )
      if ( size(VarR1D,1) /= This%Step ) call Error%Raise( Line='Incompatible target chain history', ProcName=ProcName )
      This%TargetChain = Zero
      This%TargetChain(1:This%Step) = VarR1D
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

      SubSectionName = SectionName // '>misc_chain'
      if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
        allocate(This%MiscChain(size(VarR2D,1),This%ChainLength), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MiscChain', ProcName=ProcName, stat=StatLoc )
        if ( size(VarR2D,2) /= This%Step ) call Error%Raise( Line='Incompatible misc chain history', ProcName=ProcName )
        This%MiscChain = Zero
        This%MiscChain(:,1:This%Step) = VarR2D
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      end if

      SubSectionName = SectionName // '>covariance'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Cov, Prefix=PrefixLoc )

      SubSectionName = SectionName // '>cholesky'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%L, Prefix=PrefixLoc )

      SubSectionName = SectionName // '>start_covariance'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%StartCov, Prefix=PrefixLoc )

      SubSectionName = SectionName // '>start_mu'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%StartMu, Prefix=PrefixLoc )

    end if

    ParameterName = 'proposal'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%ProposalType = VarC0D

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(MCMCDRAM_Type), intent(in)                                  ::    This
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
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    ExternalFlag = .false.
    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%AddParameter( Name='silent', Value=ConvertToString(Value=This%Silent ) )
    call GetInput%AddParameter( Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq ) )
    call GetInput%AddParameter( Name='start_threshold', Value=ConvertToString(Value=This%StartThreshold ) )
    call GetInput%AddParameter( Name='filter_frequency', Value=ConvertToString(Value=This%FilterFreq ) )
    call GetInput%AddParameter( Name='chain_length', Value=ConvertToString(Value=This%ChainLength ) )
    call GetInput%AddParameter( Name='burn_in_length', Value=ConvertToString(Value=This%BurnIn ) )
    call GetInput%AddParameter( Name='proposal_type', Value=This%ProposalType )

    SectionName = 'dr'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='nb_attempts', Value=ConvertToString(Value=This%NbSteps_DR ), SectionName=SectionName )
    call GetInput%AddParameter( Name='factor', Value=ConvertToString(Value=This%Factor_DR ), SectionName=SectionName )

    SectionName = 'am'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='update_frequency', Value=ConvertToString(Value=This%UpdateFreq_AM), SectionName=SectionName)
    call GetInput%AddParameter( Name='burn_in_length', Value=ConvertToString(Value=This%BurnIn_AM ), SectionName=SectionName )

    if ( allocated(This%IniMu) ) then
      call GetInput%AddParameter( Name='initial_start', Value=ConvertToString(Values=This%IniMu) )
    end if

    if ( allocated(This%IniCov) ) then
      SectionName = 'initial_covariance'
      call GetInput%AddParameter( Name='format', Value='source', SectionName=SectionName )
      SubSectionName = 'format'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      FileName = DirectoryLoc // '/initial_covariance.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%IniCov, File=File )
      nullify(InputSection)
    end if

    SectionName = 'rng'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection( Section=This%RNG%GetInput( MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

    if ( This%Step > 0 ) then
      SectionName = 'preload'
      call GetInput%AddSection( SectionName=SectionName )

      call GetInput%AddParameter( Name='nb_accepted', Value=ConvertToString(Value=This%Accepted), SectionName=SectionName )
      call GetInput%AddParameter( Name='nb_accepted_post_burn_in', Value=ConvertToString(Value=This%Accepted),                    &
                                                                                                         SectionName=SectionName )
      call GetInput%AddParameter( Name='nb_accepted_dr', Value=ConvertToString(Values=This%Accepted_DR), SectionName=SectionName )
      call GetInput%AddParameter( Name='nb_steps_dr', Value=ConvertToString(Values=This%Step_DR), SectionName=SectionName )

      SubSectionName = 'parameter_chain'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/parameter_chain.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%ParameterChain, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%ParameterChain )
      end if
      nullify(InputSection)

      SubSectionName = 'target_chain'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,         &
                                                                                                              Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/target_chain.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%TargetChain, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%TargetChain )
      end if
      nullify(InputSection)

      if ( allocated(This%MiscChain) ) then
        SubSectionName = 'misc_chain'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,         &
                                                                                                                Mandatory=.true. )
        if ( ExternalFlag ) then
          FileName = DirectoryLoc // '/misc_chain.dat'
          call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
          call ExportArray( Input=InputSection, Array=This%MiscChain, File=File )
        else
          call ExportArray( Input=InputSection, Array=This%ParameterChain )
        end if
        nullify(InputSection)
      end if

      SubSectionName = 'covariance'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/covariance.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%Cov, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%Cov )
      end if
      nullify(InputSection)

      SubSectionName = 'cholesky'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/cholesky.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%L, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%L )
      end if
      nullify(InputSection)

      SubSectionName = 'start_covariance'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/start_covariance.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%StartCov, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%StartCov )
      end if
      nullify(InputSection)

      SubSectionName = 'start_mu'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )      
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/start_mu.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%StartMu, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%StartMu )
      end if
      nullify(InputSection)

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Delayed Rejection schemes for efficient Markov chainMonte Carlo sampling of multimodal distributions
  subroutine GenerateChain( This, SamplingTarget, SampleSpace, ParameterChain, TargetChain, MiscChain, OutputDirectory, Debug )

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    procedure(MCMCSamplingTarget), pointer                            ::    SamplingTarget
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    character(*), optional, intent(in)                                ::    OutputDirectory
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    ParameterChain
    real(rkp), allocatable, dimension(:), optional, intent(out)       ::    TargetChain
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    MiscChain
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbDim
    real(rkp), allocatable, dimension(:,:)                            ::    LLoc
    real(rkp), allocatable, dimension(:,:)                            ::    CovLoc
    real(rkp), allocatable, dimension(:)                              ::    SpaceSample
    real(rkp), allocatable, dimension(:)                              ::    PVec
    real(rkp)                                                         ::    VarR0D
    real(rkp), dimension(:), allocatable                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    type(DistNorm_Type)                                               ::    DistNormal
    class(DistProb_Type), pointer                                     ::    DistProbPointer=>null()
    integer                                                           ::    i
    integer                                                           ::    ii
    real(rkp)                                                         ::    Factor_DRLoc
    real(rkp), allocatable, dimension(:,:)                            ::    DRSamples
    real(rkp), allocatable, target, dimension(:,:)                    ::    DRSamplesBack
    class(MHProposalMethod_Type), allocatable                         ::    ProposalLoc
    real(rkp)                                                         ::    Alpha
    real(rkp)                                                         ::    N
    real(rkp), allocatable, dimension(:)                              ::    D
    real(rkp), allocatable, dimension(:)                              ::    QForw
    real(rkp), allocatable, dimension(:)                              ::    QBack
    real(rkp), allocatable, dimension(:)                              ::    DRTransProb
    real(rkp), allocatable, dimension(:)                              ::    DRCompTransProb
    real(rkp)                                                         ::    CurrentTarget
    real(rkp), allocatable, dimension(:)                              ::    ProposedTarget
    integer                                                           ::    LocIndex
    real(rkp), allocatable, dimension(:)                              ::    Mean
    real(rkp), allocatable, dimension(:,:)                            ::    MeanXnXnT
    real(rkp), allocatable, dimension(:,:)                            ::    XnXnT
    real(rkp)                                                         ::    sd_AM
    real(rkp)                                                         ::    StepReal
    real(rkp)                                                         ::    FreqReal
    integer                                                           ::    Nm1_AM
    integer                                                           ::    NbClean
    character(:), allocatable                                         ::    Line
    logical                                                           ::    AcceptedFlag
    type(InputDet_Type)                                               ::    Input
    real(rkp)                                                         ::    TransitionProb
    type(String_Type), allocatable, dimension(:)                      ::    Labels
    real(rkp), allocatable, dimension(:)                              ::    MiscValues
    logical                                                           ::    MiscValuesFlag
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    MiscValuesFlag = .false.

    NbDim = SampleSpace%GetNbDim()

    allocate(Labels(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Labels', ProcName=ProcName, stat=StatLoc )
    i = 1
    do i = 1, NbDim
      Labels(i) = SampleSpace%GetLabel(Num=i)
    end do

    allocate(SpaceSample(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceSample', ProcName=ProcName, stat=StatLoc )
    SpaceSample = Zero

    allocate(This%Cov(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cov', ProcName=ProcName, stat=StatLoc )
    This%Cov = Zero

    allocate(CovLoc(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CovLoc', ProcName=ProcName, stat=StatLoc )
    CovLoc = Zero

    allocate(This%L(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%L', ProcName=ProcName, stat=StatLoc )
    This%L = Zero

    allocate(LLoc(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='L', ProcName=ProcName, stat=StatLoc )
    LLoc = Zero

    if ( .not. allocated(This%StartCov) ) then
      allocate(This%StartCov(NbDim,NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%StartCov', ProcName=ProcName, stat=StatLoc )
      This%StartCov = Zero
    end if

    if ( .not. allocated(This%StartMu) ) then
      allocate(This%StartMu(NbDim), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%StartMu', ProcName=ProcName, stat=StatLoc )
      This%StartMu = Zero
    end if

    allocate(XnXnT(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='XnXnT', ProcName=ProcName, stat=StatLoc )
    XnXnT = Zero

    allocate(MeanXnXnT(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='MeanXnXnT', ProcName=ProcName, stat=StatLoc )
    MeanXnXnT = Zero

    allocate(Mean(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Mean', ProcName=ProcName, stat=StatLoc )
    Mean = Zero

    allocate(ProposedTarget(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ProposedTarget', ProcName=ProcName, stat=StatLoc )
    ProposedTarget = Zero

    Nm1_AM = 0

    if ( .not. allocated(ParameterChain) ) then
      allocate(This%ParameterChain(NbDim,This%ChainLength), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParameterChain', ProcName=ProcName, stat=StatLoc )
      This%ParameterChain = Zero
    end if

    if ( .not. allocated(TargetChain) ) then
      allocate(This%TargetChain(This%ChainLength), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TargetChain', ProcName=ProcName, stat=StatLoc )
      This%TargetChain = Zero
    end if

    allocate(DRSamples(NbDim,This%NbSteps_DR+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DRSamples', ProcName=ProcName, stat=StatLoc )
    DRSamples = Zero

    allocate(DRSamplesBack(NbDim,This%NbSteps_DR+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DRSamplesBack', ProcName=ProcName, stat=StatLoc )
    DRSamplesBack = Zero

    allocate(PVec(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='PVec', ProcName=ProcName, stat=StatLoc )
    PVec = Zero

    call DistNormal%Construct(Mu=Zero, Sigma=One)

    call MHProposalMethod_Factory%Construct( Object=ProposalLoc, DesiredType=This%ProposalType )

    allocate(DRTransProb(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DRProbMat', ProcName=ProcName, stat=StatLoc )
    DRTransProb = Zero

    allocate(DRCompTransProb(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DRCompProbMat', ProcName=ProcName, stat=StatLoc )
    DRCompTransProb = Zero

    N = One

    allocate(D(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='D', ProcName=ProcName, stat=StatLoc )
    D = One

    allocate(QForw(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='QForw', ProcName=ProcName, stat=StatLoc )
    QForw = Zero

    allocate(QBack(This%NbSteps_DR), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='QBack', ProcName=ProcName, stat=StatLoc )
    QBack = Zero

    allocate(VarR1D(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    allocate(VarR2D(NbDim,NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
    VarR2D = Zero

    sd_AM = 2.4**2 / real(NbDim,rkp)

    VarR0D = 0

    do

      if ( This%Step == 0 ) then
        This%Accepted = 0
        This%Accepted_DR = 0
        This%Step_DR = 0
        This%AcceptedPostBurnIn = 0
        This%TargetChain = Zero
        This%ParameterChain = Zero
        if ( allocated(This%MiscChain) ) This%MiscChain = Zero

        if ( .not. allocated(This%IniCov) ) then
          do i = 1, NbDim
            DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)
            This%Cov(i,i) = DistProbPointer%GetVariance()
          end do
        else
          if ( size(This%IniCov,1) /= size(This%IniCov,2) .or. size(This%IniCov,1) /= NbDim ) call Error%Raise(                   &
                                                                      Line='Invalid initial covariance array', ProcName=ProcName )  
          This%Cov = This%IniCov
        end if

        if ( .not. allocated(This%IniMu) ) then
          SpaceSample = This%RNG%DrawVec( Size1=NbDim )
          i = 1
          do i = 1, NbDim
            DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)
            SpaceSample(i) = DistProbPointer%InvCDF(P=SpaceSample(i))
          end do
        else
          if ( size(This%IniMu,1) /= NbDim ) call Error%Raise( Line='Invalid initial starting point', ProcName=ProcName )
          SpaceSample = This%IniMu
        end if

        LLoc = This%Cov
        call DPOTRF( 'U', NbDim, LLoc, NbDim, StatLoc )
        if ( StatLoc /= 0 ) call Error%Raise( Line='Error in DPOTRF of initial covariance array with code: ' //                   &
                                                                               ConvertToString(Value=StatLoc), ProcName=ProcName )
        This%L = LLoc
        ii = 1
        do ii = 1, NbDim
          This%L(ii,1:ii) = This%L(1:ii,ii)
        end do

        call Input%Construct( Input=SpaceSample, Labels=Labels )
        call SamplingTarget( Input=Input, Value=CurrentTarget, MiscValues=MiscValues )
        if ( CurrentTarget <= This%StartThreshold ) then
          if ( .not. This%Silent ) then
            write(*,*) ''
            write(*,'(A)') 'Initial starting point yielded probability value of zero. Resampling.' 
          end if
          ii = 0
          do
            ii = ii + 1
            if ( .not. This%Silent ) then
              write(*,*) ''
              write(*,'(A)') 'Resampling attempt #' // ConvertToString(Value=ii) 
            end if
            SpaceSample = This%RNG%DrawVec( Size1=NbDim )
            i = 1
            do i = 1, NbDim
              DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)
              SpaceSample(i) = DistProbPointer%InvCDF(P=SpaceSample(i))
            end do

            call Input%Construct( Input=SpaceSample, Labels=Labels )
            call SamplingTarget( Input=Input, Value=CurrentTarget, MiscValues=MiscValues )
            if ( CurrentTarget > This%StartThreshold ) exit
          end do
        end if

        This%ParameterChain(:,1) = SpaceSample
        This%TargetChain(1) = CurrentTarget

        if ( (.not. allocated(This%MiscChain)) .and. allocated(MiscValues) ) then
          allocate(This%MiscChain(size(MiscValues),This%ChainLength), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MiscChain', ProcName=ProcName, stat=StatLoc )
          MiscValuesFlag = .true.
          This%MiscChain(:,1) = MiscValues
        end if
        
        This%StartCov = This%Cov
        This%StartMu = SpaceSample

        This%Step = 1
        This%Accepted = 1
        This%Accepted_DR(1) = 1
        This%Step_DR(1) = 1
        if ( This%Step >= This%BurnIn ) This%AcceptedPostBurnIn = This%AcceptedPostBurnIn + 1
      end if

      This%Step = This%Step + 1

      !***************************************************************************************************************************
      !  DR BLOCK
      !***************************************************************************************************************************

      DRSamples(:,1) = SpaceSample
      DRSamplesBack(:,This%NbSteps_DR+1) = SpaceSample

      AcceptedFlag = .false.

      i = 1
      do i = 1, This%NbSteps_DR

        This%Step_DR(i) = This%Step_DR(i) + 1

        Factor_DRLoc = This%Factor_DR**(i-1)
        CovLoc = This%Cov * Factor_DRLoc
        LLoc = This%L * dsqrt(Factor_DRLoc)

        call ProposalLoc%Construct( X=DRSamples(:,1:i), Cov=CovLoc )

        call ProposalLoc%GetMu( Mu=VarR1D )
        PVec = This%RNG%DrawVec( Size1=NbDim )
        ii = 1
        do ii = 1, NbDim
          PVec(ii) = DistNormal%InvCDF(P=PVec(ii))
          SpaceSample(ii) = dot_product(PVec(1:ii),LLoc(1:ii,ii)) + VarR1D(ii)
        end do
        DRSamples(:,i+1) = SpaceSample
        call Input%Construct( Input=SpaceSample, Labels=Labels )
        call SamplingTarget( Input=Input, Value=ProposedTarget(i), MiscValues=MiscValues )

        DRSamplesBack(:,This%NbSteps_DR+1-i) = SpaceSample

        if ( i > 1 ) then
          D(i) = ProposedTarget(i-1)
        else
          D(i) = CurrentTarget
        end if

        ii = 1
        do ii = 1, i
          LocIndex = i-ii+1
          Factor_DRLoc = This%Factor_DR**(ii-1)
          CovLoc = This%Cov * Factor_DRLoc
          LLoc = This%L * dsqrt(Factor_DRLoc)
          call ProposalLoc%Construct( X=DRSamplesBack(:,This%NbSteps_DR+1-i:This%NbSteps_DR-i+ii), Cov=CovLoc )
          QBack(LocIndex) = ProposalLoc%PDF( X=DRSamplesBack(:,This%NbSteps_DR-i+ii+1), L=LLoc )
          call ProposalLoc%Construct( X=DRSamples(:,LocIndex:i), Cov=CovLoc )
          QForw(LocIndex) = ProposalLoc%PDF( X=SpaceSample, L=LLoc )
        end do

        ii = i
        do ii = i, 1, -1
          if ( ii < i ) then
            D(ii) = D(ii) * QForw(ii) * ( One - DRTransProb(ii) )
            N = ProposedTarget(i) * product(QBack(ii:i)) * product(DRCompTransProb(ii+1:i))
          else
            D(ii) = D(ii) * QForw(ii)
            N = ProposedTarget(i) * QBack(ii)
          end if

          if ( D(ii) < Zero ) call Error%Raise( Line='Negative Denominator in DRAM DR stage', ProcName=ProcName )
          if ( N < Zero ) call Error%Raise( Line='Negative Numerator in DRAM DR stage', ProcName=ProcName )

          if ( D(ii) > Zero .and. N > Zero ) then
            DRTransProb(ii) = N/D(ii)
            if ( DRTransProb(ii) < One ) then
              DRCompTransProb(ii) = Zero
            else
              DRTransProb(ii) = One
              DRCompTransProb(ii) = One - D(ii)/N
            end if
          elseif ( N > Zero ) then
            DRTransProb(ii) = One
            DRCompTransProb(ii) = One
          elseif ( D(ii) > Zero ) then
            DRTransProb(ii) = Zero
            DRCompTransProb(ii) = Zero
          else
            DRTransProb(ii) = One
            DRCompTransProb(ii) = One
          end if
        end do

        TransitionProb = DRTransProb(1)
        Alpha = This%RNG%Draw()

        if ( Alpha <= TransitionProb  ) AcceptedFlag = .true.

        if ( AcceptedFlag ) then
          This%Accepted = This%Accepted + 1
          This%Accepted_DR(i) = This%Accepted_DR(i) + 1
          if ( This%Step >= This%BurnIn ) This%AcceptedPostBurnIn = This%AcceptedPostBurnIn + 1
        end if

        if ( .not. This%Silent ) then
          write(*,*) ''
          write(*,'(A)') 'MC Step = ' // ConvertToString(Value=This%Step)
          write(*,'(A)') 'Acceptance Percent = ' // ConvertToString(Value=real(This%Accepted,rkp)/real(This%Step,rkp)*100.0,      &
                                                                                                            Format='F6.2') // ' %'
          if ( This%Step <= This%BurnIn ) then
            write(*,'(A)') 'Acceptance Percent Post Burn-In = N/A' 
          else
            write(*,'(A)') 'Acceptance Percent Post Burn-In = ' // ConvertToString(Value=real(This%AcceptedPostBurnIn,rkp)/       &
                                                                 real(This%Step-(This%BurnIn-1),rkp)*100.0, Format='F6.2') // ' %'
          end if

          Line = 'DR step = ' // ConvertToString(Value=i) // ', DR Parameter = ' // ConvertToString(Value=Factor_DRLoc)
          write(*,'(A)') Line

          write(*,'(A)') 'Current Target = ' // ConvertToString(Value=This%TargetChain(This%Step-1)) 
          write(*,'(A)') 'Proposed Target = ' // ConvertToString(Value=ProposedTarget(i)) 
          write(*,'(A)') 'Transition Probability = ' // ConvertToString(Value=TransitionProb)

          Line = 'Current Sample : '
          write(*,'(A)') Line
          ii = 1
          do ii = 1, NbDim
            write(*,'(3X,A," = ", A)') SampleSpace%GetLabel(Num=ii), ConvertToString(Value=This%ParameterChain(ii,This%Step-1))
          end do

          Line = 'Proposed Sample : '
          write(*,'(A)') Line
          ii = 1
          do ii = 1, NbDim
            write(*,'(3X,A," = ", A)') SampleSpace%GetLabel(Num=ii), ConvertToString(Value=SpaceSample(ii))
          end do

          Line = 'Action = FAILED'
          if ( AcceptedFlag ) Line = 'Action = ACCEPTED'
          write(*,'(A)') Line

        end if

        if ( AcceptedFlag ) then
          CurrentTarget = ProposedTarget(i)
          exit
        end if

      end do

      if ( AcceptedFlag ) then
        This%ParameterChain(:,This%Step) = SpaceSample
        This%TargetChain(This%Step) = CurrentTarget
        if ( MiscValuesFlag ) This%MiscChain(:,This%Step) = MiscValues
      else
        This%ParameterChain(:,This%Step) = This%ParameterChain(:,This%Step-1)
        SpaceSample = This%ParameterChain(:,This%Step)
        This%TargetChain(This%Step) = This%TargetChain(This%Step-1)
        if ( MiscValuesFlag ) This%MiscChain(:,This%Step) = This%MiscChain(:,This%Step-1)
      end if

      if ( This%Step >= This%ChainLength ) exit

      DRSamples = Zero
      DRSamplesBack = Zero
      QBack = Zero
      QForw = Zero
      ProposedTarget = Zero
      D = One
      N = One
      DRTransProb = Zero
      DRCompTransProb = Zero

      !***************************************************************************************************************************

      !***************************************************************************************************************************
      !  AM BLOCK
      !***************************************************************************************************************************

      if ( This%UpdateFreq_AM > 0 .and. This%Burnin_AM >= 0 ) then

        ! checking if procedure was restarted
        if ( This%Step > This%BurnIn_AM .and. Nm1_AM == 0 ) then
          VarR2D = Zero
          Nm1_AM = This%Step-mod(This%Step-This%Burnin_AM,This%UpdateFreq_AM)
          Mean = sum(This%ParameterChain(:,1:Nm1_AM),2) / real(Nm1_AM,rkp)
          call DGER( NbDim, NbDim, Zero, Mean, 1, Mean, 1, MeanXnXnT, NbDim )
          i = 1
          do i = Nm1_AM+1, This%Step
            call DGEMM ('N', 'T', NbDim, NbDim, 1, 1.0, This%ParameterChain(:,i), NbDim,                                          &
                                                                              This%ParameterChain(:,i), NbDim, 1.0, VarR2D, NbDim)
            XnXnT = XnXnT + VarR2D
          end do
        end if

        if ( This%Step == 2 ) call DGER( NbDim, NbDim, One, This%ParameterChain(:,This%Step-1), 1,                                &
                                                                             This%ParameterChain(:,This%Step-1), 1, XnXnT, NbDim )

        call DGER( NbDim, NbDim, One, This%ParameterChain(:,This%Step), 1, This%ParameterChain(:,This%Step), 1, XnXnT, NbDim )

        if ( This%Step >= This%Burnin_AM .and. (This%Step == This%Burnin_AM .or. This%Step-Nm1_AM == This%UpdateFreq_AM) ) then
          if ( .not. This%Silent ) then
            write(*,*) ''
            write(*,'(A)') 'Adapting Proposal'
          end if
          StepReal = real(This%Step,rkp)
          FreqReal = real(This%UpdateFreq_AM,rkp)
          if ( This%Step > This%Burnin_AM ) then
            Mean = Mean*(real(Nm1_AM,rkp)/StepReal)+sum(This%ParameterChain(:,Nm1_AM+1:This%Step),2)/StepReal
            CovLoc = This%Cov
            CovLoc = CovLoc*((StepReal-FreqReal-One)/(StepReal-One))
            VarR0D = sd_AM/(StepReal-One)
            call Eye(Array=VarR2D)
            CovLoc = CovLoc + VarR0D*(XnXnT + (StepReal-FreqReal)*MeanXnXnT + FreqReal*This%Epsilon_AM*VarR2D)
            MeanXnXnT = Zero
            call DGER( NbDim, NbDim, One, Mean, 1, Mean, 1, MeanXnXnT, NbDim )
            CovLoc = CovLoc - VarR0D*( StepReal*MeanXnXnT )
          else
            CovLoc = Zero
            VarR2D = Zero
            MeanXnXnT = Zero
            Mean = Zero
            Mean = sum(This%ParameterChain(:,1:This%Step),2)/StepReal
            call DGER( NbDim, NbDim, One, Mean, 1, Mean, 1, MeanXnXnT, NbDim)
            CovLoc = sd_AM/(StepReal-One)*( XnXnT - StepReal*MeanXnXnT)
            call Eye(Array=VarR2D)
            CovLoc = CovLoc + sd_AM*This%Epsilon_AM*VarR2D
          end if
          Nm1_AM = This%Step
          This%Cov = CovLoc

          LLoc = CovLoc
          call DPOTRF( 'U', NbDim, LLoc, NbDim, StatLoc )
          if ( StatLoc /= 0 ) then
            if ( This%Step == This%BurnIn_AM ) then
              This%Step = 0
              Nm1_AM = 0
              Mean = Zero
              XnXnT = Zero
              MeanXnXnT = Zero
              StatLoc = 0
              write(*,'(A)') 'Restarting chain due to initial adaptation of the covariance matrix being non-positive definite'
            else
              call Error%Raise( Line='Error in DPOTRF with code: ' // ConvertToString(Value=StatLoc), ProcName=ProcName )
            end if
          else
            This%L = LLoc
            ii = 1
            do ii = 1, NbDIm
              This%L(ii,1:ii) = This%L(1:ii,ii)
            end do
          end if

          XnXnT = Zero

        end if

      end if
      
      !***************************************************************************************************************************

      if ( mod(This%Step,This%CheckpointFreq) == 0 .and. This%CheckpointFreq > 0 ) then
        call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),         &
                        Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
      end if

    end do

    if ( This%CheckpointFreq > 0 )   call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp',               &
                        Prefix=RestartUtility%GetPrefix(), Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)),&
                                                                                                  SectionChain=This%SectionChain )

    if ( present(OutputDirectory) ) then
      call This%WriteOutput( Directory=OutputDirectory )
    end if
    
    NbClean = (This%ChainLength - This%Burnin) / This%FilterFreq + 1

    if ( present(ParameterChain) ) then
      allocate(ParameterChain(NbDim,NbClean), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='ParameterChain', ProcName=ProcName, stat=StatLoc )
      ParameterChain = Zero
      ii = 0
      i = 1
      do i = 1, This%ChainLength
        if ( i < This%Burnin ) cycle
        if ( mod(i-This%Burnin,This%FilterFreq) /= 0 ) cycle
        ii = ii + 1
        ParameterChain(:,ii) = This%ParameterChain(:,i)
      end do
    end if

    deallocate(This%ParameterChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParameterChain', ProcName=ProcName, stat=StatLoc )

    if ( present(TargetChain) ) then
      allocate(TargetChain(NbClean), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='TargetChain', ProcName=ProcName, stat=StatLoc )
      TargetChain = Zero
      ii = 0
      i = 1
      do i = 1, This%ChainLength
        if ( i < This%Burnin ) cycle
        if ( mod(i-This%Burnin,This%FilterFreq) /= 0 ) cycle
        ii = ii + 1
        TargetChain(ii) = This%TargetChain(i)
      end do
    end if

    deallocate(This%TargetChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TargetChain', ProcName=ProcName, stat=StatLoc )

    if ( present(MiscChain) ) then
      if ( .not. MiscValuesFlag ) call Error%Raise( Line='Requested miscvalues chain but none were provided', ProcName=ProcName )
      allocate(MiscChain(size(This%MiscChain,1),NbClean), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='MiscChain', ProcName=ProcName, stat=StatLoc )
      MiscChain = Zero
      ii = 0
      i = 1
      do i = 1, This%ChainLength
        if ( i < This%Burnin ) cycle
        if ( mod(i-This%Burnin,This%FilterFreq) /= 0 ) cycle
        ii = ii + 1
        MiscChain(:,ii) = This%MiscChain(:,i)
      end do
    end if

    if ( allocated(This%MiscChain) ) deallocate(This%MiscChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MiscChain', ProcName=ProcName, stat=StatLoc )

    This%Step = 0

    deallocate(SpaceSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SpaceSample', ProcName=ProcName, stat=StatLoc )

    deallocate(PVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PVec', ProcName=ProcName, stat=StatLoc )

    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

    deallocate(DRSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DRSamples', ProcName=ProcName, stat=StatLoc )

    deallocate(DRSamplesBack, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DRSamplesBack', ProcName=ProcName, stat=StatLoc )

    deallocate(D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='D', ProcName=ProcName, stat=StatLoc )

    deallocate(QForw, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='QForw', ProcName=ProcName, stat=StatLoc )

    deallocate(QBack, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='QBack', ProcName=ProcName, stat=StatLoc )

    deallocate(DRTransProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DRTransProb', ProcName=ProcName, stat=StatLoc )

    deallocate(DRCompTransProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DRCompTransProb', ProcName=ProcName, stat=StatLoc )

    deallocate(Mean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Mean', ProcName=ProcName, stat=StatLoc )

    deallocate(MeanXnXnT, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='MeanXnXnT', ProcName=ProcName, stat=StatLoc )

    deallocate(XnXnT, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='XnXnT', ProcName=ProcName, stat=StatLoc )

    deallocate(ProposedTarget, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ProposedTarget', ProcName=ProcName, stat=StatLoc )

    deallocate(This%Cov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cov', ProcName=ProcName, stat=StatLoc )

    deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Mu', ProcName=ProcName, stat=StatLoc )

    deallocate(This%StartCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartCov', ProcName=ProcName, stat=StatLoc )

    deallocate(This%StartMu, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartMu', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, Debug )

    class(MCMCDRAM_Type), intent(inout)                               ::    This
    character(*), intent(in)                                          ::    Directory
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='WriteOutput'
    integer                                                           ::    StatLoc
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
        write(*,'(A)') 'Writing MCMCDRAM sampler data to the output folder'
      end if

      PrefixLoc = Directory

      FileName = '/initial_start.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%StartMu, File=File )

      FileName = '/initial_covariance.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%StartCov, File=File )

      FileName = '/parameter_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%ParameterChain, File=File )

      FileName = '/target_chain.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%TargetChain, File=File )

      if ( allocated(This%MiscChain) ) then
        FileName = '/misc_chain.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=This%MiscChain, File=File )
      end if

      FileName = '/proposal_covariance.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%Cov, File=File )

      FileName = '/nb_accepted_post_burn_in.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call File%Export( String=ConvertToString(Value=This%AcceptedPostBurnIn) )

      FileName = '/nb_accepted_dr.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%Accepted_DR, File=File )

      FileName = '/nb_steps_dr.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%Step_DR, File=File )

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(MCMCDRAM_Type), intent(out)                                 ::    LHS
    class(MCMCMethod_Type), intent(in)                                ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (MCMCDRAM_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%SectionChain = RHS%SectionChain
          if ( RHS%Step > 0 ) then
            allocate(LHS%TargetChain, source=RHS%TargetChain, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TargetChain', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%ParameterChain, source=RHS%ParameterChain, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ParameterChain', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%Accepted_DR, source=RHS%Accepted_DR, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Accepted_DR', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%Step_DR, source=RHS%Step_DR, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Step_DR', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%Cov, source=RHS%Cov, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Cov', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%L, source=RHS%L, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%L', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%StartCov, source=RHS%StartCov, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%StartCov', ProcName=ProcName, stat=StatLoc )
            allocate(LHS%StartMu, source=RHS%StartMu, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%StartMu', ProcName=ProcName, stat=StatLoc )
            if ( allocated(RHS%MiscChain) ) then
              allocate(LHS%MiscChain, source=RHS%MiscChain, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MiscChain', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( allocated(RHS%IniMu) ) then
            allocate(LHS%IniMu, source=RHS%IniMu, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%IniMu', ProcName=ProcName, stat=StatLoc )
          end if
          if ( allocated(RHS%IniCov) ) then
            allocate(LHS%IniCov, source=RHS%IniCov, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%IniCov', ProcName=ProcName, stat=StatLoc )
          end if
          LHS%Step = RHS%Step
          LHS%FilterFreq = RHS%FilterFreq
          LHS%Burnin = RHS%Burnin
          LHS%ChainLength = RHS%ChainLength
          LHS%NbSteps_DR = RHS%NbSteps_DR
          LHS%Factor_DR = RHS%Factor_DR
          LHS%UpdateFreq_AM = RHS%UpdateFreq_AM
          LHS%BurnIn_AM = RHS%BurnIn_AM
          LHS%Epsilon_AM = RHS%Epsilon_AM
          LHS%RNG = RHS%RNG
          LHS%Accepted = RHS%Accepted
          LHS%AcceptedPostBurnIn = RHS%AcceptedPostBurnIn
          LHS%CheckpointFreq = RHS%CheckpointFreq
          LHS%Silent = RHS%Silent
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(MCMCDRAM_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%IniMu) ) deallocate(This%IniMu, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IniMu', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%IniCov) ) deallocate(This%IniCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IniCov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%TargetChain) ) deallocate(This%TargetChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TargetChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParameterChain) ) deallocate(This%ParameterChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParameterChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Step_DR) ) deallocate(This%Step_DR, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Step_DR', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Accepted_DR) ) deallocate(This%Accepted_DR, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Accepted_DR', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%MiscChain) ) deallocate(This%MiscChain, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MiscChain', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Cov) ) deallocate(This%Cov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%L) ) deallocate(This%L, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%L', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StartCov) ) deallocate(This%StartCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartCov', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%StartMu) ) deallocate(This%StartMu, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%StartMu', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
