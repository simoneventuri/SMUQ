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

module ModelExternal_Class

use Input_Library
use Parameters_Library
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use ModelExternalModel_Class                                      ,only:    ModelExternalModel_Type
use ParameterWriter_Class                                         ,only:    ParameterWriter_Type
use OutputReader_Class                                            ,only:    OutputReader_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use Input_Class                                                   ,only:    Input_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    ModelExternal_Type

type, extends(Model_Type)                                             ::    ModelExternal_Type
  type(ParameterWriter_Type), allocatable, dimension(:)               ::    ParameterWriter
  type(OutputReader_Type), allocatable, dimension(:)                  ::    OutputReader
  type(String_Type), allocatable, dimension(:)                        ::    SubModelCaseDirectory
  type(String_Type), allocatable, dimension(:)                        ::    SubModelRunCommand
  integer                                                             ::    NbSubModels
  integer                                                             ::    NbConcurrentEvaluations
  integer                                                             ::    NbConcurrentSubEvaluations
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run_0D
  procedure, public                                                   ::    Run_1D
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'ModelExternal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    call This%SetDefaults()

    call This%ParameterWriter%Reset()
    call This%OutputReader%Reset()

    if ( allocated(This%ParameterWriter) ) deallocate(This%ParameterWriter, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParameterWriter', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%OutputReader) ) deallocate(This%OutputReader, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputReader', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%SubModelCaseDirectory) ) deallocate(This%SubModelCaseDirectory, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SubModelCaseDirectory', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%SubModelRunCommand) ) deallocate(This%SubModelRunCommand, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SubModelRunCommand', ProcName=ProcName, stat=StatLoc )

    This%NbOutputs = 0
    This%NbSubModels = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%WorkDirectory = ''
    This%FullWorkDirectory = ''
    This%Label = 'external'
    This%NbConcurrentEvaluations = 1
    This%NbConcurrentSubEvaluations = 1


  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library

    class(ModelExternal_Type), intent(inout)                          ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    WorkDirectoryLoc
    logical                                                           ::    Found
    integer                                                           ::    i

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Label = VarC0D

    ParameterName = 'work_directory'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%WorkDirectory = VarC0D
    This%FullWorkDirectory = PrefixLoc // This%WorkDirectory

    ParameterName = 'nb_concurrent_evaluations'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%NbConcurrentEvaluations = VarI0D

    ParameterName = 'nb_concurrent_subevaluations'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%NbConcurrentEvaluations = VarI0D

    SectionName = 'submodels'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbSubModels = InputSection%GetNumberOfSubSections()
    nullify(InputSection)

    if ( This%NbConcurrentSubEvaluations > This%NbSubModels ) This%NbConcurrentSubEvaluations = This%NbSubModels

    allocate(This%ParameterWriter(This%NbSubModels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParameterWriter', ProcName=ProcName, stat=StatLoc )

    allocate(This%OutputReader(This%NbSubModels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OutputReader', ProcName=ProcName, stat=StatLoc )

    allocate(This%SubModelCaseDirectory(This%NbSubModels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%SubModelCaseDirectory', ProcName=ProcName, stat=StatLoc )

    allocate(This%SubModelRunCommand(This%NbSubModels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%SubModelRunCommand', ProcName=ProcName, stat=StatLoc )

    call MakeDirectory( Path=This%FullWorkDirectory, Options='-p' )

    i = 1
    do i = 1, This%NbSubModels
      SubSectionName = SectionName // '>submodel' // ConvertToString(Value=i)

      ParameterName = 'case_directory'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%SubModelCaseDirectory(i) = VarC0D

      ParameterName = 'run_command'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%SubModelRunCommand(i) = VarC0D

      WorkDirectoryLoc = This%FullWorkDirectory // '/' // ConvertToString(Value=i)
      DirectoryLoc = WorkDirectoryLoc // This%SubModelCaseDirectory(i)
      call MakeDirectory( Path=DirectoryLoc, Options='-p' )

      call system( 'cp -rf ' // PrefixLoc // This%SubModelCaseDirectory(i) // '/* ' // DirectoryLoc // '/' )

      SubSectionName = 'parameter_writer'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%ParameterWriter%Construct( Input=InputSection, Prefix=WorkDirectoryLoc )
      nullify( InputSection )

      SubSectionName = 'output_reader'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%OutputReader%Construct( Input=InputSection, Prefix=WorkDirectoryLoc )
      nullify( InputSection )
    end do

    This%NbOutputs = This%OutputReader(1)%GetNbOutputs() 

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(ModelExternal_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='label', Value=This%Label )
    call GetInput%AddParameter( Name='work_directory', Value=This%WorkDirectory )
    call GetInput%AddParameter( Name='nb_concurrent_evaluations', Value=This%NbConcurrentEvaluations )
    call GetInput%AddParameter( Name='nb_concurrent_subevaluations', Value=This%NbConcurrentSubEvaluations )

    call GetInput%AddSection( Section=This%ParameterWriter(1)%GetInput(MainSectionName='parameter_writer', Prefix=PrefixLoc,      &
                                                                                                         Directory=DirectorySub) )
    call GetInput%AddSection( Section=This%OutputReader(1)%GetInput(MainSectionName='output_reader', Prefix=PrefixLoc,            &
                                                                                                         Directory=DirectorySub) )

    SectionName = 'submodels'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbSubModels
      SubSectionName = 'submodel' // ConvertToString(Value=i)
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      SubSectionName = SectionName // '>' // SubSectionName
      call GetInput%AddParameter( Name='case_directory', Value=This%SubModelCaseDirectory(i)%GetValue(),                          &
                                                                                                      SectionName=SubSectionName )
      call GetInput%AddParameter( Name='run_command', Value=This%SubModelRunCommand(i)%GetValue(), SectionName=SubSectionName )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D( This, Input, Output, Stat )

    class(ModelExternal_Type), intent(inout)                          ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    StatRun=0
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), dimension(:,:), pointer                                ::    OrdinatePointer=>null()
    real(rkp), dimension(:,:), pointer                                ::    OrdinateLocPointer=>null()
    integer                                                           ::    NbDegen
    integer                                                           ::    NbDegenLoc
    integer                                                           ::    i, ii
    type(InputDet_Type)                                               ::    InputLoc
    type(Output_Type), allocatable, dimension(:)                      ::    OutputLoc
    integer                                                           ::    NbOutputs

    NbOutputs = This%OutputReader%GetNbOutputs()

    if ( allocated(Output) ) then
      if ( size(Output,1) /= NbOutputs ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if
    if ( .not. allocated(Output) ) then
      allocate(Output(NbOutputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

    select type (Input)

      type is (InputDet_Type)
        call This%ParameterWriter%WriteInput( Input=Input )

        call This%ModelExternalModel%Run( Stat=StatRun )

        if ( present(Stat) ) Stat = StatRun

        if ( StatRun == 0 ) call This%OutputReader%ReadOutput( Output=Output )

      type is (InputStoch_Type)
        allocate(OutputLoc(NbOutputs), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

        NbDegen = Input%GetNbDegen()

        i = 1
        do i = 1, NbDegen
          InputLoc = Input%GetDetInput(Num=i)
          call This%ParameterWriter%WriteInput( Input=InputLoc )
          call This%ModelExternalModel%Run( Stat=StatRun )
          if ( present(Stat) ) Stat = StatRun

          if ( StatRun /= 0 ) exit

          call This%OutputReader%ReadOutput( Output=OutputLoc )

          if ( i == 1 ) then
            ii = 1
            do ii = 1, size(OutputLoc,1)
              allocate(VarR2D(OutputLoc(ii)%GetNbNodes(),NbDegen*OutputLoc(ii)%GetNbDegen()), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
              VarR2D = Zero
              call Output(ii)%Construct( Values=VarR2D, Label=OutputLoc(ii)%GetLabel() )
              deallocate(VarR2D, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
            end do
          end if

          ii = 1
          do ii = 1, size(OutputLoc,1)
            OrdinatePointer => Output(ii)%GetValuesPointer()
            OrdinateLocPointer => OutputLoc(ii)%GetValuesPointer()
            NbDegenLoc = OutputLoc(ii)%GetNbDegen()
            OrdinatePointer(:,(i-1)*NbDegenLoc+1:i*NbDegenLoc) = OrdinateLocPointer(:,:)
            nullify(OrdinatePointer)
            nullify(OrdinateLocPointer)
          end do
        end do

        deallocate(OutputLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      class default
        call Error%Raise( Line='Update input class definitions', ProcName=ProcName )

    end select


    if ( present(Stat) ) Stat = StatRun

    if ( StatRun /= 0 .and. allocated(Output) ) then
      deallocate(Output, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(ModelExternal_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (ModelExternal_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%ModelExternalModel = RHS%ModelExternalModel
          LHS%ParameterWriter = RHS%ParameterWriter
          LHS%OutputReader = RHS%OutputReader
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
