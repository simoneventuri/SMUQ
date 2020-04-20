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
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ParameterWriter_Class                                         ,only:    ParameterWriter_Type
use PathWriter_Class                                              ,only:    PathWriter_Type
use OutputReader_Class                                            ,only:    OutputReader_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use InputProcessor_Class                                          ,only:    InputProcessor_Type

implicit none

private

public                                                                ::    ModelExternal_Type

type, extends(Model_Type)                                             ::    ModelExternal_Type
  type(PathWriter_Type), allocatable, dimension(:)                    ::    PathWriter
  type(ParameterWriter_Type), allocatable, dimension(:)               ::    ParameterWriter
  type(OutputReader_Type), allocatable, dimension(:)                  ::    OutputReader
  type(String_Type), allocatable, dimension(:)                        ::    SubModelCaseDirectory
  type(String_Type), allocatable, dimension(:)                        ::    SubModelRunCommand
  integer                                                             ::    NbSubModels
  integer                                                             ::    NbConcurrentEvaluations
  integer                                                             ::    NbConcurrentSubEvaluations
  type(SMUQFile_Type)                                                 ::    BashLaunchFile
  character(:), allocatable                                           ::    WorkDirectory
  character(:), allocatable                                           ::    FullWorkDirectory
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
  subroutine Initialize(This)

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'ModelExternal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    call This%SetDefaults()

    call This%BashLaunchFile%Reset()

    if (allocated(This%ParameterWriter)) deallocate(This%ParameterWriter, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParameterWriter', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%OutputReader)) deallocate(This%OutputReader, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%OutputReader', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%PathWriter)) deallocate(This%PathWriter, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%PathWriter', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%SubModelCaseDirectory)) deallocate(This%SubModelCaseDirectory, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SubModelCaseDirectory', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%SubModelRunCommand)) deallocate(This%SubModelRunCommand, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SubModelRunCommand', ProcName=ProcName, stat=StatLoc)

    This%NbOutputs = 0
    This%NbSubModels = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(ModelExternal_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Silent = .false.
    This%FullWorkDirectory = ''
    This%Label = 'external'
    This%NbConcurrentEvaluations = 1
    This%NbConcurrentSubEvaluations = 1

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

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
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    VarL0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    WorkDirectoryLoc
    logical                                                           ::    Found
    integer                                                           ::    i
    integer                                                           ::    ii

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Label = VarC0D

    ParameterName = 'silent'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Silent = VarL0D

    This%FullWorkDirectory = ProgramDefs%GetRunDir() // '/TMP_EXTERNAL_' // This%Label
    call MakeDirectory(Path=This%FullWorkDirectory, Options='-p')
    call RemoveDirectory(Path=This%FullWorkDirectory, ContentsOnly=.true.)

    call This%BashLaunchFIle%Construct(File='/LaunchFile.sh', Prefix=This%FullWorkDirectory)

    ParameterName = 'nb_concurrent_evaluations'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%NbConcurrentEvaluations = VarI0D

    SectionName = 'submodels'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbSubModels = InputSection%GetNumberOfSubSections()
    if (This%NbSubModels == 0) call Error%Raise(Line='Provided no submodels', ProcName=ProcName)
    nullify(InputSection)

    ParameterName = 'nb_concurrent_subevaluations'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    This%NbConcurrentSubEvaluations = This%NbSubModels
    if (Found) This%NbConcurrentEvaluations = VarI0D

    if (This%NbConcurrentSubEvaluations == 0) call Error%Raise('Specified 0 number of concurrent submodel evaluations',        &
                                                                                                               ProcName=ProcName)
    if (This%NbConcurrentSubEvaluations > This%NbSubModels) This%NbConcurrentSubEvaluations = This%NbSubModels

    allocate(This%SubModelCaseDirectory(This%NbSubModels), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%SubModelCaseDirectory', ProcName=ProcName, stat=StatLoc)

    allocate(This%SubModelRunCommand(This%NbSubModels), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%SubModelRunCommand', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbSubModels
      SubSectionName = SectionName // '>submodel' // ConvertToString(Value=i)

      ParameterName = 'case_directory'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%SubModelCaseDirectory(i) = VarC0D

      ParameterName = 'run_command'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%SubModelRunCommand(i) = VarC0D

      ii = 1
      do ii = 1, This%NbConcurrentEvaluations
        WorkDirectoryLoc = This%FullWorkDirectory // '/' // ConvertToString(Value=ii) // This%SubModelCaseDirectory(i)
        call MakeDirectory(Path=WorkDirectoryLoc, Options='-p')
        call system('cp -rf ' // PrefixLoc // This%SubModelCaseDirectory(i) // '/* ' // WorkDirectoryLoc)
      end do
    end do

    allocate(This%ParameterWriter(This%NbConcurrentEvaluations), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%ParameterWriter', ProcName=ProcName, stat=StatLoc)

    allocate(This%OutputReader(This%NbConcurrentEvaluations), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%OutputReader', ProcName=ProcName, stat=StatLoc)

    SectionName = 'path_writer'
    if (Input%HasSection(SubSectionname=SectionName)) then
      allocate(This%PathWriter(This%NbConcurrentEvaluations), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%PathWriter', ProcName=ProcName, stat=StatLoc)
    end if

    i = 1
    do i = 1, This%NbConcurrentEvaluations
      WorkDirectoryLoc = This%FullWorkDirectory // '/' // ConvertToString(Value=i) 

      if (allocated(This%PathWriter)) then
        SectionName = 'path_writer'
        call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
        call This%PathWriter(i)%Construct(Input=InputSection, Prefix=WorkDirectoryLoc)
        nullify(InputSection)
        call This%PathWriter(i)%WritePaths()
      end if

      SectionName = 'parameter_writer'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%ParameterWriter(i)%Construct(Input=InputSection, Prefix=WorkDirectoryLoc)
      nullify(InputSection)

      SectionName = 'output_reader'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%OutputReader(i)%Construct(Input=InputSection, Prefix=WorkDirectoryLoc)
      nullify(InputSection)
    end do

    This%NbOutputs = This%OutputReader(1)%GetNbOutputs()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(ModelExternal_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    Name
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

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='label', Value=This%Label)
    call GetInput%AddParameter(Name='nb_concurrent_evaluations', Value=ConvertToString(Value=This%NbConcurrentEvaluations))
    call GetInput%AddParameter(Name='nb_concurrent_subevaluations', Value=ConvertToString(Value=This%NbConcurrentSubEvaluations))

    call GetInput%AddSection(Section=This%ParameterWriter(1)%GetInput(Name='parameter_writer', Prefix=PrefixLoc,      &
                                                                                                         Directory=DirectorySub))
    call GetInput%AddSection(Section=This%OutputReader(1)%GetInput(Name='output_reader', Prefix=PrefixLoc,            &
                                                                                                         Directory=DirectorySub))
    if (allocated(This%PathWriter)) then
      call GetInput%AddSection(Section=This%PathWriter(1)%GetInput(Name='path_writer', Prefix=PrefixLoc,              &
                                                                                                         Directory=DirectorySub))
    end if

    SectionName = 'submodels'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbSubModels
      SubSectionName = 'submodel' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName = SectionName // '>' // SubSectionName
      call GetInput%AddParameter(Name='case_directory', Value=This%SubModelCaseDirectory(i)%GetValue(),                          &
                                                                                                      SectionName=SubSectionName)
      call GetInput%AddParameter(Name='run_command', Value=This%SubModelRunCommand(i)%GetValue(), SectionName=SubSectionName)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D(This, Input, Output, Stat)

    class(ModelExternal_Type), intent(inout)                          ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    StatRun=0
    type(String_Type), allocatable, dimension(:)                      ::    Transcript
    integer                                                           ::    i
    integer                                                           ::    iPass
    integer                                                           ::    iRun
    integer                                                           ::    iLine
    integer                                                           ::    iSubModel
    character(:), allocatable                                         ::    Command
    integer                                                           ::    NbCompletedSubModels
    character(:), allocatable                                         ::    Line

    if (size(Output,1) /= This%NbOutputs) call Error%Raise('Passed down an output array of incorrect length',                  &
                                                                                                               ProcName=ProcName)

    Command = 'sh ' // This%BashLaunchFile%GetFullFile()


    if (.not. This%Silent) then
      write(*,*)
      Line = 'Scheduling 1 input with' // ConvertToString(Value=This%NbSubModels) //                                              &
                                          'submodels for a total of ' // ConvertToString(Value=This%NbSubModels) // ' evaluations'
      write(*,'(A)') Line
      Line = '  Number of concurrent input evaluations : ' // ConvertToString(Value=This%NbConcurrentEvaluations)
      write(*,'(A)') Line
      Line = '  Number of concurrent submodel calls : ' // ConvertToString(Value=This%NbConcurrentSubEvaluations)
      write(*,'(A)') Line
    end if

    call This%ParameterWriter(1)%WriteInput(Input=Input)

    allocate(Transcript(This%NbConcurrentSubEvaluations*5+3), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Transcript', ProcName=ProcName, stat=StatLoc)
    Transcript(1) = '#!/bin/bash'
    
    NbCompletedSubModels = 0
    iPass = 1
    iRun = 1
    iSubModel = 1
    do
      iLine = 2
      iPass = iPass + 1

      if (.not. This%Silent) then
        Line = '  Blocking pass ' // ConvertToString(Value=iPass)
        write(*,'(A)') Line
      end if

      i = 1
      do i = 1, This%NbConcurrentSubEvaluations
        if (iSubModel > This%NbSubModels) exit

        if (.not. This%Silent) then
          Line = '  Evaluation ' // ConvertToString(Value=iRun) // ' : Input ' // ConvertToString(Value=1) // ' Submodel '        &
                                                                                                     // ConvertToString(iSubModel)
          write(*,'(A)') Line
          Transcript(iLine) = 'echo "  Evaluation ' // ConvertToString(Value=iRun) // ' : Initializing"'
          iLine = iLine + 1
        end if

        Transcript(iLine) = '(cd ' // This%FullWorkDirectory // '/' // ConvertToString(Value=1) //                               &
                                                                         This%SubModelCaseDirectory(iSubModel)%GetValue() // ' \ '
        iLine = iLine + 1
        Transcript(iLine) = ' && ' // This%SubModelRunCommand(iSubModel)%GetValue() // ' \ '
        iLine = iLine + 1
        if (.not. This%Silent) then
          Transcript(iLine) = 'echo "  Evaluation ' // ConvertToString(Value=iRun) // ' : Complete" \ '
          iLine = iLine + 1
        end if

        Transcript(iLine) = ')&'
        iLine = iLine + 1

        iSubModel = iSubModel + 1
        iRun = iRun + 1
      end do

      if (.not. This%Silent) then
        Transcript(iLine) = 'echo "  Waiting for evaluations to complete"'
        iLine = iLine + 1
      end if

      Transcript(iLine) = 'wait'

      call This%BashLaunchFile%Export(Strings=Transcript(1:iLine))
      call ExecuteSysCommand(SysCommand=Command, Wait=.true.)

      NbCompletedSubModels = min(This%NbSubModels, NbCompletedSubModels + This%NbConcurrentSubEvaluations)

      if (NbCompletedSubModels == This%NbSubModels) exit
    end do

    StatRun = 0

    if (.not. This%Silent) then
      Line = '  Retrieving output data corresponding to input 1'
      write(*,'(A)') Line
    end if

    write(*,*)

    if (StatRun == 0) call This%OutputReader(1)%ReadOutput(Output=Output)

    if (present(Stat)) Stat = StatRun

    deallocate(Transcript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Transcript', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_1D(This, Input, Output, Stat)

    class(ModelExternal_Type), intent(inout)                          ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbInputs
    type(String_Type), allocatable, dimension(:)                      ::    Transcript
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iPass
    integer                                                           ::    iRun
    integer                                                           ::    iLine
    integer                                                           ::    iSubModel
    integer                                                           ::    iInput
    integer                                                           ::    NbCompletedSubModels
    integer                                                           ::    NbCompletedInputs
    character(:), allocatable                                         ::    Command
    character(:), allocatable                                         ::    Line

    NbInputs = size(Input,1)

    if (size(Output,1) /= This%NbOutputs .or. size(Output,2) /= NbInputs)                                                       &
                                       call Error%Raise('Passed an output array of incorrect dimensionality', ProcName=ProcName)

    if (size(Stat,1) /= NbInputs) call Error%Raise('Passed a stat array of incorrect length', ProcName=ProcName)

    Command = 'sh ' // This%BashLaunchFile%GetFullFile()

    if (.not. This%Silent) then
      write(*,*)
      Line = 'Scheduling ' // ConvertToString(Value=NbInputs) // ' inputs with ' // ConvertToString(Value=This%NbSubModels) //    &
                                 ' submodels for a total of ' // ConvertToString(Value=NbInputs*This%NbSubModels) // ' evaluations'
      write(*,'(A)') Line
      Line = '  Number of concurrent input evaluations : ' // ConvertToString(Value=This%NbConcurrentEvaluations)
      write(*,'(A)') Line
      Line = '  Number of concurrent submodel calls : ' // ConvertToString(Value=This%NbConcurrentSubEvaluations)
      write(*,'(A)') Line
    end if

    allocate(Transcript(This%NbConcurrentEvaluations*This%NbConcurrentSubEvaluations*5+3), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Transcript', ProcName=ProcName, stat=StatLoc)
    Transcript(1) = '#!/bin/bash'

    if (present(Stat)) Stat = 1
    iPass = 1
    iRun = 1
    NbCompletedSubModels = 0
    NbCompletedInputs = 0

    do
      if (.not. This%Silent) then
        write(*,*)
        Line = '  Blocking pass ' // ConvertToString(Value=iPass)
        write(*,'(A)') Line
      end if

      iLine = 2
      iInput = NbCompletedInputs + 1
      iPass = iPass + 1

      i = 1
      do i = 1, This%NbConcurrentEvaluations
        if (iInput > NbInputs) exit
        iSubModel = NbCompletedSubModels + 1

        if (iSubModel == 1) call This%ParameterWriter(i)%WriteInput(Input=Input(iInput))

        ii = 1
        do ii = 1, This%NbConcurrentSubEvaluations
          if (iSubModel > This%NbSubModels) exit
          if (.not. This%Silent) then
            Line = '    Evaluation ' // ConvertToString(Value=iRun) // ' : Input ' // ConvertToString(Value=iInput) //            &
                                                                                        ' Submodel ' // ConvertToString(iSubModel)
            write(*,'(A)') Line
            Transcript(iLine) = 'echo "    Evaluation ' // ConvertToString(Value=iRun) // ' : Initializing"'
            iLine = iLine + 1
          end if

          Transcript(iLine) = '(cd ' // This%FullWorkDirectory // '/' // ConvertToString(Value=i) //                             &
                                                                         This%SubModelCaseDirectory(iSubModel)%GetValue() // ' \ '
          iLine = iLine + 1
          Transcript(iLine) = ' && ' // This%SubModelRunCommand(iSubModel)%GetValue() // ' \ '
          iLine = iLine + 1

          if (.not. This%Silent) then
            Transcript(iLine) = 'echo "    Evaluation ' // ConvertToString(Value=iRun) // ' : Complete" \ '
            iLine = iLine + 1
          end if

          Transcript(iLine) = ')&'
          iLine = iLine + 1

          iSubModel = iSubModel + 1
          iRun = iRun + 1
        end do

        iInput = iInput + 1
      end do

      if (.not. This%Silent) then
        Transcript(iLine) = 'echo "  Waiting for evaluations to complete"'
        iLine = iLine + 1
      end if

      Transcript(iLine) = 'wait'

      if (.not. This%Silent) then
        Line = '  Initializing evaluation ' // ConvertToString(Value=NbCompletedInputs + 1) 
        if (This%NbConcurrentEvaluations > 1) Line = Line // '-' //    &
                                           ConvertToString(Value=min(NbInputs,NbCompletedInputs + This%NbConcurrentEvaluations))
        write(*,'(A)') Line
      end if
      call This%BashLaunchFile%Export(Strings=Transcript(1:iLine))
      call ExecuteSysCommand(SysCommand=Command, Wait=.true.)

      NbCompletedSubModels = min(This%NbSubModels,NbCompletedSubModels + This%NbConcurrentSubEvaluations)

      if (NbCompletedSubModels == This%NbSubModels) then
        if (.not. This%Silent) then
          Line = '  Retrieving output data corresponding to input ' // ConvertToString(Value=NbCompletedInputs + 1) 
          if (This%NbConcurrentEvaluations > 1) Line = Line // '-' //    &
                                             ConvertToString(Value=min(NbInputs,NbCompletedInputs + This%NbConcurrentEvaluations))
          write(*,'(A)') Line
        end if
        ii = 0
        i = NbCompletedInputs + 1
        do i = NbCompletedInputs + 1, min(NbInputs,NbCompletedInputs + This%NbConcurrentEvaluations)
          ii = ii + 1
          if (.not. This%OutputReader(ii)%AllFound()) then
            if (.not. This%Silent) then
              Line = '    Detected a failed run with input ' // ConvertToString(Value=i)
              write(*,'(A)') Line
            end if
            iii = 1
            do iii = 1, This%NbOutputs
              call Output(iii,i)%Reset()
            end do
            cycle
          end if
          call This%OutputReader(ii)%ReadOutput(Output=Output(:,i))
          if (present(Stat)) Stat(i) = 0
        end do
        NbCompletedInputs = min(NbInputs,NbCompletedInputs + This%NbConcurrentEvaluations)
        NbCompletedSubModels = 0
      end if

      if (NbCompletedInputs == NbInputs) exit
    end do

    write(*,*)

    deallocate(Transcript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Transcript', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(ModelExternal_Type), intent(out)                            ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (ModelExternal_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          allocate(LHS%ParameterWriter, source=RHS%ParameterWriter, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%ParameterWriter', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%OutputReader, source=RHS%OutputReader, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%OutputReader', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%SubModelCaseDirectory, source=RHS%SubModelCaseDirectory, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%SubModelcaseDirectory', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%SubModelRunCommand, source=RHS%SubModelRunCommand, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%SubModelRunCommand', ProcName=ProcName, stat=StatLoc)
          LHS%NbSubModels = RHS%NbSubModels
          LHS%NbConcurrentEvaluations = RHS%NbConcurrentEvaluations
          LHS%NbConcurrentSubEvaluations = RHS%NbConcurrentSubEvaluations
          LHS%BashLaunchFIle = RHS%BashLaunchFile
          LHS%WorkDirectory = RHS%WorkDirectory
          LHS%FullWorkDirectory = RHS%FullWorkDirectory
          LHS%Silent = RHS%Silent
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(ModelExternal_Type),intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%ParameterWriter)) deallocate(This%ParameterWriter, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParameterWriter', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%OutputReader)) deallocate(This%OutputReader, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%OutputReader', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%SubModelCaseDirectory)) deallocate(This%SubModelCaseDirectory, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SubModelCaseDirectory', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%SubModelRunCommand)) deallocate(This%SubModelRunCommand, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SubModelRunCommand', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
