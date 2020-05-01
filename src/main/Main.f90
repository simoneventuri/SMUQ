program SMUQ

  use Input_Library
  use Test_Module
  use CommandRoutines_Module
  use Logger_Class                                                  ,only:    Logger
  use Error_Class                                                   ,only:    Error
  use ProgramDefs_Class                                             ,only:    ProgramDefs
  use Root_Class                                                    ,only:    Root_Type
  use Restart_Class                                                 ,only:    RestartUtility
  use SMUQString_Class                                              ,only:    SMUQString_Type

  implicit none
  
  character(*), parameter                                               ::    ProcName='main'
  integer                                                               ::    StatLoc=0
  type(InputReader_Type)                                                ::    Input
  type(InputSection_Type), pointer                                      ::    InputSection=>null()
  type(InputSection_Type)                                               ::    CMDArgsSection
  type(Root_Type)                                                       ::    Root
  character(:), allocatable                                             ::    ParameterName
  character(:), allocatable                                             ::    SectionName
  character(:), allocatable                                             ::    FileName
  character(:), allocatable                                             ::    RunDirectory
  character(:), allocatable                                             ::    SectionChain
  type(SMUQString_Type)                                                 ::    SMUQTask
  logical                                                               ::    VarL0D
  
  write(*,*)
  write(*,'(A)') "|=================================================================================|"
  write(*,'(A)') "|                 __________                          _______                     |"
  write(*,'(A)') "|                /             /|    /| |          | |       \                    |"             
  write(*,'(A)') "|               /             / |   / | |          | |        \                   |"
  write(*,'(A)') "|              /             /  |  /  | |          | |         \                  |"
  write(*,'(A)') "|             (_________    /   | /   | |          | |          \                 |"
  write(*,'(A)') "|                       )  /    |/    | |          | |           \                |"
  write(*,'(A)') "|                      /  /           | |          | |            \               |"
  write(*,'(A)') "|                     /  /            | |          | |        \    \              |"
  write(*,'(A)') "|          __________/  /             | l__________| l_________\___!              |"
  write(*,'(A)') "|                                                               \                 |"
  write(*,'(A)') "|                                                                                 |"
  write(*,'(A)') "|=================================================================================|"
  write(*,'(A)') "|                                                                                 |"
  write(*,'(A)') "| By                                                                              |"
  write(*,'(A)') "|     Przemyslaw Rostkowski (1)                                                   |"
  write(*,'(A)') "|     Simone Venturi (1)                                                          |"
  write(*,'(A)') "|                                                                                 |"
  write(*,'(A)') "| (1) University of Illinois at Urbana Champaign                                  |"
  write(*,'(A)') "|                                                                                 |"
  write(*,'(A)') "|=================================================================================|"
  write(*,*)
  !!--------------------------------------------------------------------------------------------------------------------------------
  !! Getting Running DIrectory
  !!--------------------------------------------------------------------------------------------------------------------------------
  call GetCurrentDirectoryPath(Path=RunDirectory)
  write(*,'(A)') 'Running SMUQ in :'
  write(*,'(A)') '  ' // RunDirectory
  write(*,*)
  
  !!--------------------------------------------------------------------------------------------------------------------------------
  !! Reading in command line arguments
  !!--------------------------------------------------------------------------------------------------------------------------------
  call CMDArgsSection%SetName('cmdargs')
  call CMDArgsSection%AddCommandLineArguments()
  
  call ProgramDefs%Construct(Input=CMDArgsSection, Prefix=RunDirectory)
         
  !!--------------------------------------------------------------------------------------------------------------------------------                                                                             , 
  !! Setting up run environment
  !!--------------------------------------------------------------------------------------------------------------------------------
  write(*,'(A)') 'Setting up the run environment'
  write(*,*)
  if (len_trim(ProgramDefs%GetSuppliedCaseDir()) /= 0) then
    FileName = ProgramDefs%GetSuppliedCaseDir() // ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
    inquire(File=FileName, Exist=VarL0D)
    if (.not. VarL0D) call Error%Raise('Supplied an incompatible external case or it may not exist', ProcName=ProcName)
  else
    FileName = ProgramDefs%GetCaseDir() // ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
    inquire(File=FileName, Exist=VarL0D)
    if (.not. VarL0D) call Error%Raise('Did not find the case directory in the run directory and no external alternative was ' //&
                                          'supplied', ProcName=ProcName)
  end if
  
  write(*,'(A)') '  Creating the output directory : ' 
  write(*,'(A)') '    ' // ProgramDefs%GetOutputDir()
  call MakeDirectory(Path=ProgramDefs%GetOutputDir(), Options='-p')
  call RemoveDirectory(Path=ProgramDefs%GetOutputDir(), ContentsOnly=.true.)
  
  write(*,'(A)') '  Creating the log directory : ' 
  write(*,'(A)') '    ' // ProgramDefs%GetLogDir()
  call MakeDirectory(Path=ProgramDefs%GetLogDir(), Options='-p')
  call RemoveDirectory(Path=ProgramDefs%GetLogDir(), ContentsOnly=.true.)
  
  write(*,'(A)') '  Creating the restart directory : ' 
  write(*,'(A)') '    ' // ProgramDefs%GetRestartDir()
  call MakeDirectory(Path=ProgramDefs%GetRestartDir(), Options='-p')
  call RemoveDirectory(Path=ProgramDefs%GetRestartDir(), ContentsOnly=.true.)
  
  call MakeDirectory(Path=ProgramDefs%GetCaseDir(), Options='-p')
  
  if (len_trim(ProgramDefs%GetSuppliedCaseDir()) /= 0) then
    write(*,'(A)') '  Copying over to a local case directory the supplied external case directory : ' 
    write(*,'(A)') '    ' // ProgramDefs%GetSuppliedCaseDir()
    call RemoveDirectory(Path=ProgramDefs%GetCaseDir(), ContentsOnly=.true.)
    call CopyDirectory(Source=ProgramDefs%GetSuppliedCaseDir(), Destination=ProgramDefs%GetCaseDir(), ContentsOnly=.true.)
  end if
  
  write(*,*)
  write(*,'(A)') 'Run environment set-up complete'
  write(*,*)
  
  !!--------------------------------------------------------------------------------------------------------------------------------
  !! Initializing logger with log file
  !!--------------------------------------------------------------------------------------------------------------------------------
  call Logger%Initialize(FileName=ProgramDefs%GetLogFilePath(), Status='REPLACE', Position='REWIND', Procedure='SMUQ',             &
                                                                                                                     Indentation=2)
  
  !!--------------------------------------------------------------------------------------------------------------------------------
  !! Reading in input
  !!--------------------------------------------------------------------------------------------------------------------------------
  write(*,'(A)') 'Reading in input'
  write(*,*)
  FileName = ProgramDefs%GetInputFilePath()
  call Input%Read(FileName=FileName)
  
  call Input%Write(Logger=Logger)
  
  SMUQTask = Input%GetName()
  
  !!--------------------------------------------------------------------------------------------------------------------------------
  !! Constructing from input and running analysis
  !!--------------------------------------------------------------------------------------------------------------------------------
  
  select case (SMUQTask%Lower())
    case('main')
      write(*,'(A)') 'Constructing objects from input and creating necessary temporary work directories'
      write(*,*)
      SectionChain = 'main'
      call Root%Construct(Input=Input, SectionChain=SectionChain, Prefix=ProgramDefs%GetCaseDir())
      call RestartUtility%Construct(Input=Root%GetInput(Name='main', Prefix=ProgramDefs%GetRestartDir(),                &
                                                                            Directory='/main'), Prefix=ProgramDefs%GetRestartDir())
      call Root%Run()   
    case('test')
      call Test(Input=Input, Prefix=ProgramDefs%GetCaseDir())
    case default
      call Error%Raise(Line='Specified task (name of main input section) not recognized')
  end select
  
  end program