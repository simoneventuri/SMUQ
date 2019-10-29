SubModule(Application_Class) Application_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_DEBUG, LogLevel_HEAVYDEBUG

  implicit none

  integer ,parameter ::  DefaultLogLevel = 0

  logical       ,parameter      ::  InitializeTimerDefault  =   .True.
  logical       ,parameter      ::  WriteHeaderDefault      =   .True.        ! Default indicator for writing header to logfile and to screen
  logical       ,parameter      ::  WriteTimerDefault       =   .False.
!   integer       ,parameter      ::  DefaultLoggerIndent     =   2
!   character(*)  ,parameter      ::  DefaultLoggerProcPath   =   ""
!
  contains

Module Procedure InitializeApplication

  use Error_Class               ,only:  Error

  character(*)                                              ,parameter  ::  ProcName='InitializeApplication'     ! Name of current procedure
  logical                                                               ::  LoggerInitialize_
!   character(:)  ,allocatable                                            ::  DefaultLogFileName
  character(:)  ,allocatable                                            ::  LogFileName

  This%Initialized  = .True.

  call SetParallelEnv( This, ip,np, LogLevel )

  call This%DefineName()    ! Defining the Application name => call of user procedure

! ==============================================================================================================
!    INITIALIZING THE LOGGER OBJECT
! ==============================================================================================================
  LogFileName         =   This%GetName() // ".log"
  LogFileName         =   This%GetOptArgValue( LogFileName, LoggerName )
  LoggerInitialize_   =   This%GetOptArgValue( present(LoggerName), LoggerInitialize )
  if ( LoggerInitialize_ ) then
    call Logger%Initialize( LogFileName,            &                                                           ! Opening the Log File using the input name and:
                Status        =   'REPLACE',        &                                                           ! - replacing any previous log file with the same name
                Position      =   'REWIND',         &                                                           ! - rewinding to the top of the file
                Procedure     =   LoggerProcPath,   &                                                           ! - adding the input procedure names if any
                Indentation   =   LoggerIndentation )                                                           ! - setting the initial indentation level
    call Error%SetOutputUnit( [Logger%GetUnit(),6] )                                                            ! Setting the file unit associated to errors
  end if
  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG )
! ==============================================================================================================


! ==============================================================================================================
!    INITIALIZING THE TIMER OBJECT
! ==============================================================================================================
! ! ! ! The Spark-Configuration object inherits a Timer object component from the "Universal_Type" it extends.
! ! ! ! This section initializes this Timer object if required. This initialization is done by default if the
! ! ! ! optional input argument "Initialize_Timer" is absent. The initialization is done by calling the inherited
! ! ! ! procedure "Initialize_Timer", which sets the name of the Timer and starts the timing.
! ! ! ! This Timer is then stopped by calling the finalization procedure "Finalize" of the Spark-Configuration object.
! ! ! ! The call to the finalization procedure also print the timing inforation to the Logger.
! ============================================================================================================
  call Logger%Write( "Initializing the Timer object" )
  call Logger%Write( "-> Calling This%InitializeTimer" )
  call This%InitializeTimer( This%Name, Start=.True. )
  call Logger%Write( "-> Done initializing the Timer object" )
! ==============================================================================================================


! ==============================================================================================================
!    PROCESSING THE COMMAND LINE ARGUMENTS
! ==============================================================================================================
  call Logger%Write( "Loading command line arguments" )
  call Logger%Write( "-> Calling This%LoadCmdLineArg" )
  call This%LoadCmdLineArg( LogLevel=LogLevel )
  call Logger%Write( "-> Done loading command line arguments" )

  call Logger%Write( "Processing command line arguments" )
  call Logger%Write( "-> Calling This%ProcessCmdLineArg" )
  call This%ProcessCmdLineArg( LogLevel=LogLevel )
  call Logger%Write( "-> Done Processing command line arguments" )
! ==============================================================================================================


! ==============================================================================================================
!    LOADING GLOBAL CONFIGURATIONS IF ANY
! ==============================================================================================================
  call Logger%Write( "Loading global configurations if any" )
  call Logger%Write( "-> Calling This%LoadConfig" )
  call This%LoadConfig( LogLevel=LogLevel )
  call Logger%Write( "-> Done loading configurations" )
! ==============================================================================================================


! ==============================================================================================================
!    GETTING ENVIRONMENT VARIABLES
! ==============================================================================================================
  call Logger%Write( "Getting environment variables" )
  call Logger%Write( "-> Calling This%LoadEnvVar" )
  call This%LoadEnvVar( LogLevel=LogLevel )
  call Logger%Write( "-> Done getting environment variables" )
! ==============================================================================================================


! ==============================================================================================================
!    GETTING DIRECTORIES
! ==============================================================================================================
  call Logger%Write( "Setting directories" )
  call Logger%Write( "-> Calling This%LoadDirectories" )
  call This%LoadDirectories( LogLevel=LogLevel )
  call Logger%Write( "-> Done setting directories" )
! ==============================================================================================================

  call Logger%Exiting()

End Procedure

Subroutine SetParallelEnv( This, ip,np, LogLevel )
  class(Application_Type)                               ,intent(inout)  ::  This
  integer                                     ,optional ,intent(in)     ::  ip,np
  integer                                     ,optional ,intent(in)     ::  LogLevel
  character(*)                                              ,parameter  ::  ProcName='SetParallelEnv'
  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
  This%ip = 1
  This%Np = 1
  if ( present(ip) ) then
    This%ip = ip
    This%Np = Np
  end if
  call Logger%Exiting()
End Subroutine


Module Procedure IsInitialized
  Indicator     =       This%Initialized
End Procedure

! This procedure loads the command-line arguments.
Module Procedure LoadCmdLineArg

  character(*)                                              ,parameter  ::  ProcName='LoadCmdLineArg'     ! Name of current procedure
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  integer                                                               ::  i

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG)

  call Logger%Write( "Initializing CLI object" )
  call Logger%Write( "-> Calling This%CLI%Initialize" )
  call This%CLI%Initialize()
  call Logger%Write( "-> Done initializing CLI object" )

  This%LaunchedCommand    =   This%CLI%Command_Line
  call Logger%Write( "-> This%LaunchedCommand      = ", This%LaunchedCommand )
  call Logger%Write( "-> This%CLI%NArg = ", This%CLI%NArg )

  if ( Logger%On() ) then
    call Logger%Write( "Calling This%CLI%Write" )
    call This%CLI%Write( Lines )
    do i = 1,size(Lines)
      call Logger%Write( trim( Lines(i) ) )
    end do
  end if

  call Logger%Exiting()

End Procedure

! This procedure processes the command-line arguments.
! By default, this procedure is empty.
Module Procedure ProcessCmdLineArg
End Procedure


Module Procedure GetNumberCmdLineArg
  NArg   =   This%CLI%NArg
End Procedure

Module Procedure GetCmdLineArgNameValue
  call This%CLI%GetArguments( iArg, Name, Value )
End Procedure


! This procedure loads global configurations.
! By default, this procedure is empty.
Module Procedure LoadConfig
!   call Logger%Write( "Nothing done" )
End Procedure

! This procedure processes an Input object after it has been read.
! This enables to set some general configuration specified in the Inputs object.
! Currently, the following input processing are done:
!
! * Logger: Specify parameters controlling logs written by the Logger object.
!     -> Active:             List of procedures for which the logs should be enforced
!     -> RecursivelyActive:  List of procedures for which the logs should be recurively enforced
!
! * Output: Specify parameters controlling the output directory.
!     -> Directory:          Name of the output directory
!
Module Procedure ProcessInputs

  character(*)                                              ,parameter  ::  ProcName='ProcessInputs'     ! Name of current procedure

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

  call Logger%Write( "Processing 'Logger' section" )
  call Logger%Write( "-> Calling ProcessInputsLogger" )
  call ProcessInputsLogger( Input, LogLevel )
  call Logger%Write( "-> Done processing 'Logger' section" )

!   call Logger%Write( "Processing 'Output' section" )
!   call Logger%Write( "-> Calling ProcessInputsOutput" )
!   call ProcessInputsOutput( This, Input, LogLevel )
!   call Logger%Write( "-> Done processing 'Output' section" )

  call Logger%Exiting()

End Procedure

! This procedures processes the 'Logger' section in an Input object.
! This section can have the following parameters:
!     Start(Logger)
!       LogLevel          = <integer>
!       Active            = <ProcName-1>, <ProcName-2>, ...
!       RecursivelyActive = <ProcName-1>, <ProcName-2>, ...
!     End(Logger)
! If a 'Logger' section exists in the Input object, then these parameters are extracted and their
! values are passed to the procedures
! * Active:             call Logger%AddProcedureToLog( List )
! * RecursivelyActive:  call Logger%AddRecursivelyActiveProcedures( List )
! This will add a list of procedure for which the logs should be enforced.
Subroutine ProcessInputsLogger( Input, LogLevel )

  use Input_Library               ,only:  InputReader_Type, InputSection_Type
  use String_Library              ,only:  Is_Numeric, Convert, Equal
  use Logger_Parameters           ,only:  LogLevelNames

  class(InputReader_Type)                               ,intent(in)     ::  Input
  integer                                     ,optional ,intent(inout)  ::  LogLevel                         !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='ProcessInputsLogger'     ! Name of current procedure
  character(*)                                              ,parameter  ::  SectionName = 'Logger'
  type(InputSection_Type)                                               ::  Section
  logical                                                               ::  Found
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable                                            ::  ParameterName
  character(:)  ,allocatable ,dimension(:)                              ::  ProcedureNames

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

  call Logger%Write( "Extracting section '"//SectionName//"' from Inputs" )
  call Logger%Write( "-> Calling Input%GetSection with SectionName = ", SectionName )
  Section   =   Input%GetSection( SectionName=SectionName )
  call Logger%Write( "-> Section%Defined = ", Section%Defined )

  if ( Section%Defined ) then

    ParameterName   =   'Active'
    call Logger%Write( "Extracting parameter '"//ParameterName//"' from section '"//SectionName//"'" )
    call Section%GetValue( ProcedureNames, ParameterName, Found=Found )
    Found    =   Found .and. ( len(ProcedureNames) > 0 )
    call Logger%Write( "-> Found = ", Found )
    if (Found) then
      call Logger%Write( "-> Calling Logger%AddProcedureToLog: ProcedureNames = ", ProcedureNames )
      call Logger%AddProcedureToLog( ProcedureNames )
    end if

    ParameterName   =   'RecursivelyActive'
    call Logger%Write( "Extracting parameter '"//ParameterName//"' from section '"//SectionName//"'" )
    call Section%GetValue( ProcedureNames, ParameterName, Found=Found, Mandatory=.False. )
    Found    =   Found .and. ( len(ProcedureNames) > 0 )
    call Logger%Write( "-> Found = ", Found )
    if (Found) then
      call Logger%Write( "-> Calling Logger%AddRecursivelyActiveProcedures: ProcedureNames = ", ProcedureNames )
      call Logger%AddRecursivelyActiveProcedures( ProcedureNames )
    end if

    ParameterName   =   "LogLevel"
    call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName, "SectionName = ", SectionName )
    call Section%GetValue( Value, ParameterName, Found=Found )
    call Logger%Write( "-> Found = ", Found )
    if (Found) then
      call Logger%Write( "-> Value = ", Value )
      if ( Is_Numeric(Value) ) then
        call Convert(Value,LogLevel)
      else
        do i = lbound(LogLevelNames,1),ubound(LogLevelNames,1)
          if ( .Not. Equal(Value,LogLevelNames(i),CaseSensitive=.False.,Trimed=.True.) ) cycle
          LogLevel  =   i
          call Logger%Write( "-> LogLevel = ", LogLevel, "=> ", LogLevelNames(i) )
          exit
        end do
      end if
      call Logger%Write( "-> LogLevel = ", LogLevel )
    end if

  end if

  call Logger%Exiting()

End Subroutine

! This procedures processes the 'Output' section from an Input object.
! This section can have the following parameters:
!     Start(Output)
!       Directory   =   <name-directory>
!     End(Output)
! The output directory will be created only if an output directory has not been already
! specified on the command-line. Once the output directory has been created, then we
! move inside.
Subroutine ProcessInputsOutput( This, Input, LogLevel )

  use Input_Library               ,only:  InputReader_Type, InputSection_Type
  use File_Library                ,only:  GetAbsolutePath


  use SystemCommand_Library     ,only:  SystemCommand_Type
!   use Error_Class               ,only:  Error
!   use Directory_Class           ,only:  Directory_Type
  use String_Library            ,only:  Convert_To_String!, UpperCase
  use File_Library              ,only:  AddPathToFile
!
  class(Application_Type)                               ,intent(inout)  ::  This
  class(InputReader_Type)                               ,intent(in)     ::  Input
  integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='ProcessInputsOutput'     ! Name of current procedure
  character(*)                                              ,parameter  ::  SectionName = 'Output'
  type(InputSection_Type)                                               ::  Section
  logical                                                               ::  Found
  character(:)  ,allocatable                                            ::  ParameterName, ParameterValue
  character(:)  ,allocatable                                            ::  WorkingDirectory


  integer                                                               ::  i                               ! Index of directories
!   logical                                                               ::  SameDirectory
!   logical                                                               ::  DirFound                        ! Indicator whether a directory has been found
!   character(:)  ,allocatable                                            ::  CodeName                        ! Code name in uppercase
!   character(:)  ,allocatable                                            ::  Folder
  character(:)  ,allocatable                                            ::  DirPath                         ! Path of a given directory
  character(:)  ,allocatable                                            ::  DirName                         ! Public name of a given directory
  character(:)  ,allocatable                                            ::  DirKey                          ! Key of a given directory
  character(:)  ,allocatable                                            ::  DirDesc                         ! Description of a given directory
  character(:)  ,allocatable                                            ::  DirBaseName                     ! Public name of a given directory
  character(:)  ,allocatable                                            ::  DirSuffix                       ! Suffix for working directory name
!   character(:)  ,allocatable                                            ::  FileName                        ! Name of a file or a pattern
  type(SystemCommand_Type)                                              ::  Command                         ! Command-Line object
  type(Directory_Type)                                                  ::  Dir



  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

  call Logger%Write( "Extracting section '"//SectionName//"' from Inputs" )
  call Logger%Write( "-> Calling Input%GetSection with SectionName = ", SectionName )
  Section   =   Input%GetSection( SectionName=SectionName )
  call Logger%Write( "-> Section%Defined = ", Section%Defined )

  if ( Section%Defined ) then
    ParameterName           =   'Directory'
    call Logger%Write( "Extracting parameter '"//ParameterName//"' from section '"//SectionName//"'" )
    call Section%GetValue( ParameterValue, ParameterName, Found=Found )
    call Logger%Write( "-> Found = ", Found )
    if (Found) then
      call Logger%Write( "-> ParameterValue = ", ParameterValue )
      call Logger%Write( "-> This%NewWorkingDirectory = ", This%NewWorkingDirectory )
      This%NewWorkingDirectory  =   .true.
      call Logger%Write( "This%NewWorkingDirectory = ", This%NewWorkingDirectory )
      WorkingDirectory   =   GetAbsolutePath(ParameterValue)
      call This%AddDirectory( Directory_Type( WorkingDirectory, PublicName='$('//'WORKING_DIR'//')', Key="WORKING", Description='Working directory' ) )
      call Logger%Write( "-> This%GetDirectoryPath('WORKING') = ", This%GetDirectoryPath('WORKING') )
    end if
  end if



!       call Command%cd( ParameterValue, Create=.True. )
!       call Command%cp( "../" // Logger%Units(1)%FileName, "." )
!       call Logger%ReOpen()
!       call Command%cp( "../" // Input%FileName, "." )

! ==============================================================================================================
!     SETTING THE NAME OF THE WORKING DIRECTORY
! ==============================================================================================================
  call Logger%Write( "Setting the name of the working directory" )
  DirName   =   "$(WORKING_DIR)"                                                                              ! Setting the public name of current directory
  DirKey    =   "WORKING"                                                                                     ! Setting the key of current directory
  DirDesc   =   "Working directory"                                                                           ! Setting the description of current directory
  DirPath   =   GetNewWorkingDirectoryPath( This, LogLevel )                                                  ! Getting the path of the of working directory
  call Logger%Write( "-> Adding directory: DirName = '"//DirName//"': '"//DirPath//"'" )
  call This%AddDirectory( Directory_Type( DirPath, PublicName=DirName, Key=DirKey, Description=DirDesc ) )
! ==============================================================================================================


  if ( Logger%On() ) then
    call Logger%Write( "Summary of the directories loaded in the Spark object" )
    call Logger%Write( "-> This%GetNumberDirectories() = ", This%GetNumberDirectories() )
    do i = 1,This%GetNumberDirectories()
      Dir   =   This%GetDirectory(i)
      call Logger%Write( "-> i = ", i, "Key = ", Dir%Key, "PublicName = ", Dir%PublicName, "Dir%Path = ", Dir%Path, Fi="i3", F4="a10", F6="a25" )
    end do
  end if


! ==============================================================================================================
!   CREATING THE WORKING DIRECTORY AND ITS SUB-DIRECTORIES
! ==============================================================================================================
  call Logger%Write( "call CreateWorkingDirectory" )
  call CreateWorkingDirectory( This, LogLevel )
! ==============================================================================================================



  call Logger%Exiting()

End Subroutine



! ==============================================================================================================
!       SETTING THE NAME OF THE WORKING DIRECTORY: CURRENT DIRECTORY
! ==============================================================================================================
! When the code is launched, the working directory corresponds to the launched directory and all outputs of
! the code are by default written in this working directory. This is the default behavior and the name of the
! working directory is thus set to the name of the launch directory.
! However, one might want to organize things a little bit by creating in the launch directory a sub-directory
! where to store all the outputs (and eventually the inputs) of a given simulation. This sub-directory
! corresponds to the new working directory and it different from the launch directory. By ensuring that the
! the name of this new sub-directory is unique, each simulation can then store their results in a distinct
! directory without overriding the results from other simualtions.
! Special care is katen when performing parallel simulation to ensure that all images have the same working
! directory name. This requires a synchronization.
! This procedure sets the name of the working directory.
! It is important that all images sets the name of the working directory before the directory is actually created.
! If the working directory is created before all images sets its name, then there might be a problem since different
! images will create different working directory.
! ==============================================================================================================
Function GetNewWorkingDirectoryPath( This, LogLevel ) result(WorkingDirectoryPath)

  use SystemCommand_Library     ,only:  SystemCommand_Type
  use String_Library            ,only:  Convert_To_String
  use File_Library              ,only:  AddPathToFile
!
  class(Application_Type)                               ,intent(inout)  ::  This
  integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
  character(:)  ,allocatable                                            ::  WorkingDirectoryPath                         ! Path of a given directory

  character(*)                                              ,parameter  ::  ProcName='GetNewWorkingDirectoryPath'     ! Name of current procedure
  integer                                                               ::  i                               ! Index of directories
  character(:)  ,allocatable                                            ::  DirName                         ! Public name of a given directory
  character(:)  ,allocatable                                            ::  BasePath, Path                    ! Public name of a given directory
  character(:)  ,allocatable                                            ::  Suffix                       ! Suffix for working directory name
  character(:)  ,allocatable                                            ::  LaunchDirectoryPath
  type(SystemCommand_Type)                                              ::  Command                         ! Command-Line object


  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

  LaunchDirectoryPath     =   This%GetDirectoryPath('LAUNCH')
  call Logger%Write( "-> LaunchDirectoryPath = ", LaunchDirectoryPath )

  if ( .Not. This%NewWorkingDirectory ) then
    WorkingDirectoryPath  =   LaunchDirectoryPath
    call Logger%Exiting()
    return
  end if

  BasePath   =   This%GetDirectoryPath('WORKING')
  call Logger%Write( "-> BasePath = ", BasePath )

  if ( len(BasePath) == 0 ) BasePath = "SparkResults"

!   if ( This%Enforce_Working_Directory ) then
!     call Logger%Write( "-> Enforcing the name of the directory to ", BasePath )
!     if ( Logger%On() ) then
!       if ( Command%DoesDirectoryExists(BasePath) ) then
!         call Logger%Write( "-> Directory '" // BasePath // "' already exists => Erasing it" )
!       else
!         call Logger%Write( "-> Directory '" // BasePath // "' does not exist => Creating it" ); end if
!     end if
!   end if

  i         =     0                                                                                           ! Initializing the directory index
  do                                                                                                          ! Infinit loop in order to set the name of the working directory
    Suffix  =     ""                                                                                        ! Initializing the suffix for the working directory name
    if ( i /= 0 ) Suffix = "-" // Convert_To_String(i)                                                        ! If the index directory is non-zero, then setting the directory suffix
    i       =  i + 1                                                                                          ! Incrementing the directory index
    Path    =   BasePath // Suffix                                                               ! Setting the working directory name
    call Logger%Write( "   -> i = ", i, "Path = ", Path )                            ! Writing log info
    if ( Command%DoesDirectoryExists(Path) ) cycle
!     if ( Command%DoesDirectoryExists(Path) .And. .Not.This%Enforce_Working_Directory ) cycle               ! If current directory name already exists, then cycling
    exit                                                                                                      ! Exiting the loop so that the "DirNameg" variable stores the name of the directory to be created and it is the same for all images
  end do                                                                                                      ! End loop on directory names
  call Logger%Write( "-> Path = ", Path )
  WorkingDirectoryPath  =   AddPathToFile(LaunchDirectoryPath,Path)                                   ! Adding the full path in front of the working directory
  call Logger%Write( "-> WorkingDirectoryPath = ", WorkingDirectoryPath )

  call Logger%Exiting()

End Function

Subroutine CreateWorkingDirectory( This, LogLevel )

  use SystemCommand_Library     ,only:  SystemCommand_Type
  use File_Library              ,only:  AddPathToFile
  use Error_Class               ,only:  Error

  class(Application_Type)                               ,intent(inout)  ::  This
  integer                                     ,optional ,intent(in)     ::  LogLevel

  character(*)                                              ,parameter  ::  ProcName='CreateWorkingDirectory'     ! Name of current procedure
  integer                                                               ::  i                               ! Index of directories
  logical                                                               ::  SameDirectory
  character(:)  ,allocatable                                            ::  WorkingDirectory
  character(:)  ,allocatable                                            ::  LaunchDirectory
  character(:)  ,allocatable                                            ::  FileName                        ! Name of a file or a pattern
  type(SystemCommand_Type)                                              ::  Command                         ! Command-Line object

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

! ==============================================================================================================
!   CREATING THE WORKING DIRECTORY AND ITS SUB-DIRECTORIES
! ==============================================================================================================
! If the working directory is different than the launch directory, then it has to be created.
! Again, caution is taken for parallel simulation to ensure than one one image is creating the directory.
! Note that although it is ok for images to create the same directory since the '-p' option is used to
! the 'mkdir' command, only a single image is creating the wrojking directory used for safety.
! ==============================================================================================================
  WorkingDirectory  =   This%GetDirectoryPath('WORKING')
  LaunchDirectory   =   This%GetDirectoryPath('LAUNCH')
  SameDirectory     =   WorkingDirectory == LaunchDirectory
  call Logger%Write( "-> WorkingDirectory = ", WorkingDirectory )
  call Logger%Write( "-> LaunchDirectory  = ", LaunchDirectory )
  call Logger%Write( "-> SameDirectory  = ", SameDirectory )


  if ( .Not. SameDirectory ) then
    call Logger%Write( "-> Working directory does not exists => Create it" )

#   ifdef COARRAY
    if ( This_Image() == 1 ) then
#   endif
      call Logger%Write( "-> Calling Command%mkdir <COARRAY: Image 1 only>" )
      call Command%mkdir( WorkingDirectory, Options='-p' )
      if ( Command%CmdStat /= 0 ) call Error%Raise( Command%ErrMsg, ProcName = ProcName )                       ! If an error has occured, then printing an error message and stopping the code

#   ifdef COARRAY
    else
      call Logger%Write( "-> Waiting for image 1 to create the working directory <COARRAY: All images except 1>" )
    end if
#   endif
! #   ifdef COARRAY
!       call Logger%Write( "<SYNCHONIZATION> sync all")
!       sync all                                                                                                    ! REQUIRED ????
! #   endif
  else
    call Logger%Write( "-> Working directory already exists => Nothing to do" )
  end if

!   if ( This_Image() == 1 ) then                                                                                 !@MC
! !     if ( .Not. SameDirectory ) then
!       call Logger%Write( "-> Calling Command%mkdir => ", This%GetDirectoryPath('INPUT') )
!       call Command%mkdir( This%GetDirectoryPath('INPUT'), Options='-p' )                                        ! Creating the Inputs directory inside the result directory
!       if ( Command%CmdStat /= 0 ) call Error%Raise( Command%ErrMsg, ProcName = ProcName )                       ! If an error has occured, then printing an error message and stopping the code
!       call Logger%Write( "-> Calling Command%mkdir => ", This%GetDirectoryPath('OUTPUT') )
!       call Command%mkdir( This%GetDirectoryPath('OUTPUT'), Options='-p' )                                       ! Creating the Output directory inside the result directory
!       if ( Command%CmdStat /= 0 ) call Error%Raise( Command%ErrMsg, ProcName = ProcName )                       ! If an error has occured, then printing an error message and stopping the code
! !     end if
!   end if


!
! # ifdef COARRAY
!   call Logger%Write( "<SYNCHONIZATION> sync all")
!   sync all                                                                                                      ! REQUIRED ????
! # endif

! ==============================================================================================================
!    MOVING TO THE WORKING DIRECTORY AND REOPENING THE LOG FILE
! ==============================================================================================================
! This procedure moves into the working directory and re-opens all opened file into this new directory.
! If the results and the launch directories are the same, then nothing need to be done.
! Otherwise, since we have changed directory, one need to copy the log file(s) into the new directory.
! Moreover, the unit corresponding to the log file must be closed and reopened with the new location of the log file.
! The new unit number affected to the log file must also be set in the CommandLine object.
! For coarray simulations, all images must perform the copy since they all have their local logfile.
! Thuys, there is no conditional 'if' statement on the image index for copying the file, as usual.
! ==============================================================================================================
  if ( This%NewWorkingDirectory ) then
    call Logger%Write( "Moving to the working directory and reopening the log file(s)" )
    call Logger%Write( "-> Calling Command%cd: ", WorkingDirectory )
    call Command%cd( WorkingDirectory )
    if ( Command%CmdStat /= 0 ) call Error%Raise( Command%ErrMsg, ProcName = ProcName )
    do i = 1,Logger%NUnits
      if ( Logger%Units(i)%ToScreen ) cycle
      FileName    =   AddPathToFile( This%GetDirectoryPath('LAUNCH') , Logger%Units(i)%FileName )
      call Logger%Write( "-> Calling Command%cp: '"//FileName//"' => '"//WorkingDirectory//"'" )
      call Command%cp( FileName, WorkingDirectory )
      if ( Command%CmdStat /= 0 ) call Error%Raise( Command%ErrMsg, ProcName = ProcName )
      call Logger%Units(i)%ReOpen()
    end do
    call Error%SetOutputUnit( [Logger%GetUnit(),6] )
  end if
! ==============================================================================================================


  call Logger%Exiting()

End Subroutine

Subroutine ProcessInputsHaltingMode( Input, LogLevel )

  use Input_Library               ,only:  InputReader_Type, InputSection_Type
  use, intrinsic :: ieee_exceptions , only : ieee_invalid, ieee_set_halting_mode

  class(InputReader_Type)                               ,intent(in)     ::  Input
  integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='ProcessInputsHaltingMode'     ! Name of current procedure
  character(*)                                              ,parameter  ::  SectionName = 'HaltingMode'
  type(InputSection_Type)                                               ::  Section
  logical                                                               ::  Found
  character(:)  ,allocatable                                            ::  ParameterName, ParameterValue

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)

  call Logger%Write( "Extracting section '"//SectionName//"' from Inputs" )
  call Logger%Write( "-> Calling Input%GetSection with SectionName = ", SectionName )
  Section   =   Input%GetSection( SectionName=SectionName )
  call Logger%Write( "-> Section%Defined = ", Section%Defined )

  if ( Section%Defined ) then
    call ieee_set_halting_mode(ieee_invalid, .true.)
  end if

  call Logger%Exiting()

End Subroutine



Module Procedure DefineName
  use CommandLineInterface_Class  ,only:  CommandLineInterface_Type
  use String_Library              ,only:  Parse
  type(CommandLineInterface_Type)                                       ::  CommandLineInterface
  character(:)  ,dimension(:)   ,allocatable                            ::  Strings
  call CommandLineInterface%Initialize()
  This%Name   =    CommandLineInterface%Command_Line                            ! GETTING THE ENTIRE COMMAND BY WHICH THE PROGRAM WAS INVOKED
  call Parse( This%Name, " ", Strings )
  This%Name   =   Strings(1)
End Procedure

Module Procedure SetName
  This%Name  =   Name
End Procedure

Module Procedure GetName
  Name       =   This%Name
End Procedure



! **************************************************************************************************************
!                                       HEADER-RELATED PROCEDURES
! **************************************************************************************************************

! This procedures writes a header to the logfile and/or to the screen. The file units where the header is
! written is controled by the optional input arguments 'ToAll', 'ToLogger' and 'ToScreen' which are logical
! variables iondicating if the header should be written to in all units, to the logfile and/pr to the screen.
! By default, the header is written to all units (logfile and screen) as defined by the variable 'WriteHeaderDefault'.
! The header text is given by the type-bound procedure 'GetHeader'.
! If no header need to be written, then nothing is done and the procedure is exited.
! If a header is to be written to the logfile, then it is written at the top of the file using the procedure
! 'Write_At_Top'. If a header is to be written to the screen, then only the 1st image will do the writing is a
! 'coarray run is considered.
Module Procedure WriteHeader

  use File_Library    ,only:  Write_At_Top

  integer                                                               ::  i
  character(*)                                              ,parameter  ::  ProcName='WriteHeader'            !< Name of current procedure
  logical                                                               ::  ToAll_                            !< Indicator for the writing header everywhere
  logical                                                               ::  ToLogger_                         !< Indicator for the writing header to the Logger
  logical                                                               ::  ToScreen_                         !< Indicator for the writing header to the screen
  character(:)  ,allocatable                                            ::  Header(:)

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )

! ==============================================================================================================
!    SETTING INDICATORS WHETHER THE HEADER SHOULD BE WRITTEN AND WHERE
! ==============================================================================================================
  call Logger%Write( "Setting indicators whether the header should be written and where" )
  ToAll_      =   This%GetOptArgValue( WriteHeaderDefault,  ToAll     )
  ToLogger_   =   This%GetOptArgValue( ToAll_,              ToLogger  )
  ToScreen_   =   This%GetOptArgValue( ToAll_,              ToScreen  )
  call Logger%Write( "-> ToAll_    = ", ToAll_    )
  call Logger%Write( "-> ToLogger_ = ", ToLogger_ )
  call Logger%Write( "-> ToScreen_ = ", ToScreen_ )
  if ( (.Not.ToLogger_) .and. (.Not.ToScreen_) ) then
    call Logger%Exiting()
    return
  end if
! ==============================================================================================================


! ==============================================================================================================
!    GETTING THE HEADER TEXT
! ==============================================================================================================
  call Logger%Write( "Getting header string" )
  call Logger%Write( "-> Calling This%GetHeader" )
  call This%GetHeader( Header, LogLevel )
  call Logger%Write( "-> size(Header) = ", size(Header)  )
  call Logger%Write( "-> len(Header)  = ", len(Header) )
  do i = 1,size(Header)
    call Logger%Write( "-> i = ", i, "Header = ", Header(i), Fi="i3" )
  end do
! ==============================================================================================================


! ==============================================================================================================
!    WRITING HEADER TO LOGGER
! ==============================================================================================================
  if (ToLogger_.and.Logger%Initialized) then
    call Logger%Write( "Writing header to Logger" )
    call Write_At_Top( Logger%GetUnit(), Header )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    WRITING HEADER TO SCREEN
! ==============================================================================================================
  if (ToScreen_ .and. This%ip==1 ) then
    call Logger%Write( "Writing header to screen" )
#   ifdef COARRAY
      if ( This_Image() == 1 ) then
        do i = 1,size(Header)
          write(*,"(a)") trim( Header(i) )
        end do
      end if
#   else
      do i = 1,size(Header)
        write(*,"(a)") trim( Header(i) )
      end do
#   endif
  end if
! ==============================================================================================================

  call Logger%Exiting()

End Procedure
!
! Subroutine WorkaroundCharacterAssignment( Inp, Out )
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Inp
!   character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Out
!   Out   =   Inp
! End Subroutine
! Module Procedure SetHeader
Module Subroutine GetHeader( This, Header, LogLevel )

  use String_Library            ,only:  Add_Line_To_String
  use Utilities_Library          ,only:  WorkaroundCharacterAssignment

  class(Application_Type)                               ,intent(inout)  ::  This
  character(:)  ,allocatable                            ,intent(out)    ::  Header(:)
  integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='SetHeader'
  character(:)  ,dimension(:)   ,allocatable                            ::  HeaderLogo
  character(:)  ,dimension(:)   ,allocatable                            ::  HeaderInfo
  character(:)  ,dimension(:)   ,allocatable                            ::  Tmp

    character(:)  ,allocatable  ::  StringTmp(:)
  integer :: i, j

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )


# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
  call Logger%Write( "-> Calling This%GetHeaderLogo" )
  call WorkaroundCharacterAssignment( This%GetHeaderLogo(LogLevel=0), HeaderLogo )
  call Logger%Write( "-> i = ", "HeaderLogo = ", HeaderLogo )

  call Logger%Write( "-> Calling This%GetHeaderInfo" )
  call WorkaroundCharacterAssignment( This%GetHeaderInfo(LogLevel=0), HeaderInfo )
  call Logger%Write( "-> i = ", "HeaderInfo = ", HeaderInfo )

  call Logger%Write( "-> size(HeaderLogo) = ", size(HeaderLogo) )
  call Logger%Write( "-> size(HeaderInfo) = ", size(HeaderInfo) )
  call Logger%Write( "-> size(HeaderLogo)+size(HeaderInfo) = ", size(HeaderLogo)+size(HeaderInfo) )

  call Logger%Write( "-> len(HeaderLogo) = ", len(HeaderLogo) )
  call Logger%Write( "-> len(HeaderInfo) = ", len(HeaderInfo) )
  call Logger%Write( "-> max(len(HeaderLogo),len(HeaderInfo)) = ", max(len(HeaderLogo),len(HeaderInfo)) )

  if ( allocated(Header) ) deallocate(Header)
  allocate( Character(max(len(HeaderLogo),len(HeaderInfo))) :: Header(size(HeaderLogo)+size(HeaderInfo)) )
  do i = 1,size(HeaderLogo)
    Header(i)    =   HeaderLogo(i)
  end do
  do i = 1,size(HeaderInfo)
    j   =   i + size(HeaderLogo)
    Header(j)    =   HeaderInfo(i)
  end do
  call Logger%Write( "-> i = ", "0 Header = ", Header )

  do i = 1,size(HeaderInfo)+size(HeaderLogo)
    call Logger%Write( "-> i = ", i, "Header = ", Header(i) )
  end do
!
!   if ( allocated(This%Header) ) deallocate(This%Header)
!   call WorkaroundCharacterAssignment( Tmp, This%Header )
! !   call move_alloc( Tmp , This%Header )
!   call Logger%Write( "-> i = ", "0 This%Header = ", This%Header )
!
!
!   if ( allocated(This%Header) ) deallocate(This%Header)
!   call Add_Line_To_String( This%Header, HeaderLogo )
! !   call Add_Line_To_String( This%Header, HeaderInfo )
!   call Logger%Write( "-> i = ", "1 This%Header = ", This%Header )
!
!
!   if ( allocated(This%Header) ) deallocate(This%Header)
!   call WorkaroundCharacterAssignment( HeaderLogo, This%Header )
!   call Logger%Write( "-> i = ", "2 This%Header = ", This%Header )
!
!
!   if ( allocated(This%Header) ) deallocate(This%Header)
!   allocate( This%Header, source = HeaderLogo )
!   call Logger%Write( "-> i = ", "3 This%Header = ", This%Header )

!
!   if ( allocated(This%Header) ) deallocate(This%Header)
! !   Block
!     allocate( StringTmp(size(HeaderLogo)), source = HeaderLogo )
! !     This%Header   =   StringTmp
! !     call move_alloc( StringTmp , This%Header )
!     allocate( Character(len(StringTmp)) :: This%Header(size(StringTmp)) )
! !     This%Header(:)  =   StringTmp
!     do i = 1,size(StringTmp)
!       This%Header(i)  =   StringTmp(i)
! !       call Logger%Write( "-> i = ", i, "StringTmp(i) = ", StringTmp(i) )
!       call Logger%Write( "-> i = ", i, "This%Header(i) = ", This%Header(i) )
!     end do
! !           deallocate(This%Header)
! !           allocate( This%Header, source = StringTmp )
! !           deallocate( String_tmp )
! !   call Logger%Write( "-> i = ", "4 StringTmp = ", StringTmp )
! !   End Block
!   call Logger%Write( "-> i = ", "4 This%Header = ", This%Header )

!   call Logger%Write( "-> ** stop ** SetHeader" ); write(*,*) "-> ** stop ** SetHeader"; stop

! !
!   if ( allocated(This%Header) ) deallocate(This%Header)
! !   associate(  OutVar  =>    This%Header , &
! !               InpVar  =>    HeaderLogo    )
!   Block
!           integer                                                               ::  Length
!           character(:)  ,dimension(:)   ,allocatable                            ::  String_tmp
!           if ( .not. allocated(This%Header) ) allocate( character(0) :: This%Header(0) )
!           Length      =   max( len(This%Header), len(HeaderLogo) )
!           allocate( character(Length) :: String_tmp(size(This%Header)+size(HeaderLogo)) )
!           String_tmp(1:size(This%Header))                  =   This%Header
!           String_tmp(size(This%Header)+1:size(String_tmp)) =   HeaderLogo
! !           call move_alloc( String_tmp, This%Header )
!           deallocate(This%Header)
!           allocate( This%Header, source = String_tmp )
!           deallocate( String_tmp )
!   End Block
! !   end associate
!
!
!   call Logger%Write( "-> i = ", "This%Header = ", This%Header )
!
!
!   stop

# else
  call Logger%Write( "Calling This%GetHeaderLogo" )
  HeaderLogo    =   This%GetHeaderLogo( LogLevel )
  call Logger%Write( "-> ", "", HeaderLogo )

  call Logger%Write( "Calling This%GetHeaderInfo" )
  HeaderInfo    =   This%GetHeaderInfo( LogLevel )
  call Logger%Write( "-> ", "", HeaderInfo )

  call Add_Line_To_String( Header, HeaderLogo )
  call Add_Line_To_String( Header, HeaderInfo )
# endif

  call Logger%Write( "-> ", "", Header )

  call Logger%Exiting()

! End Procedure
End Subroutine


! This procedure will return the header logo.
! The logo depends on the presence of the 'figlet' program on the system.
! The 'figlet' program can generate ASCII art text from an input string.
! Module Procedure GetHeaderLogo
Module Function GetHeaderLogo( This, LogLevel ) result( HeaderLogo )

  use SystemCommand_Library     ,only:  SystemCommand_Type
  use String_Library            ,only:  Add_Line_To_String, AddPrefix

  class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
  character(:)  ,allocatable                                            ::  HeaderLogo(:)

  character(*)                                              ,parameter  ::  ProcName='GetHeaderLogo'
  type(SystemCommand_Type)                                              ::  Command
  character(1)                                                          ::  SepChar
  character(:)  ,allocatable                                            ::  CommandName
  character(:)  ,allocatable                                            ::  CorePart(:)
  character(:)  ,allocatable                                            ::  SeparatorLine

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )

  if ( Command%Exists('figlet') ) then
    call Command%ToString( "figlet " // This%GetName(), CorePart )
    SepChar     =   "_"
  else
!     allocate( CorePart, source = [This%GetName()])    ! @COMPILER_BUG: gcc-7.3.0: ICE ON THIS LINE
    CommandName =   This%GetName()
    allocate( character(len(CommandName)) :: CorePart(1) ); CorePart(1) =   This%GetName(); ! Workaround
!     allocate( character(len(This%GetName())) :: CorePart(1) ); CorePart(1) =   This%GetName(); ! Workaround
    SepChar     =   "-"
  end if

  SeparatorLine   =   repeat(SepChar,len(CorePart))
  call Add_Line_To_String( HeaderLogo, "" )
  call Add_Line_To_String( HeaderLogo, SeparatorLine )
  call Add_Line_To_String( HeaderLogo, CorePart )
  call Add_Line_To_String( HeaderLogo, SeparatorLine )
  call Add_Line_To_String( HeaderLogo, "" )
!   HeaderLogo    =   " " // HeaderLogo              ! @COMPILER_BUG: gcc-7.3.0: ICE ON THIS LINE
!   HeaderLogo    =   AddPrefix( HeaderLogo, " " )   ! @COMPILER_BUG: gcc-7.3.0: ICE ON THIS LINE
  Block
    integer                                                               ::  i
    character(:)  ,dimension(:)           ,allocatable                    ::  tmp
    allocate( character(len("  ")+len(HeaderLogo)) :: tmp(size(HeaderLogo)) )
    do i = 1,size(HeaderLogo)
      tmp(i)   =   "  " // HeaderLogo(i)
    end do
    call move_alloc( tmp, HeaderLogo )
  End Block

  call Logger%Write( "-> i = ", "HeaderLogo = ", HeaderLogo )

  call Logger%Exiting()

! End Procedure
End Function


! This procedure will return the header information.
! @TODO: By default write the common info in the header info: OS type and name, user name, hostnam, compielr name and options, execution date
Module Procedure GetHeaderInfo
  use SystemCommand_Library   ,only:  SystemCommand_Type
  use String_Library          ,only:  Add_Line_To_String, SetLength, UpperCase, AddPrefix
  type(SystemCommand_Type)                                              ::  Command
  integer                                                               ::  Length
  integer                                                               ::  Status
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,dimension(:)           ,allocatable                    ::  tmp
  Length    =   35
  if ( Command%Exists('uname') ) then
    call Command%ToString( "uname -o", Value ); call Add_Line_To_String( HeaderInfo, SetLength("Operating System:",Length) // Value )
    call Command%ToString( "uname -r", Value ); call Add_Line_To_String( HeaderInfo, SetLength("Kernel Release:  ",Length) // Value )
    call Command%ToString( "uname -m", Value ); call Add_Line_To_String( HeaderInfo, SetLength("Processor type:  ",Length) // Value )
    call Command%ToString( "uname -n", Value ); call Add_Line_To_String( HeaderInfo, SetLength("Hostname:        ",Length) // Value )
  else
  end if
!   call This%GetEnvVarValue( "HOSTNAME", Value, Status=Status ); if (Status==0) call Add_Line_To_String( HeaderInfo, SetLength("HostName: ",Length) // Value )
  call This%GetEnvVarValue( "USER",     Value, Status=Status ); if (Status==0) call Add_Line_To_String( HeaderInfo, SetLength("User:     ",Length) // Value )
  Value = This%LaunchedCommand; call Add_Line_To_String( HeaderInfo, SetLength("Executable: ",Length) // Value )
  call This%GetEnvVarValue( UpperCase(This%GetName())//"_VERSION", Value, Status=Status ); if (Status==0) call Add_Line_To_String( HeaderInfo, SetLength("Version: ",Length) // Value )
!   HeaderInfo    =   AddPrefix( HeaderInfo, "  " )   ! @COMPILER_BUG: gcc-7.3.0: ICE ON THIS LINE
  Block
    integer                                                               ::  i
    character(:)  ,dimension(:)           ,allocatable                    ::  tmp
    allocate( character(len("  ")+len(HeaderInfo)) :: tmp(size(HeaderInfo)) )
    do i = 1,size(HeaderInfo)
      tmp(i)   =   "  " // HeaderInfo(i)
    end do
    call move_alloc( tmp, HeaderInfo )
  End Block

End Procedure










! @TODO: Remove the output directory if empty
Module Procedure FinalizeConfiguration
  integer                                                               ::  LogLevel_                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='FinalizeConfiguration'       ! Name of current procedure
  LogLevel_   =   This%GetOptArgValue( DefaultLogLevel, LogLevel )                                       ! Setting the leg level
  call Logger%Entering( ProcName, LogLevel=LogLevel_)
  if ( This%GetOptArgValue(WriteTimerDefault,WriteTimer) ) then
    call Logger%Write( "Calling This%StopTimer" )                                                ! Debugging
    call This%StopTimer( Unit=Logger%GetUnit() )                                                                      ! Stoping the timer and outputting
  end if
  This%Initialized     =       .False.
  call Logger%Close()
  call Logger%Free()
  call Logger%Write( "Normal terminaison", NewLine=.True. )                                    ! Debugging
  call Logger%Exiting()
End Procedure



! ==============================================================================================================
!    PROCEDURES RELATED TO THE ENVVAR OBJECT
! ==============================================================================================================

! This procedure initializes the EnvironmentVariable object
Module Procedure InitializeEnvVar
  call This%EnvVar%Initialize( Names, Descriptions )
End Procedure

! This procedure adds an environment variable.
! @TODO: Add an input optional argument: Only_if_absent
Module Procedure AddEnvVar
  call This%EnvVar%Add( Name, Description )
End Procedure

! This procedure gets the value of an environment variable already set.
Module Procedure GetEnvVar
  call This%EnvVar%Get( Name, Value, Description, Mandatory )
End Procedure
! This procedure gets the value of several useful environment variables
! By default, this procedure is empty.
Module Procedure LoadEnvVar
! @TODO: Get default envVar: USER, OSTYPE, HOSTNAME, ...
End Procedure

! This procedure retreives on-the-fly the value of an environment variable.
Module Procedure GetEnvVarValue
  call This%EnvVar%GetValue( Name, Value, Status, ErrMsg )
End Procedure


! ==============================================================================================================
!    PROCEDURES RELATED TO THE DIRTREE OBJECT
! ==============================================================================================================

Module Procedure LoadDirectories
  call This%InitializeDirectories()
End Procedure

Module Procedure InitializeDirectories
  call This%DirTree%Initialize()
End Procedure

Module Procedure GetDirectoriesSummary
  Summary   =   This%DirTree%GetSummary()
End Procedure

Module Procedure AddDirectory
  call This%DirTree%Add( Directory )
End Procedure

Module Procedure UpdateDirectoryPath
  call This%DirTree%UpdatePath( Key, Path )
End Procedure

Module Procedure GetNumberDirectories
  NDirectories  =   This%DirTree%GetNumberDirectories()
End Procedure

Module Procedure GetDirectoryPath
  Path          =   This%DirTree%GetPath( Key, Hide )
End Procedure

Module Procedure GetDirectoryPaths
  call This%DirTree%GetPaths( Key, Paths, Hide )
End Procedure

Module Procedure GetDirectoryFromIndex
  Directory     =   This%DirTree%GetDirectory(iDir)
End Procedure

Module Procedure GetDirectoryFromKey
  integer                                                               ::  iDir
  iDir          =   This%DirTree%GetIndex(Key)
  Directory     =   This%DirTree%GetDirectory(iDir)
End Procedure

Module Procedure GetDirectoriesFromKey
  call This%DirTree%GetDirectories(Key,ListDir)
End Procedure

Module Procedure GetDirectoryIndex
  iDir          =   This%DirTree%GetIndex(Key)
End Procedure

Module Procedure GetDirectoryDesc
  Desc    =   This%DirTree%GetDesc( Key )
End Procedure

Module Procedure SubstituteDirectory
  OutputString  =   trim( This%DirTree%SubstituteDirectory( InputString ) )
End Procedure

Module Procedure IsDirectoryDefined
  IsDefined     =   This%DirTree%IsDefined(Key)
End Procedure

Module Procedure FindFile
  FullName    =   This%DirTree%FindFile( BaseName, DirKeys, RecKeys, Recursive, Mandatory, Found, LogLevel )
End Procedure




! ==============================================================================================================
!    PROCEDURES RELATED TO THE TIMER COMPONENT
! ==============================================================================================================

Module Procedure InitializeTimer
  if ( allocated(This%Timer) ) deallocate( This%Timer )
  allocate( This%Timer )
  call This%Timer%Initialize( Name = Name, Start = Start )
End Procedure

Module Procedure StartTimer
  call This%Timer%Start()
End Procedure

Module Procedure StopTimer
  call This%Timer%Stop()
  if ( present(Unit) ) call This%Timer%Output( Unit=Unit )
End Procedure

Module Procedure OutputTimer
  call This%Timer%Output( Unit )
End Procedure

Module Procedure AddSubTimer
    call This%Timer%AddSubTimer( Name, ID )
End Procedure

Module Procedure StartSubTimer
  call This%Timer%StartSubTimer( ID )
End Procedure

Module Procedure StopSubTimer
  call This%Timer%StopSubTimer( ID )
End Procedure

Module Procedure NextSubTimer
  call This%Timer%NextSubTimer()
End Procedure

End SubModule
