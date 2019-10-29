SubModule(SystemCommand_Class) SystemCommand_SubClass

  use Logger_Class      ,only:  Logger
  use Utilities_Library ,only:  GetOptArgValue

  implicit none

  character(*)  ,parameter              ::  Command_ls              =   "ls"
  character(*)  ,parameter              ::  Command_find            =   "find"
  character(*)  ,parameter              ::  Command_cp              =   "cp"
  character(*)  ,parameter              ::  Command_mv              =   "mv"
  character(*)  ,parameter              ::  Command_cd              =   "cd"
  character(*)  ,parameter              ::  Command_rm              =   "rm"
  character(*)  ,parameter              ::  Command_mkdir           =   "mkdir"
  character(*)  ,parameter              ::  Command_pwd             =   "pwd"
  character(*)  ,parameter              ::  Command_beep            =   "beep"
  character(*)  ,parameter              ::  Command_redirect        =   ">"
  character(*)  ,parameter              ::  Command_suppress_msg    =   "2>/dev/null"
  character(*)  ,parameter              ::  Command_Beep_BeverlyHillsCop = "beep -f 659 -l 460 -n -f 784 -l 340 -n -f 659 -l 230 -n -f 659 -l 110 -n -f 880 -l 230 -n -f 659 -l 230 -n -f 587 -l 230 -n -f 659 -l 460 -n -f 988 -l 340 -n -f 659 -l 230 -n -f 659 -l 110 -n -f 1047-l 230 -n -f 988 -l 230 -n -f 784 -l 230 -n -f 659 -l 230 -n -f 988 -l 230 -n -f 1318 -l 230 -n -f 659 -l 110 -n -f 587 -l 230 -n -f 587 -l 110 -n -f 494 -l 230 -n -f 740 -l 230 -n -f 659 -l 460"
  logical       ,parameter              ::  DefaultDebug            =   .False.

  contains

! This procedure initializes a 'SystemCommand_Type' object.
Module Procedure FinalizeSystemCommand
  call This%Free()
End Procedure

! This procedure initializes a 'SystemCommand_Type' object.
Module Procedure InitializeSystemCommand
  This%NoErr      =   .True.                                                                              ! Setting the output suppression indicator to true: No outputs
  This%FatalError =   .True.                                                                                ! Setting the fatal error indicator to true: Stop code when an error occurs
  This%ExitStat   =   0
  This%CmdStat    =   0
  This%Command    =   ""                                                                                    ! Setting the command string to an empty string: No command loaded
  This%Options    =   ""                                                                                    ! Setting the options string an empty string: No options loaded
  This%ErrMsg     =   ""                                                                                    ! Setting the error message to an empty string: No error
  This%CmdMsg     =   ""                                                                                    ! Setting the error message to an empty string: No error
End Procedure

! This procedure free a 'SystemCommand_Type' object.
Module Procedure FreeSystemCommand
  This%NoErr      =   .True.
  This%FatalError =   .True.
  This%ExitStat   =   0
  This%CmdStat    =   0
  if ( allocated(This%Command) ) deallocate( This%Command )
  if ( allocated(This%Options) ) deallocate( This%Options )
  if ( allocated(This%ErrMsg ) ) deallocate( This%ErrMsg  )
  if ( allocated(This%CmdMsg ) ) deallocate( This%CmdMsg  )
End Procedure

! This procedure raises an error an stop the code.
Module Procedure RaiseErrorSystemCommand
  use Error_Class         ,only:  Error
  use String_Library      ,only:  Convert_To_String
  character(:)  ,allocatable                                            ::  CmdStat_Description
  if ( .Not. This%HasError() ) return
  select case (This%CmdStat)
    case (-3); CmdStat_Description = "The command was not executed due to error in command pre-conditions"
    case (-2); CmdStat_Description = "No error condition occurs but wait is present with the value FALSE and the processor does not support asynchronous execution."
    case (-1); CmdStat_Description = "The processor does not support command line execution."
    case (1:); CmdStat_Description = "An error occured with a processor-dependent positive status value."
    case ( 0); CmdStat_Description = "Successful execution of command."
  end select
  call Error%Add_Line( "-> This%Command  = "//This%Command  )
  call Error%Add_Line( "-> This%CmdStat  = "//Convert_To_String(This%CmdStat)  )
  call Error%Add_Line( "-> Description   = "//CmdStat_Description )
  call Error%Add_Line( "-> This%ExitStat = "//Convert_To_String(This%ExitStat) )
  if ( len_trim(This%CmdMsg) /= 0 ) &
  call Error%Add_Line( "-> This%CmdMsg   = "//This%CmdMsg )
  if ( len_trim(This%ErrMsg) /= 0 ) &
  call Error%Add_Line( "-> This%ErrMsg   = "//This%ErrMsg )
  call Error%Raise( Title = "Error in the 'SystemCommand_Type' object", ProcName=ProcName )
End Procedure

Module Procedure HasError
  Error     =   ( This%ExitStat /= 0 ) .or. ( This%CmdStat /= 0 )
End Procedure

Module Procedure ExecuteCommand
  character( 1000 )                                                     ::  LongString                        ! Temporary variable
  character(:)  ,allocatable                                            ::  Command_
  character(:)  ,pointer                                                ::  InputVar
  if ( present(Command) ) then
    Command_ = Command
  else
    Command_ = This%Command
  end if
  nullify(InputVar)
  Command_    =   Command_//GetVariable( This%StdOutLine, InputVar )
  Command_    =   Command_//GetVariable( This%StdErrLine, InputVar )
  This%CmdMsg   =   ""
  LongString    =   ""
!   call Logger%Write( "[ExecuteCommand] Command_ = ", Command_ )
  call Execute_Command_Line( Command_, ExitStat=This%ExitStat, CmdStat = This%CmdStat, CmdMsg = LongString ) ! Executing the command
!   call Logger%Write( "[ExecuteCommand] This%ExitStat = ", This%ExitStat )
  This%CmdMsg   =   trim(LongString)
End Procedure


Module Procedure GetCommand
  Command   =   This%Command
  Command   =   Command//GetVariable(This%StdOutLine)
  Command   =   Command//GetVariable(This%StdErrLine)
End Procedure


! This procedure return a variable depending on the input arguments.
! The select ion of the output variable is done ad followed:
! - OutputVar = InputVar
!     If the optional input argument "InputVar" is present, then its value
!     return in the output variable "OutputVar".
! - OutputVar = ComponentVar
!     If "InputVar" is absent and the variable "ComponentVar" is allocated,
!     then its value return in the output variable "OutputVar".
! - OutputVar = InputVar
!     If "InputVar" is absent and the variable "ComponentVar" is not
!     allocated, the output variable "OutputVar" is set to an empty string.
Pure Function GetVariable( ComponentVar, InputVar ) result(OutputVar)
  character(:)  ,allocatable                            ,intent(in)     ::  ComponentVar
  character(*)                                ,optional ,intent(in)     ::  InputVar
  character(:)  ,allocatable                                            ::  OutputVar
  if ( present(InputVar) ) then
    OutputVar   =   " "//InputVar
  else
    if ( allocated(ComponentVar) ) then
      OutputVar =   " "//ComponentVar
    else
      OutputVar =   ""
    end if
  end if
End Function



Module Procedure SetCommand
  use Utilities_Library   ,only:  PresentAndTrue

  if ( present(Command) ) This%Command    = trim(Command)                                                                           ! Setting the command string

  if ( present(StdOut ) ) This%StdOutLine = "> " //StdOut

  if ( present(StdErr ) ) This%StdErrLine = "2> "//StdErr

  if ( PresentAndTrue(NoErr) ) This%StdErrLine = "2>/dev/null"

End Procedure

! **************************************************************************************************************
!                                   PROCEDURES EMULATING SYSTEM COMMANDS
! **************************************************************************************************************
! This procedure emulates the 'pwd' command. It returns the path of the current directory.
! Note that the method of reading the 'PWD' environment variable does not work if one has changed directory
Module Procedure Execute_pwd

# ifdef INTEL_COMPILER
  use ifport
# endif

  character(*)                                              ,parameter  ::  ProcName = 'Execute_pwd'          ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character( 1000 )                                                     ::  LongString                        ! Temporary variable

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  if (Dbg) call Logger%Write( "-> FatalError_     = ", FatalError_ )
! ==============================================================================================================


! ==============================================================================================================
!    GETTING THE PATH OF CURRENT DIRECTORY
! ==============================================================================================================
! Note that the method of reading the 'PWD' environment variable does not work if one has changed directory.
! Therefore, the following method should be avoided:
!   if (Dbg) call Logger%Write( "-> Calling Get_Environment_Variable for 'PWD'" )
  call Get_Environment_Variable( 'PWD', LongString, Status=This%CmdStat )                                          ! Getting the current working directory from the environment variable 'PWD'
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Getting the path of current directory" )
  if (Dbg) call Logger%Write( "-> Calling GetCWD')")
  This%CmdStat        =   GetCWD( LongString )                                                                    ! call in the "GETCWD" command line
  Directory     =   trim(LongString)//"/"                                                                 ! Trimming and adding the "/" character at the end of the string
  if (Dbg) call Logger%Write( "-> Directory = ", Directory )
  if ( Directory == "/" ) This%CmdStat = -2
  if ( This%CmdStat /= 0 ) then
    select case (This%CmdStat)
      case (-2); This%ErrMsg = "The environment variable 'PWD' has an empty value."
      case (-1); This%ErrMsg = "The environment variable 'PWD' is present but has a length less than the significant length of the environment variable value."
      case ( 1); This%ErrMsg = "The environment variable does not exist."
      case ( 2); This%ErrMsg = "Processor-dependent error."
    end select
    This%ErrMsg   =  "Error in '"//ProcName//"': "//This%ErrMsg
  end if
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                              ! Raising an error
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

! The command output can be suppressed if required.
! This will adds some instruction at the end of the command to redirect the command output to '/dev/null'
Module Procedure Execute_cp_Single

  use String_Library      ,only:  EscapeFileCharacters, GetFilePath
  use File_Library        ,only:  FileExist

  character(*)                                              ,parameter  ::  ProcName = 'Execute_cp_Single'    ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  FileName
  character(:)  ,allocatable                                            ::  CurrentDirectory                  ! Current working directory
  character(:)  ,allocatable                                            ::  TargetDirectory                   ! Target directory
  character(:)  ,allocatable                                            ::  SourceFile, TargetFile
  character(:)  ,allocatable                                            ::  CmdStr
  type(SystemCommand_Type)                                              ::  Cmd

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                                   ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                      ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting command" )
  if (Dbg) call Logger%Write( "-> Replacing special characters" )
  FileName  =   EscapeFileCharacters( Source )
  CmdStr    =   trim(Command_cp)//" "//trim(This%Options)//" "//trim(FileName)//" "//trim(Target)
  if (Dbg) call Logger%Write( "-> Calling This%Set" )
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
! ==============================================================================================================

! ==============================================================================================================
!    DEALING WITH THE CASE WHEN THE FILES DOES NOT EXIST
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Checking file existence of 'source' file" )
  if ( .not. FileExist(Source) ) then
    if (Dbg) call Logger%Write( "-> File '"//Source//"' does not exist" )
    This%CmdStat  =   -2
    This%ErrMsg   =   "The source file '"//Source//"' to be copied does not exist"
    if (FatalError_) call This%RaiseError(ProcName)
    if (Dbg) call Logger%Exiting()
    return
  end if
  if (Dbg) call Logger%Write( "-> File '"//Source//"' exists" )
! ==============================================================================================================

! ! ==============================================================================================================
! !    CHECKING THAT CURRENT AND TARGET DIRECTORY ARE DIFFERENT
! ! ==============================================================================================================
!   if (Dbg) call Logger%Write( "Checking that current and target directory are different" )
!   SourceFile    =
!   TargetFile
!   if (Dbg) call Logger%Write( "-> Calling Cmd%pwd" )
!   call Cmd%pwd( CurrentDirectory )                                                                             ! Getting the current directory name (actually, it return the full path)
!   if (Dbg) call Logger%Write( "-> CurrentDirectory = ", CurrentDirectory )
!   if (Dbg) call Logger%Write( "-> Calling GetFilePath" )
!   TargetDirectory       =   GetFilePath( FileName )                                                        ! Getting the target directory name
!   if (Dbg) call Logger%Write( "-> TargetDirectory  = ", TargetDirectory )
!   if ( CurrentDirectory == TargetDirectory ) then                                                               ! If current and target directories are identical, then the copy is not required
!     if (Dbg) call Logger%Write( "-> Identical current and target directories => Copy not required" )
!     if (Dbg) call Logger%Exiting()
!     return
!   end if                                                                                                        ! End if case on source file existence
! ! ==============================================================================================================

! ==============================================================================================================
!    COPYING THE FILE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Copying from '"//Source//"' to '"//Target//"'" )
  FileName  =   EscapeFileCharacters( Source )
  CmdStr    =   trim(Command_cp)//" "//trim(This%Options)//" "//trim(FileName)//" "//trim(Target)
  if (Dbg) call Logger%Write( "-> Calling This%Set" )
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
  if (Dbg) call Logger%Write( "-> Calling This%Execute" )
  call This%Execute()                                                                                   ! Executing the command line
  if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"                                                       ! Setting the error message
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================

  if ( Dbg .and. .Not. This%HasError() ) call Logger%Write( "-> Command successful" )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_cp_Multiple

  use String_Library      ,only:  EscapeFileCharacters, Inline

  character(*)                                              ,parameter  ::  ProcName = 'Execute_cp_Multiple'    ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  Sources_(:), Source
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                                   ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting command" )
  if (Dbg) call Logger%Write( "-> Replacing special characters" )
  Sources_  =   EscapeFileCharacters( Sources )
  if (Dbg) call Logger%Write( "-> i = ", "Sources_ = ", Sources_ )
  Source    =   Inline( Sources_, Separator=" " )
  if (Dbg) call Logger%Write( "-> Source = ", Source )
  if (Dbg) call Logger%Write( "-> Calling This%Set" )
  CmdStr    =   trim(Command_cp)//" "//trim(This%Options)//" "//trim(Source)//" "//trim(Target)
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
! ==============================================================================================================


! ==============================================================================================================
!    COPYING THE FILE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Copying to '"//Target//"'" )
  if (Dbg) call Logger%Write( "-> Calling This%Execute" )
  call This%Execute()
  if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)
! ==============================================================================================================

  if ( Dbg .and. .Not. This%HasError() ) call Logger%Write( "-> Command successful" )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_mv

  use String_Library      ,only:  EscapeFileCharacters, GetFilePath
  use File_Library        ,only:  FileExist

  character(*)                                              ,parameter  ::  ProcName = 'Execute_mv'           ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  FileName
  character(:)  ,allocatable                                            ::  CurrentDirectory                  ! Current working directory
  character(:)  ,allocatable                                            ::  TargetDirectory                   ! Target directory
  character(:)  ,allocatable                                            ::  SourceFile, TargetFile
  character(:)  ,allocatable                                            ::  CmdStr
  type(SystemCommand_Type)                                              ::  Cmd

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                                   ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                      ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting command" )
  if (Dbg) call Logger%Write( "-> Replacing special characters" )
  FileName  =   EscapeFileCharacters( Source )
  CmdStr    =   trim(Command_mv)//" "//trim(This%Options)//" "//trim(FileName)//" "//trim(Target)
  if (Dbg) call Logger%Write( "-> Calling This%Set" )
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
! ==============================================================================================================


! ==============================================================================================================
!    DEALING WITH THE CASE WHEN THE FILES DOES NOT EXIST
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Checking file existence of 'source' file" )
  if ( .not. FileExist(Source) ) then
    if (Dbg) call Logger%Write( "-> File '"//Source//"' does not exist" )
    This%CmdStat  =   -2
    This%ErrMsg   =   "The source file '"//Source//"' to be copied does not exist"
    if (FatalError_) call This%RaiseError(ProcName)
    if (Dbg) call Logger%Exiting()
    return
  end if
  if (Dbg) call Logger%Write( "-> File '"//Source//"' exists" )
! ==============================================================================================================


! ==============================================================================================================
!    COPYING THE FILE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Copying from '"//Source//"' to '"//Target//"'" )
  FileName  =   EscapeFileCharacters( Source )
  CmdStr    =   trim(Command_mv)//" "//trim(This%Options)//" "//trim(FileName)//" "//trim(Target)
  if (Dbg) call Logger%Write( "-> Calling This%Set" )
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
  if (Dbg) call Logger%Write( "-> Calling This%Execute" )
  call This%Execute()
  if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================

  if ( Dbg .and. .Not. This%HasError() ) call Logger%Write( "-> Command successful" )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_cd

# ifdef INTEL_COMPILER
  use ifport
# endif

  character(*)                                              ,parameter  ::  ProcName = 'Execute_cd'           ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  logical                                                               ::  CreateDirectory

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )                                          ! Setting the command line options to the input value if present
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  CreateDirectory =   GetOptArgValue( .False., Create )                                                ! Setting the indicator for directory creation
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
    call Logger%Write( "-> CreateDirectory = ", CreateDirectory )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    DEALING WITH WRONG INPUTS
! ==============================================================================================================
  if ( len_trim(Directory)==0 ) then                                                                            ! If the input directory has a zero-length, then error
    This%CmdStat  =   -1                                                                                      ! Setting the error status
    This%ErrMsg   =   "Error Execute_cd: The directory name corresponds to an empty string"                   ! Setting the error message
    if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
    if (Dbg) call Logger%Exiting()
    return
   end if                                                                                                        ! end if case on the length of the input directory
! ==============================================================================================================

! ==============================================================================================================
!    CREATING THE TARGET DIRECTORY IF REQUIRED
! ==============================================================================================================
  if ( CreateDirectory ) then                                                                                   ! If directory can be created
    if (Dbg) call Logger%Write( "Checking if the directory '"//Directory//"' needs to be created" )
    if ( DoesDirectoryExists( Directory ) ) then                                                                ! If the target directory already exists, then no need to create it
      if (Dbg) call Logger%Write( "-> The directory already exists => No need to create it" )
    else                                                                                                        ! If the target directory does not exist, then one needs to create it
      if (Dbg) call Logger%Write( "-> The directory does not exist => Calling This%mkdir" )
      call This%mkdir( Directory, Options='-p' )                                                                ! Creating the target directory
      if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
    end if                                                                                                      ! End if case on directory existence
  end if                                                                                                        ! End if case on directory creation
! ==============================================================================================================


! ==============================================================================================================
!    DEALING WITH ERRORS
! ==============================================================================================================
!   if ( This%HasError() ) then                                                                                      ! If errors in previous command, exiting
    if (Dbg) call Logger%Write( "Errors in previous copmmand => Exiting" )
    if (Dbg) call Logger%Exiting()
!   end if
! ==============================================================================================================


! ==============================================================================================================
!    MOVING INTO THE TARGET DIRECTORY
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Moving into the directory '"//Directory//"'" )
  call This%Initialize()                                                                                      ! Initializing the System-Command object
  if (Dbg) call Logger%Write( "-> Calling chdir" )
  This%CmdStat    =   chdir( Directory )                                                                    ! Changing directory to the input directory
  if ( This%CmdStat /= 0 ) This%ErrMsg = "Error in '"//ProcName//"' while executing CHDIR"                   ! Setting the error message if an error occured
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%HasError() = ", This%HasError() )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_rm

  character(*)                                              ,parameter  ::  ProcName = 'Execute_rm'           ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  ExecuteCommand
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  logical                                                               ::  DryRun_
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )
  DryRun_         =   GetOptArgValue( .False., DryRun )
  if (Dbg) then
    call Logger%Write( "-> This%Options = ", This%Options )
    call Logger%Write( "-> FatalError_  = ", FatalError_ )
    call Logger%Write( "-> DryRun_      = ", DryRun_ )
  end if
! ==============================================================================================================

  ExecuteCommand    =   .True.

  if ( len_trim(Directory)==0 ) then                                                                            ! If the input directory has a zero-length, then error
    This%CmdStat    =   -1                                                                                      ! Setting the error status
    This%ErrMsg     =   "Error Execute_rm: The directory name corresponds to an empty string"                   ! Setting the error message
    ExecuteCommand  =   .False.
    if (Dbg) call Logger%Write( "-> "//This%ErrMsg )
  end if

  if ( trim(Directory) == "/" ) then                                                                            ! If the input directory has a zero-length, then error
    This%CmdStat    =   -1                                                                                      ! Setting the error status
    This%ErrMsg     =   "Error Execute_rm: The directory to be removed corresponds '/'"                         ! Setting the error message
    ExecuteCommand  =   .False.
    if (Dbg) call Logger%Write( "-> "//This%ErrMsg )
  end if

! ==============================================================================================================
!    EXECUTING THE COMMAND LINE
! ==============================================================================================================
  if ( ExecuteCommand ) then
    if (Dbg) call Logger%Write( "Removing directory '"//Directory //"'" )
    CmdStr    =   trim(Command_rm)//" "//trim(This%Options)//" "//trim(Directory)
    if (Dbg) call Logger%Write( "-> Calling This%Set" )
    call This%Set(                      &
            Command   =   CmdStr      , &
            NoErr     =   This%NoErr    )
    if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
    if (Dbg) call Logger%Write( "-> Calling This%Execute" )
    if ( .Not. DryRun_ ) call This%Execute()
    if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"
  end if
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_mkdir

  character(*)                                              ,parameter  ::  ProcName = 'Execute_mkdir'        ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( "-p", Options )                                                  ! Setting the command line options to the input value if present
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
  end if
! ==============================================================================================================


! ==============================================================================================================
!   CREATING THE TARGET DIRECTORY
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Creating the target directory '"//Directory//'"')
  if ( DoesDirectoryExists( Directory ) ) then                                                                  ! If the target directory already exists
    if (Dbg) call Logger%Write( "-> The directory already exists => No need to create it" )
!     if ( trim(This%Options)/='-p') then                                                      ! If the directory already exists and the '-p' option is not set, then it is an error
!       This%ErrMsg         =   "The directory "//trim(Directory)//" already exists"                        ! Setting the error message
!       This%Status         =   - 1                                                                             ! Setting the error status
!     end if
  else                                                                                                          ! If the target directory does not exist, then create it
    if (Dbg) call Logger%Write( "-> The directory does not exist => Creating it" )
    if (Dbg) call Logger%Write( "-> Calling This%Set" )
    CmdStr    =   trim(Command_mkdir)//" "//trim(This%Options)//" "//trim(Directory)
    call This%Set(                      &
            Command   =   CmdStr      , &
            NoErr     =   This%NoErr    )
    if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
    if (Dbg) call Logger%Write( "-> Calling This%Execute" )
    call This%Execute()                                                                                 ! Executing the command line
    if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"                                              ! Setting the error message
  end if                                                                                                        ! End if case on directory existence
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_ls

  use String_Library      ,only:  GetBaseName

  character(*)                                              ,parameter  ::  ProcName = 'Execute_ls'        ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  integer                                                               ::  i, TmpUnit, NFiles, ios, Length
  character(:)  ,allocatable                                            ::  LocalFile                        ! Local file passed to the command
  character(:)  ,allocatable                                            ::  FileName, BaseName
  character(:)  ,allocatable                                            ::  CmdStr
  character(1000)                                                       ::  LongString

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( "-1", Options )                                                  ! Setting the command line options. Option "-1" is for one file per line
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  LocalFile       =   GetOptArgValue( '', File )                                                       ! Setting the 'File' passed to the ls command
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
    call Logger%Write( "-> LocalFile      = ", LocalFile )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    OPENING A TEMPORARY FILE TO STORE THE OUTPUT OF THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Openning temporary file for writing ls output" )
# ifdef GCC_COMPILER
  FileName  =   GetTemporaryFileName()
  open( NewUnit=TmpUnit, File=FileName, Status="NEW" )
# else
    open( NewUnit=TmpUnit, Status='SCRATCH' )
    allocate( character(1000) :: FileName )
    inquire(Unit=TmpUnit, NAME=FileName)
    FileName = trim(FileName)
# endif
  if (Dbg) call Logger%Write( "-> FileName = ", FileName )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND AND EXECUTING IT
! ==============================================================================================================
  CmdStr    =   trim(Command_ls)//" "//trim(This%Options )//" "//trim(LocalFile)
  if (Dbg) call Logger%Write( "Calling This%Set" )
  call This%Set(                                &
          Command   =   CmdStr                , &
          StdOut    =   FileName              , &
          NoErr     =   This%NoErr   )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
  if (Dbg) call Logger%Write( "Calling This%Execute" )
  call This%Execute()                                                                                   ! Executing the command line
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if

! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE ERROR MESSAGE IF ANY
! ==============================================================================================================
! When File is absent from the current directory, the ls command gives the following error
! ls: impossible d'accéder à "FILE": Aucun fichier ou dossier de ce type
! In such cases, the error status is non-zero.
! In order to allow to use ls when no file is expected, one impose  a zero error status.
! @TODO: This should be corrected... maybe there is an option to ls
! ==============================================================================================================
!   This%Status   =   0
!   This%ErrMsg   =   ""                                                                                      ! Initializing the error message to an empty string
  if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"                                                   ! Setting the error message if an error occured
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================


! ==============================================================================================================
!    GETTING THE NUMBER OF FILES AND THE MAXIMUM LENGTH
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Getting number of files and maximum length" )
  Length    =   0
  NFiles    =   0
  rewind(TmpUnit)
  do
    read(TmpUnit,"(a)",iostat=ios) LongString
    if ( ios /= 0 ) exit
    BaseName  =   trim(GetBaseName(LongString))
    if ( BaseName == FileName ) cycle
    NFiles    =   NFiles+ 1
    Length    =   max( Length, len(BaseName) )
    if (Dbg) call Logger%Write( "-> i = ", NFiles, "Length = ", Length, "String = ", trim(LongString) )
  end do
  if (Dbg) call Logger%Write( "-> NFiles = ", NFiles)
  if (Dbg) call Logger%Write( "-> Length = ", Length )
! ==============================================================================================================

! ==============================================================================================================
!    SETTING FILENAME IF OUTPUT VARIABLES
! ==============================================================================================================
  allocate( character(Length) :: ListFiles(NFiles) )
  rewind(TmpUnit)

  !i = 1,NFiles

  i     =   0
  do
    i   =   i + 1
    if ( i > NFiles ) exit
    read(TmpUnit,"(a)",iostat=ios) LongString
    BaseName  =   trim(GetBaseName(LongString))
    if ( BaseName == FileName ) then
      i   =   i - 1
      cycle
    end if
    ListFiles(i)  =   BaseName
    if (Dbg) call Logger%Write( "-> i = ", i, "ListFiles(i) = ", ListFiles(i) )
  end do
  if (Dbg) call Logger%Write( "Closing and deleting the file ", FileName )
  close( TmpUnit, Status='DELETE' )
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_find

  use String_Library      ,only:  GetBaseName
  use Utilities_Library   ,only:  PresentAndTrue
  use File_Library        ,only:  GetAbsolutePath

  character(*)                                              ,parameter  ::  ProcName = 'Execute_find'         ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  integer                                                               ::  TmpUnit
  integer                                                               ::  NFiles
  integer                                                               ::  i
  integer                                                               ::  ios                                     ! Input/Output status
  integer                                                               ::  Length                            ! Length of strings in the output list of files
  character(1000)                                                       ::  LongString
  character(:)  ,allocatable                                            ::  Path_Loc
  character(:)  ,allocatable                                            ::  Expression_Loc
  character(:)  ,allocatable                                            ::  FileName
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  This%Options    =   GetOptArgValue( This%Options, Options )                                          ! Setting the command line options to the input value if present
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  Path_Loc        =   GetOptArgValue( ".", Path          )
  Expression_Loc  =   GetOptArgValue( "*", Expression    )
  if (Dbg) then
    call Logger%Write( "-> This%Options    = ", This%Options )
    call Logger%Write( "-> FatalError_     = ", FatalError_ )
    call Logger%Write( "-> Path_Loc        = ", Path_Loc )
    call Logger%Write( "-> Expression_Loc  = ", Expression_Loc )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    OPENING A TEMPORARY FILE TO STORE THE OUTPUT OF THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Openning temporary file for writing ls output" )
# ifdef GCC_COMPILER
  FileName  =   GetTemporaryFileName()
  open( NewUnit=TmpUnit, File=FileName, Status="NEW" )
# else
    open( NewUnit=TmpUnit, Status='SCRATCH' )
    allocate( character(1000) :: FileName )
    inquire(Unit=TmpUnit, NAME=FileName)
    FileName = trim(FileName)
# endif
  if (Dbg) call Logger%Write( "-> FileName = ", FileName )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND AND EXECUTING IT
! ==============================================================================================================
  CmdStr    =   trim(Command_find)//" "//trim(Path_Loc)//" "//This%Options//" -name "//trim(Expression_Loc)
  if (Dbg) call Logger%Write( "Calling This%Set" )
  call This%Set(                                &
          Command   =   CmdStr                , &
          StdOut    =   FileName              , &
          NoErr     =   This%NoErr   )
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
  if (Dbg) call Logger%Write( "Calling This%Execute" )
  call This%Execute()                                                                                   ! Executing the command line
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE ERROR MESSAGE IF ANY
! ==============================================================================================================
  if ( This%HasError() ) This%ErrMsg = "Error in '"//ProcName//"'"                                  ! Setting the error message if an error occured
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================


! ==============================================================================================================
!   Getting the number of files and the maximum length
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Getting the number of files and the maximum length" )
  Length        =   0
  NFiles        =   0
  rewind(TmpUnit)
  do
    read(TmpUnit,"(a)",iostat=ios) LongString
    if ( ios /= 0 ) exit
    NFiles     =   NFiles + 1
    Length      =   max( Length, len_trim(LongString) )
  end do
  if (Dbg) call Logger%Write( "-> Number of files: NFiles = ", NFiles )
  if (Dbg) call Logger%Write( "-> Length of files: Length = ", Length )
! ==============================================================================================================


! ==============================================================================================================
!    Reading the filenames and storing them in the output array
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Reading the filenames and storing them in the output array" )
  allocate( character(Length) :: ListFiles(NFiles) )
  rewind(TmpUnit)
  do i = 1,NFiles
    read(TmpUnit,"(a)",iostat=ios) LongString
    ListFiles(i)   =   LongString
    if (Dbg) call Logger%Write( "-> i = ", i, "ListFiles(i) = ", ListFiles(i) )
  end do
! ==============================================================================================================


! ==============================================================================================================
!    CLOSING AND DELETING THE TEMPORARY FILE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Closing and deleting temporary file" )
  close( TmpUnit, Status='DELETE' )
! ==============================================================================================================


  if ( PresentAndTrue(AbsolutePath) ) then
    if (Dbg) call Logger%Write( "Setting absolute path" )
#   ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     @COMPILER_BUG: gcc-7.3.0: ICE
      Block
        character(:)  ,allocatable  ::  ListFilesTmp(:)
        allocate( ListFilesTmp, source = GetAbsolutePath(ListFiles) )
        call move_alloc( ListFilesTmp, ListFiles )
      End Block
#   else
      if (Dbg) call Logger%Write( "-> Before: ListFiles = ", ListFiles )
      ListFiles   =   GetAbsolutePath(ListFiles)
      if (Dbg) call Logger%Write( "-> After:  ListFiles = ", ListFiles )
#   endif
  end if


  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Execute_beep

  character(*)                                              ,parameter  ::  ProcName = 'Execute_beep'         ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  if (Dbg) call Logger%Write( "-> FatalError_     = ", FatalError_ )
! ==============================================================================================================

  if (Dbg) call Logger%Write( "Calling This%Set" )
  CmdStr    =   trim(Command_beep)//" "//trim("")
  call This%Set(                      &
          Command   =   CmdStr      , &
          NoErr     =   This%NoErr    )                                                   !
  if (Dbg) call Logger%Write( "-> This%Command = ", This%Command )
  if (Dbg) call Logger%Write( "Calling This%Execute" )
  call This%Execute()                                                                                   ! Executing the command line
  if (Dbg) then
    call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
    call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
  end if
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error

  if (Dbg) call Logger%Exiting()

End Procedure

! **************************************************************************************************************
!                                   PROCEDURES FOR COMPLEX OPERATIONS (GROUPS OF COMMANDS)
! **************************************************************************************************************
Module Procedure DoesDirectoryExists
  character(*)                                              ,parameter  ::  ProcName = 'DoesDirectoryExists'  ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  CurrentDirectory                  ! Current working directory
  type(SystemCommand_Type)                                              ::  Command                           ! Command object

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call Command%Initialize()                                                                                     ! Initializing the System-Command object

  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  FatalError_     =   GetOptArgValue( .False., FatalError )                                            ! Setting the fatal error indictor
  if (Dbg) call Logger%Write( "-> FatalError_     = ", FatalError_ )

  if (Dbg) call Logger%Write( "Calling Command%pwd" )
  call Command%pwd( CurrentDirectory )                                                                          ! Getting the current directory name (actually, it return the full path)
  if (Dbg) call Logger%Write( "-> CurrentDirectory = ",  CurrentDirectory )

  if (Dbg) call Logger%Write( "Calling Command%cd: Directory = ", Directory )
  call Command%cd( Directory, FatalError=.False. )                                                              ! Changing directory to the "Directory" directory
  DirectoryExist    =   .Not. Command%HasError()                                                               ! If no error, then the directory exists
  if (Dbg) call Logger%Write( "-> DirectoryExist = ", DirectoryExist )

  if (Dbg) call Logger%Write( "Calling Command%cd: Directory = ", CurrentDirectory )
  call Command%cd( CurrentDirectory, FatalError=.False. )                                                       ! Going back to the initial working directory
  if (FatalError_) call Command%RaiseError(ProcName)

  if (Dbg) call Logger%Exiting()

End Procedure

! This procedure find a file according to a given pattern and copy it into an other directory.
! If the directory where the file is to be compied doesn't exist, then itis created.
Module Procedure Select_and_Copy_File

  use String_Library  ,only:  GetFilePath

  character(*)                                              ,parameter  ::  ProcName='Select_and_Copy_File'   ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  character(:)  ,allocatable                                            ::  Local_SourceDir
  character(:)  ,allocatable                                            ::  Local_TargetFile                      ! Name of a file or a path where a given file has to be copied
  character(:)  ,allocatable                                            ::  Local_File_Pattern
  character(:)  ,allocatable                                            ::  Local_File_type
  character(:)  ,allocatable                                            ::  FileName                          ! Name of a file or a pattern
  character(:)  ,allocatable                                            ::  TargetDirectory                   ! Target directory
!   character(:)  ,allocatable                                            ::  ErrMsg                            ! Error message
  character(:)  ,dimension( : ) ,allocatable                            ::  ListFiles                         ! List of files satisfying a given pattern in the working directory

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

! ==============================================================================================================
!    PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  FatalError_         =   GetOptArgValue( This%FatalError, FatalError )                                ! Setting the fatal error indictor
  Local_SourceDir     =   GetOptArgValue( "", SourceDir )                                              ! Setting the
  Local_TargetFile    =   GetOptArgValue( "", TargetFile )                                             ! Setting the
  Local_File_type     =   GetOptArgValue( "", File_Type )                                              ! Setting the
  Local_File_Pattern  =   GetOptArgValue( "", File_Pattern )                                           ! Setting the
  if (Dbg) then
    call Logger%Write( "-> FatalError_         =", FatalError_        )
    call Logger%Write( "-> Local_SourceDir     =", Local_SourceDir    )
    call Logger%Write( "-> Local_TargetFile    =", Local_TargetFile   )
    call Logger%Write( "-> Local_File_type     =", Local_File_type    )
    call Logger%Write( "-> Local_File_Pattern  =", Local_File_Pattern )
  end if
! ==============================================================================================================

  FileName              =   Local_SourceDir//Local_File_Pattern                                            ! Setting the name of file (including pattern) to be found by the "ls" command

! Create the target directory if it doesn't already exists
  TargetDirectory       =   GetFilePath( Local_TargetFile )
  if (Dbg) call Logger%Write( "Calling This%mkdir: ", TargetDirectory )
  call This%mkdir( TargetDirectory, Options='-p' )                  ! Creating the Inputs directory inside the result directory
!   if ( This%HasError() ) call Error%Raise( This%ErrMsg, ProcName=ProcName )                                ! If an error has occured, then printing an error message and stopping the code

!   if (Dbg) call Logger%Write( "Searching a files with the pattern ", FileName )
  if (Dbg) call Logger%Write( "Calling This%ls: ", FileName )
  call This%ls( ListFiles, File=FileName )                                     ! Creating the list of input files (files which have the .inp extension)
!   if (Dbg) call Logger%Write( "ListFiles: ", ListFiles )
!   if (Dbg) call Logger%Write( "Number of files found: ", size(ListFiles) )
!   if ( This%HasError() ) call Error%Raise( This%ErrMsg, ProcName=ProcName )                                         ! If an error has occured, then printing an error message and stopping the code

  select case (size(ListFiles))                                                                                 ! Selecting action according to the number of input files found in the local directory
    case (1)                                                                                                    ! If one input file have been found
      FileName        =   Local_SourceDir//ListFiles(1)                                                       ! Setting the name of the file to be copied (the directory is added in front of the file name)
      if (Dbg) call Logger%Write( "Calling This%cp: "//FileName//" => "//Local_TargetFile )
      call This%cp( FileName, Local_TargetFile )                                   ! Copying the file
!       if ( This%HasError() ) call Error%Raise( This%ErrMsg, ProcName=ProcName )                                     ! If an error has occured, then printing an error message and stopping the code
    case (2:)                                                                                                   ! If several input files have been found
      FileName        =   Local_SourceDir//Select_File( ListFiles, File_Type=Local_File_type )                        ! Setting the name of the file to be copied (the directory is added in front of the file name): Interactive selection
      if (Dbg) call Logger%Write( "Calling This%cp: "//FileName//" => "//Local_TargetFile )
      call This%cp( FileName, Local_TargetFile )                                   ! Copying the file
!       if ( This%HasError() ) call Error%Raise( This%ErrMsg, ProcName=ProcName )                                     ! If an error has occured, then printing an error message and stopping the code
    case default                                                                                                ! Default case: no such file
      if (Dbg) call Logger%Write( "No file of type: ", Local_File_type, Warning=.True. )
  end select                                                                                                    ! End of select case on number of files

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Select_File

  character(:)  ,allocatable                                            ::  Prompt
  integer                                                               ::  iFile
  integer                                                               ::  ios
  integer                                                               ::  NFile
  logical                                                               ::  i_File_Selected                   ! file selection indicator
  NFile =   size(ListFiles)
  Prompt        =   "Select a file number"
  if ( present(File_Type) ) then
    if ( len_trim(File_Type) /= 0 ) Prompt = trim(Prompt)//" ("//trim(File_Type)//" file)"
  end if
  i_File_Selected       =   .False.                                                                         ! Initializing the file selection indicator to false
  do                                                                                                            ! Infinit loop for file selection (loop is run until a correct file is selected)
    if (i_File_Selected) exit                                                                                   ! If the file has been selected, then exiting the loop
    write(*,"(/,2x,a)") Prompt                                                                                  ! Wrtting on screen the prompt
    do iFile = 1,NFile                                                                                          ! Loop on all files in the list
      write(*,"(4x,i0,') ',a)") iFile, ListFiles(iFile)                                                         ! Writing on screen current file
    end do                                                                                                      ! End loop on files
    read(*,*,iostat=ios) iFile                                                                                  ! Reading from keyboard the index of the selected file
    if ( (ios==0) .and. ((iFile>=1).and.(iFile<=NFile)) ) then                                                  ! If no error when reading and if the file index is within the expected range
      i_File_Selected   =   .True.                                                                          ! Setting the file selection indicator th true so that the loop in exited
    else
      write(*,"(/,2x,'Bad input value (ios=',i0,')')") ios
      write(*,"(2x,'The expected value is a integer in the range [',i0,':',i0,']')") 1, NFile
      write(*,"(2x,'Try again.')")
    end if
  end do                                                                                                        ! End loop on file selection
  Prompt        =   "Your choice is: "
  write(*,"(/,2x,a,i0,' => ',a,/)") Prompt, iFile, ListFiles(iFile)                                               ! Writing on screen current file
  File  =   ListFiles(iFile)                                                                                ! Storing the selected file in the output variable
End Procedure



Module Procedure DoesCommandExist

  type(SystemCommand_Type)                                              ::  Command                           ! Command object
  character(*)                                              ,parameter  ::  ProcName='DoesCommandExist'       ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  character(:)  ,allocatable                                            ::  CmdStr

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )


  if (Dbg) call Logger%Write( "Checking if command '"//CommandName//"' exists" )

! All of these should be equivalent
  CmdStr    =   "type "//CommandName
!   CmdStr    =   "hash "//CommandName
!   CmdStr    =   "command -v "//CommandName\

  CmdStr    =   CmdStr//" &> /dev/null"

  call Command%Initialize()
  call Command%Set( Command=CmdStr )
  call Command%Execute()
  CommandExist    =   Command%ExitStat == 0
  if (Dbg) then
    call Logger%Write( "-> Command%CmdStat  = ", Command%CmdStat )
    call Logger%Write( "-> Command%ExitStat = ", Command%ExitStat )
    call Logger%Write( "-> CommandExist     = ", CommandExist )
  end if

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure CommandToString
  character(*)                                              ,parameter  ::  ProcName = 'CommandToString'        ! Name of current procedure
  logical                                                               ::  Dbg                         ! Local debugging indicator
  character(:)  ,allocatable                                            ::  Strings(:)
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  call This%ToString( Command, Strings, FatalError, Debug )
  Output  =   ""
  if ( allocated(Strings) ) Output  =   Strings(1)
  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure CommandToStrings

  use String_Library      ,only:  Convert_Ratio

  character(*)                                              ,parameter  ::  ProcName = 'CommandToStrings'        ! Name of current procedure
  logical                                                               ::  Dbg                            ! Local debugging indicator
  logical                                                               ::  FatalError_                       ! Local fatal error indicator
  integer                                                               ::  i
  integer                                                               ::  ios                                     ! Input/Output status
  integer                                                               ::  TmpUnit                           ! Unit of the temporary file used to store the command output
  integer                                                               ::  NumberOfLines                     ! Number of lines of the command output
  integer                                                               ::  Length                            ! Maximum length of the command output
  character(:)  ,allocatable                                            ::  FileName                      ! Name of the temporary file used to store the command output
  character(:)  ,allocatable                                            ::  CmdStr
  character(1000)                                                       ::  LongString                        ! Long string used to store each line of the command output

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)                                              ! Setting local debugging indicator
  if (Dbg) call Logger%Entering( ProcName )

  call This%Initialize()                                                                                        ! Initializing the System-Command object

  if (Dbg) call Logger%Write( "Processing optional input arguments" )
  FatalError_     =   GetOptArgValue( This%FatalError, FatalError )                                    ! Setting the fatal error indictor
  if (Dbg) call Logger%Write( "-> FatalError_     = ", FatalError_ )


! ==============================================================================================================
!    OPENING A TEMPORARY FILE TO STORE THE OUTPUT OF THE COMMAND
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Openning temporary file for writing output" )
# ifdef GCC_COMPILER
  FileName  =   GetTemporaryFileName()
  open( NewUnit=TmpUnit, File=FileName, Status="NEW" )
# else
    open( NewUnit=TmpUnit, Status='SCRATCH' )
    allocate( character(1000) :: FileName )
    inquire(Unit=TmpUnit, NAME=FileName)
    FileName = trim(FileName)
# endif
  if (Dbg) call Logger%Write( "-> FileName = ", FileName )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE COMMAND AND EXECUTING IT
! ==============================================================================================================
  CmdStr    =   trim(Command)
  if (Dbg) call Logger%Write( "Setting the command string" )
  call This%Set(                                &
          Command   =   CmdStr                , &
          StdOut    =   FileName              , &
          NoErr     =   This%NoErr   )
  if (Dbg) call Logger%Write( "-> This%Command = ",This%Command )

! ==============================================================================================================
  if (Dbg) call Logger%Write( "Executing the command" )
  if (Dbg) call Logger%Write( "-> Calling This%Execute" )
  call This%Execute()
  if (Dbg) call Logger%Write( "-> This%CmdStat  = ", This%CmdStat )
  if (Dbg) call Logger%Write( "-> This%ExitStat = ", This%ExitStat )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE ERROR MESSAGE IF ANY
! ==============================================================================================================
! When File is absent from the current directory, the ls command gives the following error
! ls: impossible d'accéder à "FILE": Aucun fichier ou dossier de ce type
! In such cases, the error status is non-zero.
! In order to allow to use ls when no file is expected, one impose  a zero error status.
! @TODO: This should be corrected... maybe there is an option to ls
! ==============================================================================================================
!   This%Status   =   0
!   This%ErrMsg   =   ""                                                                                      ! Initializing the error message to an empty string
  if ( This%HasError() ) This%ErrMsg = "Error in procedure '"//ProcName//"'"                                                   ! Setting the error message if an error occured
  if (FatalError_) call This%RaiseError(ProcName)                                                            ! Raising an error
! ==============================================================================================================


! ==============================================================================================================
!    Loading the command output to the output string
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Loading the command output to the output string" )
  Length        =   0
  NumberOfLines =   0
  rewind(TmpUnit)
  do
    read(TmpUnit,"(a)",iostat=ios) LongString
    if ( ios /= 0 ) exit
    NumberOfLines =   NumberOfLines+ 1
    Length        =   max( Length, len_trim(LongString) )
  end do
  allocate( character(Length) :: Output(NumberOfLines) )
  if (Dbg) call Logger%Write( "-> NumberOfLines = ", NumberOfLines )
  if (Dbg) call Logger%Write( "-> Length        = ", Length )

  rewind(TmpUnit)
  do i = 1,NumberOfLines
    read(TmpUnit,"(a)",iostat=ios) LongString
    Output(i)   =   trim(LongString)
    if (Dbg) call Logger%Write( " -> Line "//Convert_Ratio(i,NumberOfLines)//": "//Output(i) )
  end do
  if (Dbg) call Logger%Write( "Closing and deleting temporary file '"//FileName//"'" )
  close( TmpUnit, Status='DELETE' )
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure




! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

! Function Does_File_Exist( FileName, FileUnit ) result(i_Exist)
!   character(*)                                ,optional ,intent(in)     ::  FileName                          !< Name of file to be checked for existence
!   integer                                     ,optional ,intent(in)     ::  FileUnit                          !< Unit of file to be checked for existence
!   logical                                                               ::  i_Exist                           !< Existence indicator of the input file
!   if ( present(FileName) ) then                                                                                 ! If the file name is provided in input arguments
!     inquire( File=FileName, Exist=i_Exist )                                                                     ! Setting the file existence indicator
!   else if ( present(FileUnit) ) then                                                                            ! If the file unit is provided in input arguments
!     inquire( Unit=FileUnit, Exist=i_Exist )                                                                     ! Setting the file existence indicator
!   else
!     stop "Error in Does_File_Exist procedure: either the FileName of FileUnit optional argument must be specified"
!   end if
! End Function

Function GetTemporaryFileName( BaseName ) result(FileName)
  use Utilities_Library   ,only:  GetOptArgValue
  use String_Library      ,only:  Convert_To_String
  character(*)                                ,optional ,intent(in)     ::  BaseName
  character(*)                                              ,parameter  ::  DefaultBaseName = "forpack"
  character(*)                                              ,parameter  ::  FileExtension   = ".tmp"
  logical                                                               ::  FileExist
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  FileName, BaseName_, FileSuffix
  BaseName_     =   GetOptArgValue( DefaultBaseName, BaseName )
  FileSuffix    =   ""
  i             =   0
  do
    FileName    =   BaseName_//FileSuffix//FileExtension
    inquire( File=FileName, Exist=FileExist )
    if ( .Not. FileExist ) exit
    i           =   i + 1
    FileSuffix  =   "-"//Convert_To_String(i)
  end do
End Function

End SubModule
