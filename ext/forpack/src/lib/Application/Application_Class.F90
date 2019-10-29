Module Application_Class

  use Object_Class                ,only:  Object_Type
  use Timer_Library               ,only:  Timer_Type
  use Directory_Class             ,only:  Directory_Type
  use DirectoriesContainer_Class  ,only:  DirectoriesContainer_Type
  use EnvironmentVariable_Library ,only:  EnvironmentVariable_Type
  use CommandLineInterface_Class  ,only:  CommandLineInterface_Type

  implicit none

  private
  public    ::  Application_Type

  Type  ,extends(Object_Type)               ::  Application_Type
    private
    logical          ,public                ::  Initialized = .False.
    character(:)     ,public  ,allocatable  ::  Name
!     character(:)              ,allocatable  ::  Header(:)
    type(Timer_Type)          ,allocatable  ::  Timer
    type(DirectoriesContainer_Type) ,public ::  DirTree
    type(EnvironmentVariable_Type)          ::  EnvVar
    type(CommandLineInterface_Type)         ::  CLI
    character(:)      ,allocatable ,public  ::  LaunchedCommand
    character(:)      ,allocatable          ::  Compiler_Version
    character(:)      ,allocatable          ::  Compiler_Options


    logical          ::  NewWorkingDirectory = .False.
    integer :: ip=1, Np=1

  contains
    private
    procedure   ,public         ::  Init    =>    InitializeApplication
!     procedure                   ::  InitializeLogger
    procedure   ,public         ::  ProcessInputs
    procedure   ,public         ::  IsInitialized
    procedure   ,public         ::  GetName
    procedure   ,public         ::  LoadConfig
!   Procedures related to the command-line-arguments object
    procedure   ,public         ::  LoadCmdLineArg
    procedure   ,public         ::  ProcessCmdLineArg
    procedure   ,public         ::  GetNumberCmdLineArg
    procedure   ,public         ::  GetCmdLineArgNameValue
!   Procedures related to the EnvVar object
    procedure   ,public         ::  LoadEnvVar
    procedure   ,public         ::  InitializeEnvVar
    procedure   ,public         ::  AddEnvVar
    procedure   ,public         ::  GetEnvVar
    procedure   ,public         ::  GetEnvVarValue
!   Procedures related to the DirTree object
    procedure   ,public         ::  LoadDirectories
    procedure   ,public         ::  InitializeDirectories
    procedure   ,public         ::  GetDirectoriesSummary
    procedure   ,public         ::  AddDirectory
    procedure   ,public         ::  UpdateDirectoryPath
    procedure   ,public         ::  GetNumberDirectories
    procedure   ,public         ::  GetDirectoryPath
    procedure   ,public         ::  GetDirectoryPaths
    generic     ,public         ::  GetDirectory    =>  GetDirectoryFromIndex, GetDirectoryFromKey
    procedure   ,public         ::  GetDirectories  =>  GetDirectoriesFromKey
    procedure   ,private        ::  GetDirectoryFromIndex
    procedure   ,private        ::  GetDirectoryFromKey
    procedure   ,public         ::  GetDirectoryIndex
    procedure   ,public         ::  GetDirectoryDesc
    procedure   ,public         ::  SubstituteDirectory
    procedure   ,public         ::  IsDirectoryDefined
    procedure   ,public         ::  FindFile
!   Procedures related to the header object
    procedure   ,public         ::  WriteHeader
    procedure   ,public         ::  GetHeader
    procedure   ,public         ::  GetHeaderLogo
    procedure   ,public         ::  GetHeaderInfo
!   Procedures related to the Timer object
    procedure   ,public         ::  InitializeTimer
    procedure   ,public         ::  StartTimer
    procedure   ,public         ::  StopTimer
    procedure   ,public         ::  OutputTimer
    procedure   ,public         ::  AddSubTimer
    procedure   ,public         ::  StartSubTimer
    procedure   ,public         ::  StopSubTimer
    procedure   ,public         ::  NextSubTimer
    procedure   ,public         ::  SetName
    procedure   ,public         ::  DefineName
!     procedure(ParameterDefinition) ,deferred ::  DefineName
  End type

!   Abstract Interface
!     Pure Elemental Subroutine ParameterDefinition( This )
!       import  ::  Application_Type
!       class(Application_Type)                     ,intent(inout)  ::  This
!     End Subroutine
!   End Interface

  Interface
    Module Subroutine InitializeApplication( This, LoggerInitialize, LoggerName, LoggerProcPath, LoggerIndentation, ip,np, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This
      logical                                     ,optional ,intent(in)     ::  LoggerInitialize
      character(*)                                ,optional ,intent(in)     ::  LoggerName
      character(*)                                ,optional ,intent(in)     ::  LoggerProcPath              !< Names of the calling procedures to reach the current procedure. Each procedure neames are separated by the '>' character.  Only used by the Logger object
      integer                                     ,optional ,intent(in)     ::  LoggerIndentation
      integer                                     ,optional ,intent(in)     ::  ip,np
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
    End Subroutine
!
!     Module Subroutine InitializeLogger( This, LoggerName, LoggerProcPath, LoggerIndentation )
!       class(Application_Type)                               ,intent(inout)  ::  This
!       character(*)                                ,optional ,intent(in)     ::  LoggerName
!       character(*)                                ,optional ,intent(in)     ::  LoggerProcPath
!       integer                                     ,optional ,intent(in)     ::  LoggerIndentation
!     End Subroutine

    Pure Elemental Module Function IsInitialized( This ) result(Indicator)
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical                                                               ::  Indicator                       !< Indicator whether the object has been initialized
    End Function

    Module Subroutine LoadConfig( This, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine

    Module Subroutine ProcessInputs( This, Input, LogLevel )
      use Input_Library               ,only:  InputReader_Type
      class(Application_Type)                               ,intent(inout)  ::  This
      class(InputReader_Type)                               ,intent(inout)  ::  Input
      integer                                     ,optional ,intent(inout)  ::  LogLevel
    End Subroutine

    Module Subroutine DefineName( This )
      class(Application_Type)                               ,intent(inout)  ::  This
    End Subroutine
    Pure Module Subroutine SetName( This, Name )
      class(Application_Type)                               ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
    End Subroutine

    Pure Module Function GetName( This ) result(Name)
      class(Application_Type)                               ,intent(in)     ::  This                          !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Name                          !< Name of the project

    End Function


    ! **************************************************************************************************************
    !                             PROCEDURES RELATED TO THE COMMAND-LINE-argumentS OBJECT
    ! **************************************************************************************************************

    Module Subroutine LoadCmdLineArg( This, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                          !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
    End Subroutine

    Module Subroutine ProcessCmdLineArg( This, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                          !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
    End Subroutine

    Module Function GetNumberCmdLineArg( This ) result( NArg )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                               ::  NArg
    End Function

    Module Subroutine GetCmdLineArgNameValue( This, iArg, Name, Value )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  iArg                            !< Index of the argument to be extracted
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Value
    End Subroutine


    ! **************************************************************************************************************
    !                                       HEADER-RELATED PROCEDURES
    ! **************************************************************************************************************
    Module Subroutine WriteHeader( This, ToAll, ToLogger, ToScreen, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                          !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  ToAll                         !< Indicator for the writing header everywhere
      logical                                     ,optional ,intent(in)     ::  ToLogger                      !< Indicator for the writing header to the Logger
      logical                                     ,optional ,intent(in)     ::  ToScreen                      !< Indicator for the writing header to the screen
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
    End Subroutine
    Module Subroutine GetHeader( This, Header, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  Header(:)
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
    End Subroutine
    Module Function GetHeaderLogo( This, LogLevel ) result( HeaderLogo )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
      character(:)  ,allocatable                                            ::  HeaderLogo(:)
    End Function
    Module Function GetHeaderInfo( This, LogLevel ) result( HeaderInfo )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                      !< Debugging indicator
      character(:)  ,allocatable                                            ::  HeaderInfo(:)
    End Function




!     Module Subroutine LoadDirectories( This, LogLevel )
!       class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
!     End Subroutine

    Module Subroutine FinalizeConfiguration( This, WriteTimer, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  WriteTimer                      !< Indicator whether the timer should be outputed
      integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Debugging indicator
    End Subroutine
!
!     Module Subroutine Process_Command_Line( This, LogLevel )
!       class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Debugging indicator
!     End Subroutine
!
!     Module Subroutine Get_Environment_Variables( This, LogLevel )
!       class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
!     End Subroutine
!
!     Module Subroutine Goto_Working_Directory( This, LogLevel )
!       class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
!     End Subroutine
!
!     Module Subroutine Set_Working_Directory( This, LogLevel )
!       class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
!     End Subroutine

!     Module Function Set_LogFile_Header( This ) result(Header)
!       class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
!       character(:)  ,dimension(:)   ,allocatable                            ::  Header                          ! Header
!     End Function
!
!     Module Subroutine Get_EnvVar( Name, Value, Status, ErrMsg, LogLevel )
!       character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable to be retrieved
!       character(:)          ,allocatable                    ,intent(out)    ::  Value                           !< Value of the input environment variable
!       integer                                     ,optional ,intent(out)    ::  Status                          !< Local status indicator
!       character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg                          !< Local error message
!       logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator
!     End Subroutine
!
!     Pure Module Function Is_Initialized( This ) result(Indicator)
!       class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
!       logical                                                               ::  Indicator                       !< Indicator whether the object has been initialized
!     End Function



    ! ==============================================================================================================
    !    PROCEDURES RELATED TO THE ENVVAR OBJECT
    ! ==============================================================================================================
    Module Subroutine InitializeEnvVar( This, Names, Descriptions )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Names
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Descriptions
    End Subroutine
    Module Subroutine AddEnvVar( This, Name, Description )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                ,optional ,intent(in)     ::  Description
    End Subroutine
    Pure Module Subroutine GetEnvVar( This, Name, Value, Description, Mandatory )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Value                           !< Value of the input environment variable
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Description
      logical                                     ,optional ,intent(in)     ::  Mandatory
    End Subroutine
    Module Subroutine LoadEnvVar( This, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine
    Module Subroutine GetEnvVarValue( This, Name, Value, Status, ErrMsg )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable
      character(:)          ,allocatable                    ,intent(out)    ::  Value                           !< Value of the input environment variable
      integer                                     ,optional ,intent(out)    ::  Status                          !< Status indicator
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg                          !< Error message
    End Subroutine

    ! ==============================================================================================================
    !    PROCEDURES RELATED TO THE DIRTREE OBJECT
    ! ==============================================================================================================
    Module Subroutine LoadDirectories( This, LogLevel )
      class(Application_Type)                               ,intent(inout)  ::  This
      integer                                     ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine
    Module Subroutine InitializeDirectories( This )
      class(Application_Type)                               ,intent(inout)  ::  This
    End Subroutine
!     Pure
    Module Function GetDirectoriesSummary( This ) result(Summary)
      class(Application_Type)                               ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Summary(:)
    End Function
    Pure Module Subroutine AddDirectory( This, Directory )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(Directory_Type)                                  ,intent(in)     ::  Directory
    End Subroutine
    Pure Module Subroutine UpdateDirectoryPath( This, Key, Path )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Key
      character(*)                                          ,intent(in)     ::  Path
    End Subroutine
    Pure Module Function GetNumberDirectories( This ) result(NDirectories)
      class(Application_Type)                               ,intent(in)     ::  This
      integer                                                               ::  NDirectories
    End Function
    Pure Module Function GetDirectoryPath( This, Key, Hide ) result(Path)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      logical                                     ,optional ,intent(in)     ::  Hide
      character(:)  ,allocatable                                            ::  Path
    End Function
    Pure Module Subroutine GetDirectoryPaths( This, Key, Paths, Hide )
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      character(:)  ,allocatable                            ,intent(out)    ::  Paths(:)
      logical                                     ,optional ,intent(in)     ::  Hide
    End Subroutine
    Pure Module Function GetDirectoryFromIndex( This, iDir ) result(Directory)
      class(Application_Type)                               ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  iDir
      type(Directory_Type)                                                  ::  Directory
    End Function
    Pure Module Function GetDirectoryFromKey( This, Key ) result(Directory)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      type(Directory_Type)                                                  ::  Directory
    End Function
    Pure Module Subroutine GetDirectoriesFromKey( This, Key, ListDir )
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      type(Directory_Type)  ,allocatable                    ,intent(out)    ::  ListDir(:)
    End Subroutine
    Pure Module Function GetDirectoryIndex( This, Key ) result(iDir)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      integer                                                               ::  iDir
    End Function
    Pure Module Function GetDirectoryDesc( This, Key ) result(Desc)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      character(:)  ,allocatable                                            ::  Desc
    End Function
    Pure Module Function SubstituteDirectory( This, InputString ) result(OutputString)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function
    Pure Module Function IsDirectoryDefined( This, Key ) result(IsDefined)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      logical                                                               ::  IsDefined
    End Function
    Module Function FindFile( This, BaseName, DirKeys, RecKeys, Recursive, Mandatory, Found, LogLevel ) result(FullName)
      class(Application_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  BaseName
      character(*)                                          ,intent(in)     ::  DirKeys
      logical                                     ,optional ,intent(in)     ::  RecKeys(:)
      logical                                     ,optional ,intent(in)     ::  Recursive
      logical                                     ,optional ,intent(in)     ::  Mandatory
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  LogLevel
      character(:)  ,allocatable                                            ::  FullName
    End Function


    ! ==============================================================================================================
    !    PROCEDURES RELATED TO THE TIMER COMPONENT
    ! ==============================================================================================================
    Module Subroutine InitializeTimer( This, Name, Start )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Timer name
      logical                                     ,optional ,intent(in)     ::  Start                           !< Indicator whether the Timer has to be started during the initialization
    End Subroutine
    Module Subroutine StartTimer( This )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine
    Module Subroutine StopTimer( This, Unit )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  Unit
    End Subroutine
    Module Subroutine OutputTimer( This, Unit )
      class(Application_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  Unit
    End Subroutine
    Module Subroutine AddSubTimer( This, Name, ID )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the sub-timer to be added
      integer                                     ,optional ,intent(out)    ::  ID                              !< ID of the sub-timer to be added
    End Subroutine
    Module Subroutine StartSubTimer( This, ID )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer to be started
    End Subroutine
    Module Subroutine StopSubTimer( This, ID )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer to be started
    End Subroutine
    Module Subroutine NextSubTimer( This )
      class(Application_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine

  End Interface

End Module