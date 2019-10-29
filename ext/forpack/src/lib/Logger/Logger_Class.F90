Module Logger_Class

! #define Purity Pure
#define Purity

! @TODO In Logger_Type remove all private procedure interfaces from the main module and add them as private entities to the sub-modules

  use ,intrinsic :: iso_fortran_env      ,only:  Output_Unit
  use LoggerItem_Class  ,only:  LoggerItem_Type
  use LoggerUnit_Class  ,only:  LoggerUnit_Type
  use Logger_Parameters ,only:  LogLevel_NOLOGS, LogLevel_ERROR, LogLevel_WARNING, LogLevel_INFO, &
                                LogLevel_DEBUG, LogLevel_HEAVYDEBUG, LogLevel_DEFAULT

  implicit none

  private
  public        ::  Logger
  public        ::  Logger_Type
  public        ::  LogLevel_NOLOGS
  public        ::  LogLevel_ERROR
  public        ::  LogLevel_WARNING
  public        ::  LogLevel_INFO
  public        ::  LogLevel_DEBUG
  public        ::  LogLevel_HEAVYDEBUG
  public        ::  LogLevel_DEFAULT

  Type                                  ::  Logger_Type
    logical                             ::  Initialized =   .False.                   !< Indicator whether the Logger is initialized
    logical                             ::  Activated   =   .True.                    !< Indicator whether the Logger is activated
    logical                             ::  Advancing   =   .True.                    !< Indicator whether the Logger is in an "Advancing" mode
    integer                             ::  RecursivelyActiveCount  =   0             !< Iteration count of recurively active items
    logical                             ::  RecursivelyActive  =   .False.            !< Indicator whether the Logger is recursively active
    integer                             ::  iItem       =   0                         !< Index of current LoggerItem object
    integer                             ::  NItems      =   0                         !< Number of LoggerItem object loaded
    integer                             ::  NUnits      =   0                         !< Number of LoggerUnit object loaded
    character(:)          ,allocatable  ::  ActiveProcedures(:)                       !< List of procedures for which logs should be activated
    character(:)          ,allocatable  ::  RecursivelyActiveProcedures(:)            !< List of procedures for which logs should be recursively activated
    type(LoggerItem_Type) ,allocatable  ::  Items(:)                                  !< List of LoggerItem objects
    type(LoggerItem_Type) ,pointer      ::  CurrentItem  => null()                    !< Pointer to current LoggerItem
    type(LoggerUnit_Type) ,allocatable  ::  Units(:)                                  !< List of LoggerUnits objects
  contains
    Final                 ::  FinalizeLogger
!   Procedures to initialize a Logger object and to perform file-related operations
    procedure   ,public         ::  Initialize  =>  InitializeLogger                        !< Initializes a Logger object
    procedure   ,public         ::  Activate                                                !< Procedure to force activation of the Logger object
    procedure   ,public         ::  Deactivate                                              !< Procedure to force deactivation of the Logger object
    procedure   ,public         ::  Free        =>  FreeLogger                              !< Free a Logger object
    procedure   ,public         ::  Reopen      =>  ReopenLogger                            !< Close and deleted current logfile, and then reopen it in 'append' mode
    procedure   ,public         ::  Close       =>  CloseLogger                             !< Close the logfile associated to a Logger object
    procedure   ,public         ::  Backspace   =>  BackspaceLogger                         !< Backspaces the logfile associated to a Logger object
    procedure   ,public         ::  Rewind      =>  RewindLogger                            !< Rewind the logfile associated to a Logger object
    procedure   ,public         ::  Flush       =>  FlushLogger                             !< Flush the unit associated to a Logger object
    procedure   ,public         ::  Entering
    procedure   ,public         ::  Exiting
    procedure   ,public         ::  GetLogLevelName
    procedure   ,public         ::  GetLogLevel
    procedure   ,public         ::  HasLogLevel
    procedure   ,public         ::  StartDebugMode
    procedure   ,public         ::  StopDebugMode

    procedure   ,public         ::  ChangeMsgLogLevel
    procedure   ,public         ::  RestoreMsgLogLevel
    procedure   ,public         ::  DecreaseMsgLogLevel
    procedure   ,public         ::  IncreaseMsgLogLevel

!   Procedures related to units
    generic     ,public         ::  AddUnit     =>  AddLoggerUnitFromObj, AddLoggerUnitFromFileName
    procedure   ,public         ::  RemoveUnit  =>  RemoveLoggerUnit
    procedure   ,public         ::  ActivateOnlyUnit
    procedure   ,public         ::  ActivateAllUnits
    procedure   ,public         ::  GetFileName =>  GetLoggerUnitFileName
    procedure   ,public         ::  GetUnit     =>  GetLoggerUnit
    procedure   ,public         ::  GetUnits    =>  GetLoggerUnits
    procedure   ,private        ::  AddLoggerUnitFromObj
    procedure   ,private        ::  AddLoggerUnitFromFileName

!   Procedures to enforce the activation of logs for a set of procedures
    generic     ,public         ::  AddProcedureToLog  => AddActiveProcedures0d, AddActiveProcedures1d
    procedure   ,public         ::  GetProcedureToLog
    procedure   ,private        ::  AddActiveProcedures0d
    procedure   ,private        ::  AddActiveProcedures1d

    generic     ,public         ::  RemoveProcedureToLog  => RemoveActiveProcedures0d, RemoveActiveProcedures1d
    procedure   ,private        ::  RemoveActiveProcedures0d
    procedure   ,private        ::  RemoveActiveProcedures1d

!   Procedures to enforce the activation of logs for a set of procedures
    generic     ,public         ::  AddRecursivelyActiveProcedures  => AddRecursivelyActiveProcedures_0d, AddRecursivelyActiveProcedures_1d
    procedure   ,public         ::  GetRecursivelyActiveProcedures
    procedure   ,private        ::  AddRecursivelyActiveProcedures_0d
    procedure   ,private        ::  AddRecursivelyActiveProcedures_1d

!   Procedures to access the properties of the Logger object
    generic     ,public         ::  On => LoggerActive
    procedure   ,public         ::  LoggerActive                                                  !< Return a logical variable which indicates whether of not the Logger is active for the current log-level
    procedure   ,public         ::  GetFormatPrefix                                         !< Returns a character string corresponding to the prefix of the format used to write logs
    procedure   ,public         ::  GetPrefix_
    procedure   ,public         ::  GetPrefixProcedure
    procedure   ,public         ::  GetPath                                                 !< Get the path of the Logger in term of procedure names
    procedure   ,public         ::  GetIndentation
!   Private procedures used during initialization or writing
    procedure   ,private        ::  AddItem                                             !< Add an element to the list of 'LoggerItem' sub-objects and update the current index of log-level
    procedure   ,private        ::  RemoveItem                                          !< Remove the last element from the list of 'LoggerItem' sub-objects and update the current index of log-level
    procedure   ,private        ::  Write_NewLine
    procedure   ,private        ::  Set_Backspace
    procedure   ,public ,nopass ::  SelectedLine
    procedure   ,public ,nopass ::  SkippedLine
    procedure   ,public ,nopass ::  FirstSkippedLine
    procedure   ,public ,nopass ::  SkippedLinesRange
!   Procedures for writing logs
    procedure   ,public         ::  Stop  =>  StopFromLogger
    procedure   ,private        ::  Write_Blank_Line
    procedure   ,private        ::  Write_1xV0
    procedure   ,private        ::  Write_2xV0
    procedure   ,private        ::  Write_3xV0
    procedure   ,private        ::  Write_4xV0
    procedure   ,private        ::  Write_5xV0
    procedure   ,private        ::  Write_6xV0
    procedure   ,private        ::  Write_7xV0
    procedure   ,private        ::  Write_8xV0
    procedure   ,private        ::  Write_9xV0
    procedure   ,private        ::  Write_10xV0
    procedure   ,private        ::  Write_11xV0
    procedure   ,private        ::  Write_12xV0
    procedure   ,private        ::  Write_13xV0
    procedure   ,private        ::  Write_14xV0
    procedure   ,private        ::  Write_15xV0
    procedure   ,private        ::  Write_16xV0
    procedure   ,private        ::  Write_17xV0
    procedure   ,private        ::  Write_18xV0
    procedure   ,private        ::  Write_20xV0
    procedure   ,private        ::  Write_21xV0
    procedure   ,private        ::  Write_22xV0
    procedure   ,private        ::  Write_23xV0
    procedure   ,private        ::  Write_24xV0
    procedure   ,private        ::  Write_25xV0
    procedure   ,private        ::  Write_26xV0
    procedure   ,private        ::  Write_27xV0
    procedure   ,private        ::  Write_28xV0
    procedure   ,private        ::  Write_29xV0
    procedure   ,private        ::  Write_30xV0
    procedure   ,private        ::  Write_1xV1
    procedure   ,private        ::  Write_1xV0_1xV1
    procedure   ,private        ::  Write_2xV0_1xV1
    procedure   ,private        ::  Write_3xV0_1xV1
    procedure   ,private        ::  Write_5xV0_1xV1
    procedure   ,private        ::  Write_7xV0_1xV1
    procedure   ,private        ::  Write_9xV0_1xV1
    procedure   ,private        ::  Write_3xV0_1xV1_1xV0
    procedure   ,private        ::  Write_3xV0_1xV1_2xV0
    procedure   ,private        ::  Write_3xV0_1xV1_1xV0_1xV1
    procedure   ,private        ::  Write_3xV0_2xV1
    procedure   ,private        ::  Write_1xV0_2xV1
    procedure   ,private        ::  Write_1xV0_2xV0V1
    procedure   ,private        ::  Write_1xV0_3xV0V1
    procedure   ,private        ::  Write_4xV0_2xV0V1
    procedure   ,private        ::  Write_6xV0_2xV0V1
    procedure   ,private        ::  Write_1xV0_1xV2
    procedure   ,private        ::  Write_2xV0V1
    procedure   ,private        ::  Write_3xV0V1
    procedure   ,private        ::  Write_4xV0V1

    procedure   ,private        ::  WriteMatrix_1xV0_1xV2
    generic     ,public         ::  WriteMatrix => WriteMatrix_1xV0_1xV2

!     procedure   ,private        ::  WriteToString_2xV0

    generic     ,public         ::  Write   =>  Write_Blank_Line    , &
!                                             WriteToString_2xV0  , &
                                            Write_1xV0          , &
                                            Write_2xV0          , &
                                            Write_3xV0          , &
                                            Write_4xV0          , &
                                            Write_5xV0          , &
                                            Write_6xV0          , &
                                            Write_7xV0          , &
                                            Write_8xV0          , &
                                            Write_9xV0          , &
                                            Write_10xV0         , &
                                            Write_11xV0         , &
                                            Write_12xV0         , &
                                            Write_13xV0         , &
                                            Write_14xV0         , &
                                            Write_15xV0         , &
                                            Write_16xV0         , &
                                            Write_17xV0         , &
                                            Write_18xV0         , &
                                            Write_20xV0         , &
                                            Write_21xV0         , &
                                            Write_22xV0         , &
                                            Write_23xV0         , &
                                            Write_24xV0         , &
                                            Write_25xV0         , &
                                            Write_26xV0         , &
                                            Write_27xV0         , &
                                            Write_28xV0         , &
                                            Write_29xV0         , &
                                            Write_30xV0         , &
                                            Write_1xV1          , &
                                            Write_1xV0_1xV1     , &
                                            Write_2xV0_1xV1     , &
                                            Write_3xV0_1xV1     , &
                                            Write_5xV0_1xV1     , &
                                            Write_7xV0_1xV1     , &
                                            Write_9xV0_1xV1     , &
                                            Write_3xV0_2xV1     , &
                                            Write_3xV0_1xV1_1xV0, &
                                            Write_3xV0_1xV1_2xV0, &
                                            Write_3xV0_1xV1_1xV0_1xV1, &
                                            Write_1xV0_2xV1     , &
                                            Write_1xV0_2xV0V1   , &
                                            Write_1xV0_3xV0V1   , &
                                            Write_4xV0_2xV0V1   , &
                                            Write_6xV0_2xV0V1   , &
                                            Write_1xV0_1xV2     , &
                                            Write_2xV0V1        , &
                                            Write_3xV0V1        , &
                                            Write_4xV0V1
  End Type

  type(Logger_Type) ,target   ::  Logger

  Interface

    ! **************************************************************************************************************
    !         PROCEDURES TO INITIALIZE A LOGGER OBJECT AND TO PERFORM FILE-RELATED OPERATIONS
    ! **************************************************************************************************************
    Module Subroutine InitializeLogger( This, FileName, Status, Position, Procedure, Indentation, i_Force_FileName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< Name of the log file
      character(*)                                ,optional ,intent(in)     ::  Status
      character(*)                                ,optional ,intent(in)     ::  Position
      character(*)                                ,optional ,intent(in)     ::  Procedure                           !< Names of the calling procedures to reach the current procedure. Each procedure neames are separated by the '>' character.  Only used by the Logger object
      integer                                     ,optional ,intent(in)     ::  Indentation                         !< Indentation level
      logical                                     ,optional ,intent(in)     ::  i_Force_FileName
    End Subroutine
    Purity Module Subroutine Activate( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
    End Subroutine
    Purity Module Subroutine Deactivate( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
    End Subroutine
    Module Subroutine FreeLogger( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
    End Subroutine
    Module Subroutine FinalizeLogger( This )
      type(Logger_Type)                                     ,intent(inout)  ::  This                                !< Passed-object dummy argument
    End Subroutine
    Module Subroutine ReopenLogger( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
    End Subroutine
!     Module Subroutine ChangeLoggerDirectory( This, Path )
!       class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
!       character(*)                                          ,intent(in)     ::  Path                                !< Path of new directory where to copy log units and re-open files
!     End Subroutine

    Module Subroutine CloseLogger( This, Status )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Status                              !< Status of the close instruction
    End Subroutine
    Module Subroutine BackspaceLogger( This, Status )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
    End Subroutine
    Module Subroutine RewindLogger( This, Status )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
    End Subroutine
    Module Subroutine FlushLogger( This )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
    End Subroutine


    Module Subroutine Entering( This, ProcedureName, LogLevel, DefLogLevel, MsgLogLevel, LogInOut )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      character(*)                                          ,intent(in)     ::  ProcedureName                       !< Name of the calling procedure
      integer                                     ,optional ,intent(in)     ::  LogLevel
      integer                                     ,optional ,intent(in)     ::  DefLogLevel
      integer                                     ,optional ,intent(in)     ::  MsgLogLevel
      logical                                     ,optional ,intent(in)     ::  LogInOut                            !< Indicator whether a message should be written when entering/exiting a procedure
    End Subroutine
    Module Subroutine Exiting( This, LogInOut )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      logical                                     ,optional ,intent(in)     ::  LogInOut                            !< Indicator whether a message should be written when entering/exiting a procedure
    End Subroutine

    ! **************************************************************************************************************
    !         PROCEDURES FOR ENFORCING THE ACTIVATION OF LOGS FOR A SET OF PROCEDURES
    ! **************************************************************************************************************
    Purity Module Subroutine GetProcedureToLog( This, ProcName )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding
      character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  ProcName                            !< Names of the procedures for which the activation of logs should be enforced
    End Subroutine
    Purity Module Subroutine AddActiveProcedures0d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)                                          ,intent(in)     ::  ProcName                            !< Name of a procedure to add to the list of procedures for which the activation of logs should be enforced
    End Subroutine
    Purity Module Subroutine AddActiveProcedures1d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)  ,dimension(:)                           ,intent(in)     ::  ProcName                            !< Name of the procedures to add to the list of procedures for which the activation of logs should be enforced
    End Subroutine
    Purity Module Subroutine RemoveActiveProcedures0d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)                                          ,intent(in)     ::  ProcName                            !< Name of a procedure to add to the list of procedures for which the activation of logs should be enforced
    End Subroutine
    Purity Module Subroutine RemoveActiveProcedures1d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)  ,dimension(:)                           ,intent(in)     ::  ProcName                            !< Name of the procedures to add to the list of procedures for which the activation of logs should be enforced
    End Subroutine

    ! **************************************************************************************************************
    !         PROCEDURES FOR ENFORCING THE DESACTIVATION OF LOGS FOR A SET OF PROCEDURES
    ! **************************************************************************************************************
    Purity Module Subroutine GetRecursivelyActiveProcedures( This, ProcName )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding
      character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  ProcName                            !< Names of the procedures for which the desactivation of logs should be enforced
    End Subroutine
    Purity Module Subroutine AddRecursivelyActiveProcedures_0d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)                                          ,intent(in)     ::  ProcName                            !< Name of a procedure to add to the list of procedures for which the desactivation of logs should be enforced
    End Subroutine
    Purity Module Subroutine AddRecursivelyActiveProcedures_1d( This, ProcName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)  ,dimension(:)                           ,intent(in)     ::  ProcName                            !< Name of the procedures to add to the list of procedures for which the desactivation of logs should be enforced
    End Subroutine

    ! **************************************************************************************************************
    !                           PROCEDURES TO ACCESS THE PROPERTIES OF THE LOGGER OBJECT
    ! **************************************************************************************************************
    Purity Module Function LoggerActive( This, LogLevel ) result(IsActive)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel
      logical                                                               ::  IsActive
    End Function
    Purity Module Function GetIndentation( This ) result(Indentation)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                                               ::  Indentation
    End Function
    Purity Module Function GetFormatPrefix( This, LogLevel, Error, Warning, Info, Debug, HeavyDebug, PrefixPresence ) result(FormatPrefix)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'HeavyDebug' log message
      logical                                     ,optional ,intent(in)     ::  PrefixPresence                      !< Indicator of the prefix presence
      character(:)  ,allocatable                                            ::  FormatPrefix
    End Function
    Purity Module Function GetPrefix_( This, Indent, Procedure ) result(Prefix)
      class(Logger_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Indent
      logical                                     ,optional ,intent(in)     ::  Procedure
      character(:)  ,allocatable                                            ::  Prefix                          !< Character string corresponding to the "procedure" part of the format's prefix
    End Function
    Purity Module Function GetPrefixProcedure( This ) result(Prefix)
      class(Logger_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Prefix                          !< Character string corresponding to the "procedure" part of the format's prefix
    End Function
    Purity Module Function GetPath( This, ProcedureName ) result(Path)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  ProcedureName
      character(:)  ,allocatable                                            ::  Path                                !< Path of the Logger in term of procedure names
    End Function

    Purity Module Function GetLogLevelName( This ) result(LogLevelName)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  LogLevelName
    End Function

    Purity Module Function GetLogLevel( This ) result(LogLevel)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                                               ::  LogLevel
    End Function

    Purity Module Function HasLogLevel( This, LogLevel ) result(Inidcator)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  LogLevel
      logical                                                               ::  Inidcator
    End Function

    Purity Module Subroutine StartDebugMode( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine
    Purity Module Subroutine StopDebugMode( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine

    Purity Module Subroutine ChangeMsgLogLevel( This, MsgLogLevel )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      integer                                               ,intent(in)     ::  MsgLogLevel
    End Subroutine

    Purity Module Subroutine RestoreMsgLogLevel( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine

    Purity Module Subroutine DecreaseMsgLogLevel( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine

    Purity Module Subroutine IncreaseMsgLogLevel( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine


    ! **************************************************************************************************************
    !                           PROCEDURES RELATED TO LOGGER UNITS
    ! **************************************************************************************************************
    Purity Module Subroutine AddLoggerUnitFromObj( This, LoggerUnit )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      type(LoggerUnit_Type)                                 ,intent(in)     ::  LoggerUnit                          !< LoggerUnit object to be added to the list of LoggerUnit sub-objects
    End Subroutine
    Purity Module Subroutine AddLoggerUnitFromFileName( This, FileName )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      character(*)                                          ,intent(in)     ::  FileName                            !< Name of the log file
    End Subroutine
    Purity Module Subroutine RemoveLoggerUnit( This, FileName, Index )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< Name of the log file
      integer                                     ,optional ,intent(in)     ::  Index
    End Subroutine
    Purity Module Subroutine ActivateOnlyUnit( This, Unit, Units )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
    End Subroutine
    Purity Module Subroutine ActivateAllUnits( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
    End Subroutine
    Module Function GetLoggerUnitFileName( This, i, AbsolutePath ) result(FileName)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding the Logger object
      integer                                               ,intent(in)     ::  i
      logical                                     ,optional ,intent(in)     ::  AbsolutePath
      character(:)  ,allocatable                                            ::  FileName
    End Function
    Purity Module Function GetLoggerUnit( This ) result(Unit)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding the Logger object
      integer                                                               ::  Unit                                !< Unit number of the last LoggerUnit object loaded
    End Function
    Purity Module Function GetLoggerUnits( This ) result(Units)
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding the Logger object
      integer ,allocatable                                                  ::  Units(:)                            !< Unit numbers of all LoggerUnit objects loaded
    End Function

    ! **************************************************************************************************************
    !                           PROCEDURES RELATED TO LOGGER ITEMS
    ! **************************************************************************************************************
    Purity Module Subroutine AddItem( This, LoggerItem )
      class(Logger_Type)                ,target             ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
      type(LoggerItem_Type)                                 ,intent(in)     ::  LoggerItem                          !< LoggerItem object to be added to the list of LoggerItem sub-objects
    End Subroutine
    Purity Module Subroutine RemoveItem( This )
      class(Logger_Type)                ,target             ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding the Logger object
    End Subroutine


    ! **************************************************************************************************************
    !                           PRIVATE PROCEDURES USED DURING INITIALIZATION OR WRITING
    ! **************************************************************************************************************
!     Purity Module Subroutine SetFileName( This, FileName, i_Force_FileName )
!       class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
!       character(*)                                ,optional ,intent(in)     ::  FileName                            !< FileName of the log file
!       logical                                     ,optional ,intent(in)     ::  i_Force_FileName
!     End Subroutine


    Module Subroutine Set_Backspace( This, Backspace )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
    End Subroutine

    Module Subroutine Write_NewLine( This, NewLine )
      class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
    End Subroutine

    Purity Module Function SelectedLine( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                     ,optional ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to an element to be printed
    End Function

    Purity Module Function SkippedLine( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                     ,optional ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to an element to be printed
    End Function


    Purity Module Function FirstSkippedLine( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                     ,optional ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to the first element to be skipped
    End Function
    Purity Module Function SkippedLinesRange( NItemTot, NItemMax ) result(String)
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                     ,optional ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      character(:)  ,allocatable                                            ::  String                              !< Character string describing the skipped items
    End Function

  End Interface



  Interface
    Module Subroutine StopFromLogger( This, Message )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
      character(*)                                ,optional ,intent(in)     ::  Message
    End Subroutine

    Module Subroutine Write_Blank_Line( This )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument corresponding
    End Subroutine
!     Module Subroutine CallingProcedure(  This,                                      &
!                             V1,                                                     &
!                             Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Underline, Fc, Fi, Fr, Fmt, &
!                             F1                                                      )
!       class(Logger_Type)                                    ,intent(inout)  ::  This                              !< Passed-object dummy argument
!       character(*)                                          ,intent(in)     ::  V1
!       logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                            !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
!       logical                                     ,optional ,intent(in)     ::  Prefix                            !< Indicator of the prefix presence
!       integer                                     ,optional ,intent(in)     ::  LogLevel                          !< Indicator of the log level associated to current log message
!       logical                                     ,optional ,intent(in)     ::  Error                             !< Indicator of an 'Error' log message
!       logical                                     ,optional ,intent(in)     ::  Warning                           !< Indicator of an 'Warning' log message
!       logical                                     ,optional ,intent(in)     ::  Info                              !< Indicator of an 'Info' log message
!       logical                                     ,optional ,intent(in)     ::  Debug                             !< Indicator of an 'Debug' log message
!       logical                                     ,optional ,intent(in)     ::  HeavyDebug                        !< Indicator of an 'Debug' log message
!       logical                                     ,optional ,intent(in)     ::  NewLine                           !< Indicator whether or not a black line must be written before writing the variables
!       logical                                     ,optional ,intent(in)     ::  Advance                           !< Indicator whether or not the line should be advanced
!       logical                                     ,optional ,intent(in)     ::  Backspace                         !< Indicator whether or not backpace should be used atfer the write statment
!       integer                                     ,optional ,intent(out)    ::  Status                            !< Error status indicator (=0 if everthing is ok, /=0 if error)
!       character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                   !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
!       character(*)                                ,optional ,intent(in)     ::  Underline
!       character(*)                                ,optional ,intent(in)     ::  F1
!     End Subroutine
    Module Subroutine Write_1xV0(  This,                                                   &
                            V1,                                                     &
                            Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Underline, Fc, Fi, Fr, Fmt, &
                            F1                                                      )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  Underline
      character(*)                                ,optional ,intent(in)     ::  F1
    End Subroutine
    Module Subroutine Write_2xV0(  This,                                               &
                                  V1,  V2,                                            &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, ToString, &
                                  F1,  F2                                             )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ToString(:)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2
    End Subroutine
    Module Subroutine Write_3xV0(  This,                                               &
                                  V1,  V2,  V3,                                       &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3                                        )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3
    End Subroutine
    Module Subroutine Write_4xV0(   This,                                               &
                                  V1,  V2,  V3,  V4,                                  &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4                                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4
    End Subroutine
    Module Subroutine Write_5xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,                             &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5                              )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5
    End Subroutine
    Module Subroutine Write_6xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,                        &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6                         )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6
    End Subroutine
    Module Subroutine Write_7xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,                   &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7                    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7
    End Subroutine
    Module Subroutine Write_8xV0(   This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,              &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8               )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8
    End Subroutine
    Module Subroutine Write_9xV0(   This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,         &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9          )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9
    End Subroutine
    Module Subroutine Write_10xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10
    End Subroutine
    Module Subroutine Write_11xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11,                                                &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11                                                 )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11
    End Subroutine
    Module Subroutine Write_12xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12,                                           &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12                                            )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12
    End Subroutine
    Module Subroutine Write_13xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13,                                      &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13                                       )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13
    End Subroutine
    Module Subroutine Write_14xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14,                                 &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14
    End Subroutine
    Module Subroutine Write_15xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15,                            &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15                             )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15
    End Subroutine
    Module Subroutine Write_16xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16,                       &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16                        )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16
    End Subroutine
    Module Subroutine Write_17xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17,                  &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17
    End Subroutine
    Module Subroutine Write_18xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18,             &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18              )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18
    End Subroutine
    Module Subroutine Write_19xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19,        &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19         )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19
    End Subroutine
    Module Subroutine Write_20xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20
    End Subroutine
    Module Subroutine Write_21xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21,                                                &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21                                                 )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21
    End Subroutine
    Module Subroutine Write_22xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22,                                           &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22                                            )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22
    End Subroutine
    Module Subroutine Write_23xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23,                                      &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23                                       )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23
    End Subroutine
    Module Subroutine Write_24xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24,                                 &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24
    End Subroutine
    Module Subroutine Write_25xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25,                            &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25                             )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25
    End Subroutine
    Module Subroutine Write_26xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25, V26,                       &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25, F26                        )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25, V26
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25, F26
    End Subroutine
    Module Subroutine Write_27xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25, V26, V27,                  &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25, F26, F27                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25, V26, V27
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25, F26, F27
    End Subroutine
    Module Subroutine Write_28xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25, V26, V27, V28,             &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25, F26, F27, F28              )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25, V26, V27, V28
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25, F26, F27, F28
    End Subroutine
    Module Subroutine Write_29xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25, V26, V27, V28, V29,        &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25, F26, F27, F28, F29         )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25, V26, V27, V28, V29
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25, F26, F27, F28, F29
    End Subroutine
    Module Subroutine Write_30xV0(  This,                                               &
                                  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                  V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                  V21, V22, V23, V24, V25, V26, V27, V28, V29, V30,   &
                                  Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                  F21, F22, F23, F24, F25, F26, F27, F28, F29, F30    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                                                                    V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,   &
                                                                                    V21, V22, V23, V24, V25, V26, V27, V28, V29, V30
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10,   &
                                                                                    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,   &
                                                                                    F21, F22, F23, F24, F25, F26, F27, F28, F29, F30
    End Subroutine
    Module Subroutine Write_1xV1(   This,                                               &
                                    V1,                                                 &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, &
                                    Inline, &
                                    Fc, Fi, Fr, Fmt, &
                                    F1                                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)      ,dimension(:)                           ,intent(in)     ::  V1
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      logical                                     ,optional ,intent(in)     ::  Inline
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1
    End Subroutine


    Module Subroutine Write_1xV0_1xV1(  This,                                               &
                                    V1,  V2,                                            &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2, NItemMax                                             )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1
      class(*)      ,dimension(:)                           ,intent(in)     ::  V2
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2
      integer                                     ,optional ,intent(in)     ::  NItemMax
    End Subroutine
    Module Subroutine Write_2xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,                                       &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F0, F1,  F2,  F3                                    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2
      class(*)      ,dimension(:)                           ,intent(in)     ::  V3
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0, F1,  F2,  F3
    End Subroutine
    Module Subroutine Write_3xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,  V4,                                  &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4                                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3
      class(*)      ,dimension(:)                           ,intent(in)     ::  V4
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4
    End Subroutine
    Module Subroutine Write_5xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,  V6,                        &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5,  F6                         )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5
      class(*)      ,dimension(:)                           ,intent(in)     ::  V6
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6
    End Subroutine
    Module Subroutine Write_7xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,              &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8               )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7
      class(*)      ,dimension(:)                           ,intent(in)     ::  V8
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8
    End Subroutine
    Module Subroutine Write_9xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9,  V10,   &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,  V9
      class(*)      ,dimension(:)                           ,intent(in)     ::  V10
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10
    End Subroutine
    Module Subroutine Write_3xV0_1xV1_1xV0(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,                                  &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5                                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V5
      class(*)      ,dimension(:)                           ,intent(in)     ::  V4
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5
    End Subroutine
    Module Subroutine Write_3xV0_1xV1_2xV0(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,  V6,                                 &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5,  F6                                   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V5,  V6
      class(*)      ,dimension(:)                           ,intent(in)     ::  V4
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6
    End Subroutine
    Module Subroutine Write_3xV0_1xV1_1xV0_1xV1(  This,                                               &
                                    V1,  V2,  V3,  V4,  V5,  V6,                                      &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5,  F6                                       )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3,  V5
      class(*)      ,dimension(:)                           ,intent(in)     ::  V4, V6
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6
    End Subroutine
    Module Subroutine Write_3xV0_2xV1(  This,                                               &
                                    V1,  V2,  V3,  V4, V5,                                  &
                                    Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                    F1,  F2,  F3,  F4,  F5                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1,  V2,  V3
      class(*)      ,dimension(:)                           ,intent(in)     ::  V4, V5
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5
    End Subroutine
    Module Subroutine Write_1xV0_2xV1(  This,                                               &
                                      V1,  V2,  V3,                                       &
                                      Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                      F0, F1,  F2,  F3                                    )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1
      class(*)      ,dimension(:)                           ,intent(in)     ::  V2, V3
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0,  F1,  F2,  F3
    End Subroutine
    Module Subroutine Write_1xV0_2xV0V1(  This,                                             &
                                      V1,  V2,  V3,  V4,  V5,                             &
                                      Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                      F0, F1,  F2,  F3,  F4,  F5                          )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V2, V4
      class(*)      ,dimension(:)                           ,intent(in)     ::  V3, V5
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0, F1,  F2,  F3,  F4,  F5
    End Subroutine
    Module Subroutine Write_1xV0_3xV0V1(  This,                                             &
                                      V1,  V2,  V3,  V4,  V5,  V6,  V7,                   &
                                      Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                      F0, F1,  F2,  F3,  F4,  F5,  F6,  F7                )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V2, V4, V6
      class(*)      ,dimension(:)                           ,intent(in)     ::  V3, V5, V7
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0, F1,  F2,  F3,  F4,  F5,  F6,  F7
    End Subroutine
    Module Subroutine Write_4xV0_2xV0V1(  This,                                             &
                                      V1,  V2,  V3,  V4,  V5,  V6,  V7, V8,                 &
                                      Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                      F0, F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8                )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V2, V3, V4, V5, V7
      class(*)      ,dimension(:)                           ,intent(in)     ::  V6, V8
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0, F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8
    End Subroutine
    Module Subroutine Write_6xV0_2xV0V1(  This,                                             &
                                      V1,  V2,  V3,  V4,  V5,  V6,  V7, V8, V9, V10,        &
                                      Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                                      F0, F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8, F9,  F10   )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V2, V3, V4, V5, V6, V7, V9
      class(*)      ,dimension(:)                           ,intent(in)     ::  V8, V10
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F0, F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10
    End Subroutine
    Module Subroutine Write_1xV0_1xV2(  This,                                              &
                            V1, V2,                                                 &
                            Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                            F1, F2, MaxRow, MaxCol                                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1
      class(*)      ,dimension(:,:)                         ,intent(in)     ::  V2
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1, F2
      integer                                     ,optional ,intent(in)     ::  MaxRow, MaxCol
    End Subroutine
    Module Subroutine Write_2xV0V1(  This,                                                   &
                              V1,  V2,  V3,  V4,                                      &
                              Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                              F1,  F2,  F3,  F4                                       )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V3
      class(*)      ,dimension(:)                           ,intent(in)     ::  V2, V4
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4
    End Subroutine
    Module Subroutine Write_3xV0V1(  This,                                                   &
                              V1,  V2,  V3,  V4,  V5,  V6,                            &
                              Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                              F1,  F2,  F3,  F4,  F5,  F6                             )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V3, V5
      class(*)      ,dimension(:)                           ,intent(in)     ::  V2, V4, V6
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6
    End Subroutine
    Module Subroutine Write_4xV0V1(  This,                                                   &
                              V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,                  &
                              Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                              F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8 )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1, V3, V5, V7
      class(*)      ,dimension(:)                           ,intent(in)     ::  V2, V4, V6, V8
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8
    End Subroutine


    Module Subroutine WriteMatrix_1xV0_1xV2(  This,                                              &
                            V1, V2,                                                 &
                            Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
                            Rows, Cols, &
                            F1, F2                                                  )
      class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  V1
      class(*)      ,dimension(:,:)                         ,intent(in)     ::  V2
      logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                              !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
      logical                                     ,optional ,intent(in)     ::  Prefix                              !< Indicator of the prefix presence
      integer                                     ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level associated to current log message
      logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
      logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
      logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
      logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
      logical                                     ,optional ,intent(in)     ::  NewLine                             !< Indicator whether or not a black line must be written before writing the variables
      logical                                     ,optional ,intent(in)     ::  Advance                             !< Indicator whether or not the line should be advanced
      logical                                     ,optional ,intent(in)     ::  Backspace                           !< Indicator whether or not backpace should be used atfer the write statment
      integer                                     ,optional ,intent(out)    ::  Status                              !< Error status indicator (=0 if everthing is ok, /=0 if error)
      character(*)                                ,optional ,intent(in)     ::  Rows(:)
      character(*)                                ,optional ,intent(in)     ::  Cols(:)
      character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt                     !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
      character(*)                                ,optional ,intent(in)     ::  F1, F2
    End Subroutine
!     Module Subroutine WriteToString_2xV0(  This,                                               &
!                                   ToString, V1,  V2,                                            &
!                                   Unused, Prefix, Error, Warning, Info, Debug, HeavyDebug, NewLine, Advance, Backspace, LogLevel, Status, Fc, Fi, Fr, Fmt, &
!                                   F1,  F2                                             )
!       class(Logger_Type)                                    ,intent(inout)  ::  This                        !< Passed-object dummy argument
!       character(:)  ,allocatable                            ,intent(inout)  ::  ToString(:)
!       class(*)                                              ,intent(in)     ::  V1,  V2
!       logical       ,dimension(:,:,:,:)           ,optional ,intent(in)     ::  Unused                      !< Unused optional variables required to avoid "The type/rank/keyword signature for this specific procedure matches another specific procedure that shares the same generic binding name."
!       logical                                     ,optional ,intent(in)     ::  Prefix                      !< Indicator of the prefix presence
!       integer                                     ,optional ,intent(in)     ::  LogLevel                    !< Indicator of the log level associated to current log message
!       logical                                     ,optional ,intent(in)     ::  Error                       !< Indicator of an 'Error' log message
!       logical                                     ,optional ,intent(in)     ::  Warning                     !< Indicator of an 'Warning' log message
!       logical                                     ,optional ,intent(in)     ::  Info                        !< Indicator of an 'Info' log message
!       logical                                     ,optional ,intent(in)     ::  Debug                       !< Indicator of an 'Debug' log message
!       logical                                     ,optional ,intent(in)     ::  HeavyDebug                  !< Indicator of an 'Debug' log message
!       logical                                     ,optional ,intent(in)     ::  NewLine                     !< Indicator whether or not a black line must be written before writing the variables
!       logical                                     ,optional ,intent(in)     ::  Advance                     !< Indicator whether or not the line should be advanced
!       logical                                     ,optional ,intent(in)     ::  Backspace                   !< Indicator whether or not backpace should be used atfer the write statment
!       integer                                     ,optional ,intent(out)    ::  Status                      !< Error status indicator (=0 if everthing is ok, /=0 if error)
!       character(*)                                ,optional ,intent(in)     ::  Fc, Fi, Fr, Fmt             !< Type-specific format specificators for character (Fc), interger (Fi) and real (Fr) variables and common format specificator Fc
!       character(*)                                ,optional ,intent(in)     ::  F1,  F2
!     End Subroutine
  End Interface


End Module