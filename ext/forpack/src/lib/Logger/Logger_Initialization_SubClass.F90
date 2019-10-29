SubModule(Logger_Class) Logger_Initialization_SubClass

! @TODO: Adding all procdure in RecursivelyActiveProcedures in ActiveProcedures

! #define Purity Pure
#define Purity

  implicit none

  integer       ,parameter                      ::  Indentation_Step          =   2
  character(*)  ,parameter                      ::  Default_Name_LogFile      =   "logfile.log"
  character(*)  ,parameter                      ::  Default_Logical_Format    =   "l1"
  character(*)  ,parameter                      ::  Default_Integer_Format    =   "i0"
  character(*)  ,parameter                      ::  Default_Real_Format       =   "g0"
  character(*)  ,parameter                      ::  Default_Character_Format  =   "a"
  character(*)  ,parameter                      ::  Default_Spacing2_Format   =   "3x"
  logical       ,parameter                      ::  DefaultLogInOut           =   .False.     ! Default value for the indicator whether logs should be written when going in/out procedures (inside 'Entering'/'Exiting')

  character(*)  ,parameter                      ::  DefaultEnteringMessage    =   "Entering"  ! Default message to be written when entering a procedure (only if LogInOut if active)
  character(*)  ,parameter                      ::  DefaultExitingMessage     =   "Exiting"   ! Default message to be written when exiting  a procedure (only if LogInOut if active)

  character(*)  ,parameter                      ::  Default_OpenStatus   = "REPLACE"
  character(*)  ,parameter                      ::  Default_OpenPosition = "REWIND"
  character(*)  ,parameter      ,dimension(2)   ::  Valid_OpenStatus     = ["REPLACE","OLD    "]
  character(*)  ,parameter      ,dimension(2)   ::  Valid_OpenPosition   = ["REWIND","APPEND"]

  logical ,parameter :: LocalDebug=.False.
!
!   Interface             Convert_Variable_To_String
!     Module Procedure    Convert_Var0d_To_Str0d
!     Module Procedure    Convert_Var1d_To_Str0d
!     Module Procedure    Convert_Var1d_To_Str1d
!     Module Procedure    Convert_Var2d_To_Str1d
!   End Interface

  Interface             AddElementToArray
    Module Procedure    AddElementToArray_C0
    Module Procedure    AddElementToArray_C1
  End Interface

!   Interface             Convert_To_String
!     Module Procedure    Convert_Logical_To_String_0D, Convert_Logical_To_String_1D, Convert_Logical_To_Strings_1D
!     Module Procedure    Convert_INT8_To_String_0D   , Convert_INT8_To_String_1D   , Convert_INT8_To_Strings_1D
!     Module Procedure    Convert_INT16_To_String_0D  , Convert_INT16_To_String_1D  , Convert_INT16_To_Strings_1D
!     Module Procedure    Convert_INT32_To_String_0D  , Convert_INT32_To_String_1D  , Convert_INT32_To_Strings_1D
!     Module Procedure    Convert_INT64_To_String_0D  , Convert_INT64_To_String_1D  , Convert_INT64_To_Strings_1D
!     Module Procedure    Convert_REAL32_To_String_0D , Convert_REAL32_To_String_1D , Convert_REAL32_To_Strings_1D
!     Module Procedure    Convert_REAL64_To_String_0D , Convert_REAL64_To_String_1D , Convert_REAL64_To_Strings_1D
!     Module Procedure    Convert_REAL128_To_String_0D, Convert_REAL128_To_String_1D, Convert_REAL128_To_Strings_1D
!     Module Procedure    Convert_String_To_String_0D , Convert_String_To_String_1D , Convert_String_To_Strings_1D
!   End Interface

  contains

! This procedure initializes a Logger object.
Module Procedure InitializeLogger
  use Logger_Tools_Module   ,only:  Get_OptOrDef_Value
  use Logger_Parameters     ,only:  Default_OpenStatus, Valid_OpenStatus, Default_OpenPosition, Valid_OpenPosition
  character(:)  ,allocatable                                            ::  OpenStatus
  character(:)  ,allocatable                                            ::  OpenPosition
  integer                                                               ::  ios
  call This%Free()
  call This%Activate()
  call SetInitialLoggerItem( This, Procedure, Indentation )
  call This%AddUnit( LoggerUnit_Type(FileName,Status,Position,i_Force_FileName) )
  This%Initialized  =   .True.
End Procedure






Module Procedure Activate
  This%Activated  =   .True.
End Procedure
Module Procedure Deactivate
  This%Activated  =   .False.
End Procedure

! This procedure free a Logger object.
Module Procedure FreeLogger
  call FinalizeLogger(This)
End Procedure

Module Procedure FinalizeLogger
  if (This%Initialized) call This%Close()
!   This%Unit         =   Output_Unit
  if ( allocated(This%Units) )  deallocate(This%Units)
  This%Initialized  =   .False.
  This%Advancing    =   .True.
  This%Activated    =   .True.
  This%iItem        =   0
  This%NItems       =   0
  This%NUnits       =   0
!   if ( allocated(This%FileName) )                     deallocate(This%FileName)
  if ( allocated(This%ActiveProcedures) )             deallocate(This%ActiveProcedures)
  if ( allocated(This%RecursivelyActiveProcedures) )  deallocate(This%RecursivelyActiveProcedures)
  if ( allocated(This%Items) )                        deallocate(This%Items)
  if ( associated(This%CurrentItem) )                 nullify(This%CurrentItem)
!   if ( allocated(This%CurrentItem) )  deallocate(This%CurrentItem)
End Procedure

! This procedure reopens a Logger object.
! The current Logger is first closed and deleted. Then, it is initialized using the same filename than the
! one it currently has. This will open a new file (with the same name but eventually in a different directory)
! with the poition is set to 'APPEND'.
! This procedure is mainly used when one wnat to change the working directory of the Fortran application.
! Once the application has changed directory, this 'Reopen' procedure is called to delete the logfile in the
! previous working directory, and to open it in the new working directory. The 'APPEND' position ensures
! that the informaton previously written to the logfile are not overwritten.
Module Procedure ReopenLogger
  integer                                                               ::  k
  do k = 1,This%NUnits
    call This%Units(k)%Reopen()
  end do
End Procedure

! Module Procedure ChangeLoggerDirectory
!   integer                                                               ::  i
!   character(:)  ,allocatable                                            ::  FileName                        ! Name of a log file
!   do i = 1,This%NUnits
!     if ( This%Units(i)%ToScreen ) cycle
!     FileName    =   This%GetFileName(i,AbsolutePath=.True.)
!     call Command%cp( FileName, Path )   ! Circular dep.
!     call This%Units(i)%ReOpen()
!   end do
! End Procedure


! This procedure closes a Logger object.
!   TODO: Add a checking that the "Status" string is a valid close status
Module Procedure CloseLogger
  integer                                                               ::  k
  character(:)  ,allocatable                                            ::  Status_                    !< Local value of the close status
  Status_  =       ""
  do k = 1,This%NUnits
    call This%Units(k)%Close( Status )
  end do
!   character(:)  ,allocatable                                            ::  Status_                    !< Local value of the close status
!   Status_  =       ""
!   if ( present(Status) ) then
!     Status_        =       Status
!     close( This%Unit, Status=Status_ )
!   else
!     close( This%Unit )
!   end if
End Procedure

! This procedure backspaces the logfile associated to a Logger object.
Module Procedure BackspaceLogger
  integer ,dimension(This%NUnits)                                       ::  ios
  integer                                                               ::  k
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    call This%Units(k)%Backspace( Status=ios(k) )
  end do
  if ( present(Status) ) Status = sum(ios)
!   integer                                                               ::  ios
!   backspace( This%Unit, iostat=ios )
!   if ( present(Status) ) Status = ios
End Procedure

! This procedure rewinds the logfile associated to a Logger object
Module Procedure RewindLogger
  integer ,dimension(This%NUnits)                                       ::  ios
  integer                                                               ::  k
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    call This%Units(k)%Rewind( Status=ios(k) )
  end do
  if ( present(Status) ) Status = sum(ios)
!   integer                                                               ::  ios
!   rewind( This%Unit, iostat=ios )
!   if ( present(Status) ) Status = ios
End Procedure

! This procedure flushes the unit associated to a Logger object
Module Procedure FlushLogger
  integer                                                               ::  k
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    call This%Units(k)%Flush()
  end do
!   flush( This%Unit )
End Procedure









Module Procedure Entering
  use Utilities_Library   ,only:  GetOptArgValue
  logical                                                               ::  LogInOut_
  logical                                                               ::  IsActive
  integer                                                               ::  Indentation
  type(LoggerItem_Type)                                                 ::  LoggerItem

!   if ( allocated(This%Units) ) then
!     if (size(This%Units) >= 1 ) then
!     write(This%Units(1)%Unit,*) ProcedureName
!     end if
!   end if

  if ( .Not.This%Activated ) return
  Indentation   =   This%GetIndentation()                                                                       ! Getting the current indentation levels
  call LoggerItem%Initialize( ProcedureName, Indentation, LogLevel, DefLogLevel, MsgLogLevel )                  ! Initializing a 'LoggerItem' object at current indentation level and with the input properties
  This%RecursivelyActive  =   This%RecursivelyActive .or. IsPresent(LoggerItem%Name,This%RecursivelyActiveProcedures)   ! Set the recursive activation indicator to its previous state or to true if current procedure is in the list of procedure to recursively activate
  if (This%RecursivelyActive) This%RecursivelyActiveCount = This%RecursivelyActiveCount + 1                               ! If Logger is currently recursively active, increment the count of procedures
  if ( LoggerItem%IsActive(ActiveProcedures=This%ActiveProcedures) .or. This%RecursivelyActive ) call LoggerItem%Indent()
  call This%AddItem( LoggerItem )                                                                               ! Adding the new 'LoggerItem' object to the list of 'LoggerItem' sub-objects
  LogInOut_   =   GetOptArgValue(LogInOut,DefaultLogInOut)
  if ( LogInOut_ .and. This%On() ) call This%Write(DefaultEnteringMessage)
End Procedure

Module Procedure Exiting
  use Utilities_Library   ,only:  GetOptArgValue
  logical                                                               ::  LogInOut_
  if ( .Not.This%Activated ) return
  LogInOut_   =   GetOptArgValue(LogInOut,DefaultLogInOut)
  if ( LogInOut_ .and. This%On() ) call This%Write(DefaultExitingMessage)
  if ( This%RecursivelyActive ) then                                                                            ! If recursive activation is set on, then ...
    This%RecursivelyActiveCount = This%RecursivelyActiveCount - 1                                               ! ... decrement the counter and ...
    if (This%RecursivelyActiveCount == 0 ) This%RecursivelyActive = .False.                                     ! ... deactivate recursive log if no procedure left
  end if
  call This%RemoveItem()                                                                                        ! Removing the last procedure from the list of nested procedures
End Procedure

Module Procedure StartDebugMode
  if ( .Not. associated(This%CurrentItem) ) return
  This%CurrentItem%SavedMsgLogLevel  =   This%CurrentItem%MsgLogLevel
  This%CurrentItem%MsgLogLevel       =   LogLevel_DEBUG
End Procedure

Module Procedure StopDebugMode
  if ( .Not. allocated(This%Items) ) return
  associate( LogLev => This%Items(This%iItem) )
    LogLev%MsgLogLevel       =   LogLev%SavedMsgLogLevel
  end associate
End Procedure

Module Procedure ChangeMsgLogLevel
  if ( .Not. associated(This%CurrentItem) ) return
  This%CurrentItem%SavedMsgLogLevel  =   This%CurrentItem%MsgLogLevel
  This%CurrentItem%MsgLogLevel       =   MsgLogLevel
End Procedure

Module Procedure RestoreMsgLogLevel
  if ( .Not. allocated(This%Items) ) return
  associate( LogLev => This%Items(This%iItem) )
    LogLev%MsgLogLevel       =   LogLev%SavedMsgLogLevel
  end associate
End Procedure

Module Procedure DecreaseMsgLogLevel
  if ( .Not. associated(This%CurrentItem) ) return
  This%CurrentItem%MsgLogLevel    =   This%CurrentItem%MsgLogLevel-1
End Procedure

Module Procedure IncreaseMsgLogLevel
  if ( .Not. associated(This%CurrentItem) ) return
  This%CurrentItem%MsgLogLevel    =   This%CurrentItem%MsgLogLevel+1
End Procedure

Function IsCurrentActive( This, ProcedureName, OptionalActive, DefaultActive ) result(WriteLogs_)
  class(Logger_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Logger object
  character(*)                                          ,intent(in)     ::  ProcedureName                   !< Name of the calling procedure
  logical                                     ,optional ,intent(in)     ::  OptionalActive
  logical                                     ,optional ,intent(in)     ::  DefaultActive
  logical                                                               ::  WriteLogs_

  logical                                                               ::  OptionalActive_
  logical                                                               ::  DefaultActive_
  integer                                                               ::  i

  DefaultActive_  = .False.
  if ( present(DefaultActive) ) DefaultActive_ = DefaultActive
  OptionalActive_ = DefaultActive_
  if ( present(OptionalActive) ) OptionalActive_ = OptionalActive

  WriteLogs_    =     OptionalActive_

  if ( allocated(This%ActiveProcedures) ) then
    do i = 1,size(This%ActiveProcedures)
      if ( .Not. ( LowerCase(ProcedureName) == LowerCase(This%ActiveProcedures(i)) ) ) cycle
      WriteLogs_  = .True.
      exit
    end do
  end if
!   if ( allocated(This%ActiveProcedures) ) then
!     if ( Procedure == This%ActiveProcedures(1) ) then ! @TODO
!       ActiveLoggerItem  = .True.
!     end if
!   end if
End Function


!   if ( allocated(This%ActiveProcedures) ) then
!     do i = 1,size(This%ActiveProcedures)
!       if ( .Not. ( LowerCase(ProcedureName) == LowerCase(This%ActiveProcedures(i)) ) ) cycle
!       LogLevel  = .True.
!       exit
!     end do
!   end if

!   if ( allocated(This%ActiveProcedures) ) then
!     if ( Procedure == This%ActiveProcedures(1) ) then ! @TODO
!       ActiveLoggerItem  = .True.
!     end if
!   end if

! Function ActivateLogs( This, ProcedureName, OptionalActive, DefaultActive ) result(WriteLogs_)
!   class(Logger_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Logger object
!   character(*)                                          ,intent(in)     ::  ProcedureName                       !< Name of the calling procedure
!   logical                                     ,optional ,intent(in)     ::  OptionalActive
!   logical                                     ,optional ,intent(in)     ::  DefaultActive
!   logical                                                               ::  WriteLogs_
!
!
!   logical                                                               ::  OptionalLogLevel_
!   logical                                                               ::  DefaultLogLevel_
!   integer                                                               ::  i
!
!   DefaultLogLevel_   = GetOptArgValue( LogLevel_DEFAULT, DefaultLogLevel  )
!   OptionalLogLevel_  = GetOptArgValue( DefaultLogLevel_, OptionalLogLevel )
!
!   if ( allocated(This%ActiveProcedures) ) then
!     do i = 1,size(This%ActiveProcedures)
!       if ( .Not. ( LowerCase(ProcedureName) == LowerCase(This%ActiveProcedures(i)) ) ) cycle
!       LogLevel  = .True.
!       exit
!     end do
!   end if
! !   if ( allocated(This%ActiveProcedures) ) then
! !     if ( Procedure == This%ActiveProcedures(1) ) then ! @TODO
! !       ActiveLoggerItem  = .True.
! !     end if
! !   end if
! End Function

! **************************************************************************************************************
!              PROCEDURES FOR ENFORCING THE ACTIVATION OF LOGS FOR A SET OF PROCEDURES
! **************************************************************************************************************
Module Procedure GetProcedureToLog
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  if ( allocated(This%ActiveProcedures) ) then
    allocate( ProcName(size(This%ActiveProcedures)), source = This%ActiveProcedures )
  end if
#else
  allocate( ProcName, source = This%ActiveProcedures )
#endif
End Procedure

Module Procedure AddActiveProcedures0d
  call AddElementToArray( ProcName, This%ActiveProcedures )
End Procedure

Module Procedure AddActiveProcedures1d
  call AddElementToArray( ProcName, This%ActiveProcedures )
End Procedure

Module Procedure RemoveActiveProcedures0d

  use Utilities_Library     ,only:  RemoveElementFromArray
  use String_Library        ,only:  GetPosition

  integer                                                               ::  i

  i   =   GetPosition( ProcName, This%ActiveProcedures, CaseSensitive=.False. )
  if ( i /= 0 ) call RemoveElementFromArray( This%ActiveProcedures, i )

  i   =   GetPosition( ProcName, This%RecursivelyActiveProcedures, CaseSensitive=.False. )
  if ( i /= 0 ) call RemoveElementFromArray( This%RecursivelyActiveProcedures, i )

End Procedure

Module Procedure RemoveActiveProcedures1d
!   use Utilities_Library     ,only:  RemoveElementFromArray
!   call AddElementToArray( ProcName, This%ActiveProcedures )
End Procedure

! **************************************************************************************************************
!              PROCEDURES FOR RECURSIVELY ENFORCING THE ACTIVATION OF LOGS FOR A SET OF PROCEDURES
! **************************************************************************************************************
Module Procedure GetRecursivelyActiveProcedures
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  if ( allocated(This%RecursivelyActiveProcedures) ) then
    allocate( ProcName(size(This%RecursivelyActiveProcedures)), source = This%RecursivelyActiveProcedures )
  end if
#else
  allocate( ProcName, source = This%RecursivelyActiveProcedures )
#endif
End Procedure

Module Procedure AddRecursivelyActiveProcedures_0d
  call AddElementToArray( ProcName, This%RecursivelyActiveProcedures )
End Procedure

Module Procedure AddRecursivelyActiveProcedures_1d
  call AddElementToArray( ProcName, This%RecursivelyActiveProcedures )
End Procedure

! **************************************************************************************************************
!                   PROCEDURES TO ACCESS THE PROPERTIES OF THE LOGGER OBJECT
! **************************************************************************************************************

! This procedure return a logical variable which indicates whether the 'Logger' is active (ie. it will should
! logs) for the default log-level of logging message of the current 'LoggerItem' level.
! If the 'Logger' object has no 'LoggerItem' sub-objects (ie. if the component 'CurrentItem' is not associated),
! then the Logger is always set as active. This behavior is required in order to write logs without having
! initialized the 'Logger' object through a call to 'Logger%Initialize'.
! A given log level can be passed as an optional input arguemnt. If so, the returned variable corresponds to a
! logical variable which indicates if the Logger is active for that particuler log level.
! @TODO: Comment: Note that ... override the Active indeicator if current procedure corresponds to a procedure
! which need to be logged
Module Procedure LoggerActive
  IsActive      =   .True.
  if ( .Not. This%Activated ) then
    IsActive    =   .False.
  else
    if ( associated(This%CurrentItem) ) then
      IsActive  =   This%CurrentItem%IsActive( LogLevel, This%ActiveProcedures  )
    end if
    if ( This%RecursivelyActive ) IsActive = .True.
  end if
End Procedure

! This procedure returns a character string corresponding to the prefix of the format used to write logs.
! The first step is to check wether the prefix need to be written for the current log message.
! The is done using two information:
! * the advancing mode of the previous log message which is stored in "This%Advancing"
! * the value of the optional input argument "PrefixPresence"
! When this procedure is called, the variable "This%Advancing" contains the advancing mode of the previous
! log message. By default, if the previous log message was in a non-advancing mode, then we do NOT want the
! prefix to be written for current log message.  Since the "This%Advancing" variable is set in the procedure
! "SetAdvancingMode", the current procedure must be called before the "SetAdvancingMode" procedure in order
! for the "This%Advancing" variable to contain the advancing mode of the previous log message and not of the
! current one. This default behavior of removing the prefix when the previous log message was in a non-advancing
! mode can be changed by using the optional argument "PrefixPresence". In other words, the optional argument
! "PrefixPresence" has predominance over the previous advancing mode. This enables a fine control of the log
! "message format.
! This prefix is made of two parts:
! * the indentation level
! * the name of the procedure
! * the logging level (if explicitely stated)
! @TODO: May be it is a good idea to set a full string in "FormatPrefix" instead of a format-string.
!        This will involve changing "3x" by 3 space, etc...
Module Procedure GetFormatPrefix
  use Logger_Tools_Module   ,only:  GetPrefixLogLevel
  logical                                                               ::  WritePrefix
  character(:)  ,allocatable                                            ::  PrefixProcedure
  WritePrefix   =   This%Advancing   ! If the previous log message was in a non-advancing mode, then by default do not write the prefix
  if ( present(PrefixPresence) ) WritePrefix = PrefixPresence
  if ( WritePrefix ) then
    PrefixProcedure = GetPrefixProcedure(This)
    if ( len_trim(PrefixProcedure) /= 0 ) PrefixProcedure = "'"//PrefixProcedure//"'"
    FormatPrefix  =   GetPrefixTimeStamp(This) &
                  //  GetPrefixIndentation(This,Error,Warning,Info,Debug,HeavyDebug)  &
                  //  PrefixProcedure &
                  //  GetPrefixLogLevel(Error,Warning,Info,Debug,HeavyDebug )
    if ( len_trim(FormatPrefix) > 1 ) then
      if ( This%Advancing ) FormatPrefix = FormatPrefix // ","  ! Only if in advancing mode, because when the cursor is not advancing, we do not want the FormatPrefix
    end if
    FormatPrefix    =   "(" // FormatPrefix
  else
    FormatPrefix    =   "("
  end if
End Procedure

Module Procedure GetPrefix_

  use Utilities_Library    ,only:  PresentAndTrue, AbsentOrTrue

  character(:)  ,allocatable                                            ::  PrefixPart

  Prefix  =   ""

  if ( AbsentOrTrue(Indent) )     then
    IndentPrefix: Block
      if ( .Not.associated(This%CurrentItem) ) exit IndentPrefix
      if ( This%CurrentItem%Indentation <= 0 ) exit IndentPrefix
      PrefixPart  =   repeat(" ",This%CurrentItem%Indentation)
      Prefix      =   Prefix // PrefixPart
    End Block IndentPrefix
  end if

  if ( AbsentOrTrue(Procedure) )  then
    Prefix  =   Prefix // GetPrefixProcedure(This)
  end if

End Procedure

! This procedure returns a character string corresponding to the path of the Logger in term of procedure names.
! This path corresponds to the list of all procedures the Logger went through.
! Each name are separated by the character
! @TODO: Define a optional input argument to select the Separator
Module Procedure GetPath
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  Separator = " > "
  Path        =       ""
  if ( allocated(This%Items) ) then
    do i = 1,size(This%Items)
    associate( LogLev => This%Items(i) )
      Path      =       Path // trim(LogLev%Name)
      if ( i /= size(This%Items) ) Path = Path // Separator
    end associate
    end do
  end if
  if ( present(ProcedureName) ) then
    if ( len_Trim(Path) == 0 ) then
      Path = Path // trim(ProcedureName)
    else
      Path = Path // Separator // trim(ProcedureName)
    end if
  end if
End Procedure

! This procedure returns a character string which corresponds to the name of the log level of the current
! LoggerItem object.
Module Procedure GetLogLevelName
  LogLevelName = ""
  if ( associated (This%CurrentItem) ) LogLevelName = LogLevelToString( This%CurrentItem%LogLevel )
!   if ( .Not. allocated(This%Items) ) return
!   LogLevel     =   LogLevelToString( This%Items(This%iItem)%LogLevel )
End Procedure

Module Procedure GetLogLevel
  LogLevel  =   0
  if ( associated (This%CurrentItem) ) LogLevel = This%CurrentItem%LogLevel
End Procedure

Module Procedure HasLogLevel
  Inidcator =   .False.
  if ( associated (This%CurrentItem) ) Inidcator = ( LogLevel == This%CurrentItem%LogLevel )
End Procedure

! This procedure returns an integer which corresponds to the indentation level of the current LoggerItem object.
Module Procedure GetIndentation
  Indentation   =   0
  if ( associated (This%CurrentItem) ) Indentation = This%CurrentItem%Indentation
End Procedure


! **************************************************************************************************************
!                           PROCEDURES RELATED TO UNITS
! **************************************************************************************************************

! This procedure adds an element to the list of 'LoggerUnit' sub-objects from a LoggerUnit object
Module Procedure AddLoggerUnitFromObj
  type(LoggerUnit_Type) ,allocatable                                    ::  LoggerUnits(:)
  if ( .not. allocated(This%Units) ) allocate( This%Units(0) )
  This%NUnits   =   size(This%Units)
  allocate( LoggerUnits(This%NUnits+1) )
  LoggerUnits(1:This%NUnits)  =   This%Units
  LoggerUnits(This%NUnits+1)  =   LoggerUnit
  call move_alloc( LoggerUnits, This%Units )
  This%NUnits   =   size(This%Units)
End Procedure

! This procedure adds an element to the list of 'LoggerUnit' sub-objects from a filename.
Module Procedure AddLoggerUnitFromFileName
  call This%AddUnit( LoggerUnit_Type(FileName) )
End Procedure

! This procedure removes an element from the list of 'LoggerUnit' sub-objects.
! The element to be removed is selected either from its index or its filename.
Module Procedure RemoveLoggerUnit
  integer                                                               ::  k
  integer                                                               ::  iRemove
  if ( present(FileName) ) then
    iRemove   =   0
    do k = 1,This%NUnits
      if ( FileName /= This%Units(k)%FileName ) cycle
      iRemove =   k
      exit
    end do
    if ( iRemove /= 0 ) call RemoveLoggerUnitFromIndex( This, iRemove )
  end if
  if ( present(Index) ) then
    iRemove   =   0
    if ( (Index > 0) .and. (Index <= This%NUnits) ) iRemove = Index
    if ( iRemove /= 0 ) call RemoveLoggerUnitFromIndex( This, iRemove )
  end if
End Procedure

! This procedure deactivate all units and only activate the ones specified in input.
Module Procedure ActivateOnlyUnit
  integer                                                               ::  i, k
  do k = 1,This%NUnits
    This%Units(k)%Active  =   .False.
  end do
  if ( present(Unit) ) then
    k     =   Unit
    if ( (k>0) .and. (k<=This%NUnits) )   This%Units(k)%Active = .True.
  end if
  if ( present(Units) ) then
    do i = 1,size(Units)
      k   =   Units(i)
      if ( (k>0) .and. (k<=This%NUnits) ) This%Units(k)%Active = .True.
    end do
  end if
End Procedure

! This procedure activate all units.
Module Procedure ActivateAllUnits
  integer                                                               ::  k
  do k = 1,This%NUnits
    This%Units(k)%Active  =   .True.
  end do
End Procedure




Purity Subroutine RemoveLoggerUnitFromIndex( This, iRemove )
  type(Logger_Type)                                     ,intent(inout)  ::  This
  integer                                               ,intent(in)     ::  iRemove
  integer                                                               ::  k
  type(LoggerUnit_Type) ,allocatable                                    ::  LoggerUnits(:)
  allocate( LoggerUnits(This%NUnits-1) )
  do k = 1,This%NUnits-1
    LoggerUnits(k)    =   This%Units(k)
  end do
  do k = This%NUnits+1,This%NUnits
    LoggerUnits(k-1)  =   This%Units(k)
  end do
  call move_alloc( LoggerUnits, This%Units )
  This%NUnits   =   size(This%Units)
End Subroutine

Module Procedure GetLoggerUnitFileName
  FileName      =   ""
  if ( .Not. allocated(This%Units) ) return
  if ( (i<1) .or. (i>This%NUnits) ) return
  FileName      =   This%Units(i)%GetFileName( AbsolutePath=AbsolutePath )
End Procedure

Module Procedure GetLoggerUnit
  if ( This%NUnits == 0 ) then
    Unit    =   Output_Unit
  else
    Unit    =   This%Units(This%NUnits)%Unit
  end if
End Procedure

Purity Module Function GetLoggerUnits( This ) result(Units)
  class(Logger_Type)                                    ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding the Logger object
  integer ,allocatable                                                  ::  Units(:)                            !< Unit numbers of all LoggerUnit objects loaded
  integer                                                               ::  k
  allocate( Units(This%NUnits)  )
  do k = 1,This%NUnits
    Units(k)  =   This%Units(k)%Unit
  end do
End Function


! **************************************************************************************************************
! **************************************************************************************************************
!                                   PRIVATE PROCEDURES USED TO SET THE LOGGER PARAMETERS
! **************************************************************************************************************
! **************************************************************************************************************

! ! This procedure sets the name of the file associated to the logger.
! ! If a coarray simulation is considered, then the index of the image is added as a suffix to the filename.
! ! This ensures that all images have different filenames. It is required since different images are not
! ! allowed to operate on the same file.
! Module Procedure SetFileName
! #ifdef COARRAY
!   character(10)                                                         ::  String                          ! Character string required to store the current image index (Required because write cannot make a length allocation)
! #endif
!   logical                                                               ::  i_Add_Image_Index
!   if ( .Not. present(FileName) ) then
!     This%FileName   =   "<STDOUT>"
!     return
!   end if
!   i_Add_Image_Index =   .True.
!   This%FileName     =   trim(adjustl(FileName))                                                         ! Setting the FileName of the log file
!   if ( present(i_Force_FileName) ) i_Add_Image_Index = .not. i_Force_FileName
! #ifdef COARRAY
!   if (i_Add_Image_Index) then
!     if ( This_Image() == 1 ) then                                                                                 ! If the 1st image is considered
!       This%FileName =   trim(FileName)                                                                  ! Setting the FileName of the log file
!     else                                                                                                          ! If an image other than the 1st one is considered, then adding the image index to the log file
!       write(String,"(i0)") This_Image()                                                                           ! Converting the image index into a string
!       This%FileName =   trim(FileName) // "_" // trim(adjustl(String))                                  ! Setting the FileName of the log file with the image index
!     end if                                                                                                        ! End if case on image index
!   end if
! #endif
! End Procedure

! This procedure adds an element to the list of 'LoggerItem' sub-objects and update the
! current index of log-level. Also, the number of 'LoggerItem' sub-objects is updated.
Module Procedure AddItem
  type(LoggerItem_Type)   ,dimension(:)   ,allocatable                  ::  LoggerItems
  if ( .not. allocated(This%Items) ) allocate( This%Items(0) )
  This%NItems      =   size(This%Items)
  allocate( LoggerItems(This%NItems+1) )
  LoggerItems(1:This%NItems)    =       This%Items
  LoggerItems(This%NItems+1)    =       LoggerItem
  call move_alloc( LoggerItems, This%Items )
  This%iItem      =   This%iItem + 1
  This%NItems      =   size(This%Items)
  This%CurrentItem  =>  This%Items(This%iItem)
End Procedure

! This procedure removes the last element from the list of 'LoggerItem' sub-objects and update
! the current index of log-level. Also, the number of 'LoggerItem' sub-objects is updated.
Module Procedure RemoveItem
  type(LoggerItem_Type)  ,dimension(:)   ,allocatable                  ::  LoggerItems
  if ( .not. allocated(This%Items) ) allocate( This%Items(0) )
  This%NItems      =   size(This%Items)
  allocate( LoggerItems, source=This%Items(1:This%NItems-1) )
  call move_alloc( LoggerItems, This%Items )
  This%iItem      =   This%iItem - 1
  This%NItems      =   size(This%Items)
  if ( (This%iItem>0) .and. (This%iItem<=This%NItems)  ) then
    This%CurrentItem  =>  This%Items(This%iItem)
  else
    This%CurrentItem  => null()
  end if
End Procedure

Module Procedure Set_Backspace
  if ( .Not. present(Backspace) ) return
  if ( .Not. Backspace ) return
  call This%Backspace()
End Procedure

Module Procedure Write_NewLine
  integer                                                               ::  k
  if ( present(NewLine) ) then
    if (NewLine) then
      do k = 1,This%NUnits
        if ( .Not. This%Units(k)%Active ) cycle
        write(This%Units(k)%Unit,"(a)")
      end do
    end if
!     if (NewLine) write(This%Unit,"(a)") ''
  end if
End Procedure

! Module Procedure Error_Open
!   write(*,"(4x,'[Initialize_Logger]: Error opening the Log file')")
! !   write(*,"(4x,'[Initialize_Logger]: FileName   = ',a)")  This%FileName
! !   write(*,"(4x,'[Initialize_Logger]: Unit   = ',i0)") This%Unit
!   write(*,"(4x,'[Initialize_Logger]: Stopping the code')")
!   error stop
! End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                           TOOLS
! **************************************************************************************************************
! **************************************************************************************************************
!
! ! This Function set and check a valid open status.
! ! If a valid optional open status is passed, then it is set other wise the default open status is taken
! Function Get_OptOrDef_Value( Default_Value, Valid_Values, Optional_Value ) result( Output_Value )
!   character(*)                                          ,intent(in)     ::  Default_Value                   !< Default value used if no optional value
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Valid_Values                    !< Valid values used to check validity of optional values if present
!   character(*)                                ,optional ,intent(in)     ::  Optional_Value                  !< Optional values used if present and valid
!   character(:)  ,allocatable                                            ::  Output_Value                    !< Output values
!   Output_Value  =       Default_Value
!   if ( present(Optional_Value) ) then
!     if ( Is_Valid(Optional_Value,Valid_Values) ) then
!       Output_Value = Optional_Value
!     else
!       call Error_Unvalid_Value( "", Optional_Value, Valid_Values )
!     end if
!   end if
! End Function
!
! Purity Function Is_Valid( Value, Valid_Values ) result(Valid)
!   implicit none
!   character(*)                                  ,intent(in)     ::  Value                                   !< Value to be checked for validity
!   character(*)          ,dimension( : )         ,intent(in)     ::  Valid_Values                            !< Valid values used for validity check
!   logical                                                       ::  Valid                                   !< Indicator of input object validity
!   integer                                                       ::  i                                       ! Index of valid strings
!   Valid         =       .False.                                                                                 ! Initialization of the object validity indicator to false
!   do i = 1,size(Valid_Values)                                                                                   ! Loop on all valid strings
!     if ( trim(Value) == trim(Valid_Values(i)) ) Valid = .True.                                                  ! If the object if found in the list of valid strings, then setting validity indicator to True
!   end do                                                                                                        ! End do loop on valid strings
! End Function

! ! Subroutine AddElementToArray( Element, Array )
! !   implicit none
! !   character(*)                                          ,intent(in)     ::  Element
! !   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
! !   integer                                                               ::  Length
! !   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
! ! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! !   integer                                                               ::  i
! ! #endif
! !   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
! !   Length        =       max( len(Array), len(Element) )
! !   allocate( character(Length) :: Array_tmp(size(Array)+1) )
! ! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! ! !   ------------------------------
! !   do i = 1,size(Array)
! !     Array_tmp(i)    =       Array(i)
! !   end do
! ! !   ------------------------------
! ! #else
! !   Array_tmp(1:size(Array))    =       Array     ! COMPILER_BUG:GFORTRAN
! ! #endif
! !   Array_tmp(size(Array)+1)    =       Element
! !   call move_alloc( Array_tmp, Array )
! ! End Subroutine

Purity Subroutine AddElementToArray_C0( Element, Array )
  character(*)                                          ,intent(in)     ::  Element
  character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
  character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
  if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
  allocate( List_Elements, source = [Array,Element] )
  call move_alloc( List_Elements, Array )
End Subroutine

Purity Subroutine AddElementToArray_C1( Elements, Array )
  character(*)  ,dimension(:)                           ,intent(in)     ::  Elements
  character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
  character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
  if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
  allocate( List_Elements, source = [Array,Elements] )
  call move_alloc( List_Elements, Array )
End Subroutine
!
! Purity Subroutine Remove_Element_From_Array( Array )
!   implicit none
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   integer                                                               ::  NElements
!   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
!   if ( allocated(Array) ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( Array_tmp(NElements-1), source=Array(1:NElements-1) )
! #else
!     allocate( Array_tmp, source=Array(1:NElements-1) )
! #endif
!     call move_alloc( Array_tmp, Array )
!   end if
! End Subroutine
!
! Purity Function VecTrim( Input_String ) result(Output_String)
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Input_String
!   character(:)  ,dimension(:)           ,allocatable                    ::  Output_String
!   integer                                                               ::  i
!   allocate( character(LenTrim(Input_String)) :: Output_String(size(Input_String)) )
!   do i = 1,size(Input_String)
!     Output_String(i)    =       trim( Input_String(i) )
!   end do
! End Function
!
! Purity Function LenTrim( Strings ) result( Length )
!   character(*)  ,dimension(:)                   ,intent(in)             ::  Strings                         !< Array of character string
!   integer                                                               ::  Length                          !< Maximum length without trailling blanks along all elements of the input string array
!   integer                                                               ::  i                               ! Index of string' elements
!   Length        =       0                                                                                       ! Initialization of maximum length of string
!   do i = 1,size(Strings,1)                                                                                      ! Loop on all elements
!     Length      =       max( Length, len_trim(Strings(i)) )                                                     ! Setting the maximum length
!   end do                                                                                                        ! End loop on all elements
! End Function
!

Purity Function LowerCase(StrInp) result(StrOut)
  character(*)                  ,intent(in)     ::  StrInp                                                  !<
  character(:)  ,allocatable                    ::  StrOut
  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =       len_trim(StrInp)
  ioffset       =       iachar('A')-iachar('a')
  iquote        =       0
  StrOut        =       StrInp
  do i = 1,ilen
    iav =       iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =       1
      iqc       =       iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =       0
      cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('A') .and. iav <= iachar('Z')) then
      StrOut(i:i)       =       achar(iav-ioffset)
    else
      StrOut(i:i)       =       StrInp(i:i)
    end if
  end do
  StrOut    =   trim(adjustl(StrOut))
End Function
! !
! Purity Function GetOptArgValue_Logical( VarDef, VarOpt ) result(VarLoc)
!   logical                                                       ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
!   logical                                             ,optional ,intent(in)     ::  VarOpt                  !< Optional varibale
!   logical                                                                       ::  VarLoc                  !< Local variable to be set
!   if ( present(VarOpt) ) then;  VarLoc = VarOpt                                                                 ! Setting the local variable to the optional variable if present ...
!   else;                         VarLoc = VarDef; end if                                                         ! ... otherwise setting the local variable to the default variable
! End Function
!
! Purity Function GetOptArgValue_Integer( VarDef, VarOpt ) result(VarLoc)
!   integer                                                       ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
!   integer                                             ,optional ,intent(in)     ::  VarOpt                  !< Optional varibale
!   integer                                                                       ::  VarLoc                  !< Local variable to be set
!   if ( present(VarOpt) ) then;  VarLoc = VarOpt                                                                 ! Setting the local variable to the optional variable if present ...
!   else;                         VarLoc = VarDef; end if                                                         ! ... otherwise setting the local variable to the default variable
! End Function
!
! Purity Function GetOptArgValue_Real( VarDef, VarOpt ) result(VarLoc)
!   use iso_fortran_env ,only:  REAL64
!   real(REAL64)                                                  ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
!   real(REAL64)                                        ,optional ,intent(in)     ::  VarOpt                  !< Optional varibale
!   real(REAL64)                                                                  ::  VarLoc                  !< Local variable to be set
!   if ( present(VarOpt) ) then;  VarLoc = VarOpt                                                                 ! Setting the local variable to the optional variable if present ...
!   else;                         VarLoc = VarDef; end if                                                         ! ... otherwise setting the local variable to the default variable
! End Function
!
! Purity Function GetOptArgValue_Character( VarDef, VarOpt ) result(VarLoc)
!   character(*)                                                  ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
!   character(*)                                        ,optional ,intent(in)     ::  VarOpt                  !< Optional varibale
!   character(:)  ,allocatable                                                    ::  VarLoc                  !< Local variable to be set
!   if ( present(VarOpt) ) then;  VarLoc = VarOpt                                                                 ! Setting the local variable to the optional variable if present ...
!   else;                         VarLoc = VarDef; end if                                                         ! ... otherwise setting the local variable to the default variable
! End Function


Purity Function LogLevelToString( LogLevel ) result(String)
  use Logger_Parameters     ,only:  LogLevelNames
  integer                                               ,intent(in)     ::  LogLevel
  character(:)  ,allocatable                                            ::  String
!   select case (LogLevel)
!     case( LogLevel_NOLOGS     ); String = "NOLOGS"
!     case( LogLevel_ERROR      ); String = "ERROR"
!     case( LogLevel_WARNING    ); String = "WARNING"
!     case( LogLevel_INFO       ); String = "INFO"
!     case( LogLevel_DEBUG      ); String = "DEBUG"
!     case( LogLevel_HEAVYDEBUG:); String = "HEAVYDEBUG"
!   end select
  String    =   ""
  if ( (LogLevel >= lbound(LogLevelNames,1)) .and. (LogLevel <= ubound(LogLevelNames,1)) ) then
    String  =   trim(LogLevelNames(LogLevel))
  end if
End Function
!
Purity Function IsPresent( Element, Array ) result(PresenceIndicator)
  character(*)                                          ,intent(in)     ::  Element
  character(:)  ,allocatable ,dimension(:)              ,intent(in)     ::  Array
  logical                                                               ::  PresenceIndicator
  integer                                                               ::  i
  PresenceIndicator   =   .False.
  if ( allocated(Array) ) then
    do i = 1,size(Array)
      if ( .Not. ( LowerCase(Element) == LowerCase(Array(i)) ) ) cycle
      PresenceIndicator  = .True.
      return
    end do
  end if
End Function

Subroutine SetInitialLoggerItem( This, Procedure, Indentation )
  type(Logger_Type)                                     ,intent(inout)  ::  This                        !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  Procedure            !< Names of the calling procedures to reach the current procedure. Each procedure neames are separated by the '>' character.  Only used by the Logger object
  integer                                     ,optional ,intent(in)     ::  Indentation                 !< Indentation level
  character(:)  ,allocatable                                            ::  ProcedureName
  character(:)  ,allocatable  ,dimension(:)                             ::  ProcedureNames                  ! Vector containing the name of the calling procedures
  type(LoggerItem_Type)                                                 ::  LoggerItem
  integer                                                               ::  i                               ! Index of procedure names
  integer                                                               ::  ItemIndent
  integer                                                               ::  Indentation_
  call GetProcedureNames( Procedure, ProcedureNames )
  Indentation_ =    2
  if ( present(Indentation) ) Indentation_ = Indentation
  ItemIndent   =    0
  do i = 1,size(ProcedureNames)
    ProcedureName   =   trim( ProcedureNames(i) )                                                                 ! Setting the procedure name used to construct the current Logger-Item object
    ItemIndent      =   ItemIndent + Indentation_                                                                 ! Setting the indentation level used to construct the current Logger-Item object
    call LoggerItem%Initialize( ProcedureName, ItemIndent )                                                       ! Initializing a 'LoggerItem' object
    call This%AddItem( LoggerItem )                                                                               ! Adding the new 'LoggerItem' object to the list of 'LoggerItem' sub-objects
  end do
!   call LoggerItem%Initialize( Procedure, Indentation )                                                          ! Initializing a 'LoggerItem' object with the input properties for the name of the procedure and the indentation level
!   call This%AddItem( LoggerItem )                                                                               ! Adding the new 'LoggerItem' object to the list of 'LoggerItem' sub-objects
End Subroutine


Subroutine GetProcedureNames( Procedures, Names )
  character(*)                                ,optional ,intent(in)     ::  Procedures                      !< Names of the calling procedures to reach the current procedure. Each procedure neames are separated by the '>' character.  Only used by the Logger object
  character(:)  ,allocatable  ,dimension(:)             ,intent(out)    ::  Names                           !< Vector containing the name of the calling procedures

  character(*)                                              ,parameter  ::  Separator = ">"                 ! Character separating the names of procedures
  integer                                                               ::  iSep                            ! Position iof the separator character
  integer                                                               ::  NProc                           ! Number of procedure names
  integer                                                               ::  TotLength                       ! Length of the input character string
  integer                                                               ::  MaxLength                       ! Maximum length of the procedure names
  integer                                                               ::  i                               ! Index of procedure names
  character(:)  ,allocatable                                            ::  CurrentNames
  character(:)  ,allocatable                                            ::  LeftNames

  TotLength =   len_trim(Procedures)

  if ( .Not. present(Procedures) ) then
    allocate( character(0) :: Names(0) )
    return
  end if

  if ( TotLength == 0 ) then
    allocate( character(0) :: Names(0) )
    return
  end if

  LeftNames     =   Procedures                                                                                ! Copying the input character string
  NProc         =   0                                                                                         ! Initializing the number of procedures
  MaxLength     =   0                                                                                         ! Initializing the length of procedures
  do                                                                                                          ! Loop
    if ( len_trim(LeftNames) == 0 ) exit                                                                      ! If the string which still to be processed, then exiting the loop
    iSep        =       index(LeftNames,Separator)                                                            ! Getting the position of the separator character
    if ( iSep == 0  ) then                                                                                    ! If the separator character has not been found, then there is only one procedure
      CurrentNames  =   LeftNames                                                                             ! Setting the name of current procedure to the entire string
      LeftNames     =   ''                                                                                    ! Setting the string which still to be processed to an empty string
    else                                                                                                      ! If the separator character has not been found, then
      CurrentNames  =   trim( LeftNames(1:iSep-1) )                                                           ! Setting the name of current procedure to the string at the LHS of the separator
      if ( iSep == TotLength ) then; LeftNames   =   ''                                                       ! If the separator is the last character, then setting the string which still to be processed to an empty string
      else; LeftNames = LeftNames(iSep+1:); end if                                                            ! ... otherwise setting the string which still to be processed to the string at the RHS of the separator
    end if                                                                                                    ! End if case on the presence of the separator
    NProc       =   NProc + 1                                                                                 ! Incrementing the number of procedures
    MaxLength   =   max( MaxLength , len_trim(CurrentNames) )                                                 ! Updating the maximum length of the procedure names
  end do

  allocate( character(MaxLength) :: Names(NProc) )
  LeftNames     =   Procedures                                                                                ! Copying the input character string
  i             =   0                                                                                         ! Initializing the index of procedures name
  do                                                                                                          ! Loop
    if ( len_trim(LeftNames) == 0 ) exit                                                                      ! If the string which still to be processed, then exiting the loop
    iSep        =       index(LeftNames,Separator)                                                            ! Getting the position of the separator character
    if ( iSep == 0  ) then                                                                                    ! If the separator character has not been found, then there is only one procedure
      CurrentNames  =   LeftNames                                                                             ! Setting the name of current procedure to the entire string
      LeftNames     =   ''                                                                                    ! Setting the string which still to be processed to an empty string
    else                                                                                                      ! If the separator character has not been found, then
      CurrentNames  =   trim( LeftNames(1:iSep-1) )                                                           ! Setting the name of current procedure to the string at the LHS of the separator
      if ( iSep == TotLength ) then; LeftNames   =   ''                                                       ! If the separator is the last character, then setting the string which still to be processed to an empty string
      else; LeftNames = LeftNames(iSep+1:); end if                                                            ! ... otherwise setting the string which still to be processed to the string at the RHS of the separator
    end if                                                                                                    ! End if case on the presence of the separator
    i               =   i + 1                                                                                 ! Incrementing the names index
    Names(i)        =   CurrentNames                                                                          ! Setting the current procedure name
  end do
End Subroutine


!
!
! ! **************************************************************************************************************
! !                           PRIVATE PROCEDURES RELATED TO THE LOG MESSAGE PREFIX
! ! **************************************************************************************************************

! This procedure sets the "TimeStamp" part of the format's prefix.
Purity Function GetPrefixTimeStamp( This ) result(Prefix)
  type(Logger_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument
  character(:)  ,allocatable                                            ::  Prefix                          !< Character string corresponding to the "procedure" part of the format's prefix
  Prefix       =       ""
  return
  if (This%Initialized) then; end if
End Function


! This procedure sets the "indentation" part of the format's prefix.
Purity Function GetPrefixIndentation( This, Error, Warning, Info, Debug, HeavyDebug ) result(Prefix)
  use Utilities_Library    ,only:  PresentAndTrue
  type(Logger_Type)                                     ,intent(in)     ::  This                                !< Passed-object dummy argument
  logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
  logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
  logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
  logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
  logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
  character(:)  ,allocatable                                            ::  Prefix                              !< Character string corresponding to the indentation part of the format's prefix
  integer                                                               ::  Indentation
  character(10)                                                         ::  LongString
  character(1)                                                          ::  Letter
  Prefix        =   ""
  if ( .Not. associated(This%CurrentItem) ) return
  Indentation   =   This%CurrentItem%Indentation
  if ( Indentation <= 0 ) return
  Letter        =   ""
  if ( PresentAndTrue(Error) )      Letter = "E"
  if ( PresentAndTrue(Warning) )    Letter = "W"
  if ( PresentAndTrue(Info) )       Letter = "I"
  if ( PresentAndTrue(Debug) )      Letter = "D"
  if ( PresentAndTrue(HeavyDebug) ) Letter = "H"
  if ( len_trim(Letter) == 1 ) then
    Indentation =   Indentation - 1
    Prefix      =   "'"//Letter//"',"
  end if
# ifndef WORKAROUND_GFORTRAN_INTERNAL_WRITE
    write(LongString,"(i0)") Indentation
    Prefix      =   Prefix // trim(adjustl(LongString)) // "x,"
# else
    Prefix      =   Prefix // "'" // repeat(" ",Indentation) // "',"
# endif
End Function


! This procedure sets the "procedure" part of the format's prefix.
Module Procedure GetPrefixProcedure
  Prefix       =       ""
  if ( associated(This%CurrentItem) ) then
    if ( allocated(This%CurrentItem%Name) ) then
      Prefix    =       "[" // trim(This%CurrentItem%Name) // "]: "
    end if
  end if
End Procedure

! ! This procedure sets the "LogLevel" part of the format's prefix.
! Purity Function GetPrefixLogLevel( Error, Warning, Info, Debug, HeavyDebug ) result(Prefix)
!   logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
!   logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
!   logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
!   logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
!   logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
!   character(:)  ,allocatable                                            ::  Prefix
!   Prefix  = ""
!   if ( PresentAndTrue(Error) )       Prefix = ",'<ERROR> '"
!   if ( PresentAndTrue(Warning) )     Prefix = ",'<WARNING> '"
!   if ( PresentAndTrue(Info) )        Prefix = ",'<INFO> '"
!   if ( PresentAndTrue(Debug) )       Prefix = ",'<DEBUG> '"
!   if ( PresentAndTrue(HeavyDebug) )  Prefix = ",'<HEAVYDEBUG> '"
! End Function
!
!
!
! Subroutine Error_Unvalid_Value( Type_Value, Value, Valid_Values )
!   character(*)                                          ,intent(in)     ::  Type_Value                      !< Type of value
!   character(*)                                          ,intent(in)     ::  Value                           !< Erroneous value
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Valid_Values                    !< Valid values
!   write(*,"(4x,'[Error_Unvalid_Value]: Error: Unvalid value ')")
!   write(*,"(4x,'[Error_Unvalid_Value]: Type_Value   = ',a)") Type_Value
!   write(*,"(4x,'[Error_Unvalid_Value]: Value        = ',a)") Value
!   write(*,"(4x,'[Error_Unvalid_Value]: Valid_Values = ',*(a,3x))") Valid_Values(:)
!   write(*,"(4x,'[Error_Unvalid_Value]: Stopping the code')")
!   stop
! End Subroutine


End SubModule
