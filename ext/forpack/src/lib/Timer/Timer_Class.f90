Module Timer_Class

  use cpuTime_Class            ,only:  cpuTime_Type
  use sysTime_Class            ,only:  sysTime_Type

  implicit none

  private
  public        ::  Timer_Type

  Type                                                  ::  Timer_Type
!     private
    character(:)        ,allocatable                    ::  Name                                            !< Name of current Timer object
    Type(cpuTime_Type)                                  ::  CPU                                             !< Time object associated to the CPU time
    Type(sysTime_Type)                                  ::  SYS                                             !< Time object associated to the system_clock time
    logical                                             ::  Running   =   .False.                           !< Status of the Timer object
    logical                                             ::  Locked    =   .False.                           !< Status of the Timer object
    integer                                             ::  NCount    =   0                                 !< Count of the Timer object
    integer                                             ::  iSubTimer =   0                                 !< Index of the current SubTimer (actually, this index referes to the last SubTimer which has been started)
    type(Timer_Type)    ,dimension(:)   ,pointer        ::  SubTimers => null()
  contains
!   Procedures to initialize or to get the timer properties
!     Final                               ::  FinalizeTimer  !@COMPILER_BUG: This procedure causes a seg. fault for some application                                                 !< Finalizes the Timer object
    procedure   ,public                 ::  Initialize  =>    InitializeTimer                               !< Initializes a Timer object
    procedure   ,public                 ::  Start       =>    StartTimer                                    !< Starts the Timer
    procedure   ,public                 ::  Stop        =>    StopTimer                                     !< Stops the Timer
    procedure   ,public                 ::  Reset       =>    ResetTimer                                    !< Resets the Timer
    procedure   ,public                 ::  Lock        =>    LockTimer                                     !< Lock the Timer
    procedure   ,public                 ::  UnLock      =>    UnLockTimer                                   !< Unlock the Timer
    procedure   ,public                 ::  Tag                                                             !< Tag the Timer (@NOT_IMPLEMENTED)
!   Procedure to get the timer properties
    procedure   ,public                 ::  IsRunning
    procedure   ,public                 ::  GetNumberSubTimers
    procedure   ,public                 ::  GetNamesLength
    procedure   ,public                 ::  GetName
    procedure   ,public                 ::  GetTime
    procedure   ,public                 ::  GetCPUTime
    procedure   ,public                 ::  GetSystemTime
    procedure   ,public                 ::  GetElapsedTime
    procedure   ,public                 ::  GetCPUElapsedTime
    procedure   ,public                 ::  GetSystemElapsedTime
!   Procedures to operate on sub-timers
    procedure   ,public                 ::  AddSubTimer                                                     !< Adds a SubTimer to the list of sub-timers associated to the input timer
    procedure   ,public                 ::  StartSubTimer                                                   !< Starts a SubTimer
    procedure   ,public                 ::  StopSubTimer                                                    !< Stops the SubTimer currently running
    procedure   ,public                 ::  NextSubTimer                                                    !< Stops the SubTimer currently running and starts the next one
    procedure   ,public                 ::  SetTimeFromSubTimers                                            !< Sets the time of a Timer object as the sum of the individual times stores in the SubTimers components
    procedure   ,public                 ::  IsSubTimerRunning
    procedure   ,public                 ::  IsValidSubTimerIndex
!   Procedures to output the timer properties
    generic     ,public                 ::  Output      =>    OutputToString, OutputToFile, OutputToLogger  !< Outputs the Timer properties
    procedure   ,private                ::  OutputToString                                                  !< Outputs the Timer properties to a string
    procedure   ,private                ::  OutputToFile                                                    !< Outputs the Timer properties to a file
    procedure   ,private                ::  OutputToLogger                                                  !< Outputs the Timer properties to the Logger
  End type

  Interface

    ! ==============================================================================================================
    !    PROCEDURES TO INITIALIZE OR TO GET THE TIMER PROPERTIES
    ! ==============================================================================================================
    Module Subroutine InitializeTimer( This, Name, SubTimerNames, Start, Locked )
      class(Timer_Type)                                     ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the Timer object to be constructed
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  SubTimerNames                   !< Name of the sub-timers imside the Timer object to be constructed
      logical                                     ,optional ,intent(in)     ::  Start                           !< Indicator whether the Timer has to be started during the initialization
      logical                                     ,optional ,intent(in)     ::  Locked                          !< Timer locking indicator after creation
    End Subroutine
    Module Subroutine FinalizeTimer( This )
      type(Timer_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object to be finalize
    End Subroutine
    Module Subroutine StartTimer( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine
    Recursive Module Subroutine StopTimer( This, Time )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      real(8)                                     ,optional ,intent(out)    ::  Time                            !< Time in seconds
    End Subroutine
    Pure Module Subroutine Tag( This, Name )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Timer name
    End Subroutine
    Recursive Pure Module Subroutine ResetTimer( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine
    Pure Module Subroutine LockTimer( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine
    Pure Module Subroutine UnLockTimer( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine

    ! ==============================================================================================================
    !    PROCEDURES TO GET THE TIMER PROPERTIES
    ! ==============================================================================================================
    Pure Elemental Module Function IsRunning( This ) result(Running)
      class(Timer_Type)                             ,intent(in)             ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      logical                                                               ::  Running                         !< Indicator whether the timer is currently running
    End Function
    Pure Elemental Module Function GetNumberSubTimers( This ) result(NSubTimer)
      class(Timer_Type)                             ,intent(in)             ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                                               ::  NSubTimer                       !< Number of sutimers
    End Function
    Recursive Module Function GetNamesLength( This ) result(Length)
      class(Timer_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                       ::  Length                          !< Maximum length of all Timer names
    End Function
    Pure Module Function GetName( This, ID ) result(Name)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      character(:)  ,allocatable                                            ::  Name
    End Function
    Module Function GetTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function
    Module Function GetCPUTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function
    Module Function GetSystemTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function
    Module Function GetElapsedTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function
    Module Function GetCPUElapsedTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function
    Module Function GetSystemElapsedTime( This, ID ) result(Time)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer
      real(8)                                                               ::  Time                            !< Time in seconds
    End Function

    ! ==============================================================================================================
    !    PROCEDURES PROCEDURES TO OPERATE ON SUB-TIMERS
    ! ==============================================================================================================
    Module Subroutine AddSubTimer( This, Name, ID )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the sub-timer to be added
      integer                                     ,optional ,intent(out)    ::  ID                              !< ID of the sub-timer to be added
    End Subroutine
    Module Subroutine StartSubTimer( This, ID )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer to be started
    End Subroutine
    Module Subroutine StopSubTimer( This, ID )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                     ,optional ,intent(in)     ::  ID                              ! Index of the SubTimer to be stopped
    End Subroutine
    Module Subroutine NextSubTimer( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine
    Pure Elemental Module Subroutine SetTimeFromSubTimers( This )
      class(Timer_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
    End Subroutine
    Pure Elemental Module Function IsSubTimerRunning( This, ID ) result(Running)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                               ,intent(in)     ::  ID                              ! Index of the SubTimer to be started
      logical                                                               ::  Running
    End Function
    Pure Elemental Module Function IsValidSubTimerIndex( This, ID ) result(IsValid)
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                               ,intent(in)     ::  ID                              ! Index of the SubTimer to be started
      logical                                                               ::  IsValid
    End Function



    ! ==============================================================================================================
    !    PROCEDURES TO OUTPUT THE TIMER PROPERTIES
    ! ==============================================================================================================
    Module Subroutine OutputToString( This, Lines, CPUTime, SystemTime, SubTimers, Remainder )
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Lines
      logical                                     ,optional ,intent(in)     ::  CPUTime
      logical                                     ,optional ,intent(in)     ::  SystemTime
      logical                                     ,optional ,intent(in)     ::  SubTimers
      logical                                     ,optional ,intent(in)     ::  Remainder
    End Subroutine
    Module Subroutine OutputToFile( This, Unit, CPUTime, SystemTime, SubTimers, Remainder )
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      integer                                               ,intent(in)     ::  Unit
      logical                                     ,optional ,intent(in)     ::  CPUTime
      logical                                     ,optional ,intent(in)     ::  SystemTime
      logical                                     ,optional ,intent(in)     ::  SubTimers
      logical                                     ,optional ,intent(in)     ::  Remainder
    End Subroutine
    Module Subroutine OutputToLogger( This, CPUTime, SystemTime, SubTimers, Remainder )
      class(Timer_Type)                                     ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Timer object
      logical                                     ,optional ,intent(in)     ::  CPUTime
      logical                                     ,optional ,intent(in)     ::  SystemTime
      logical                                     ,optional ,intent(in)     ::  SubTimers
      logical                                     ,optional ,intent(in)     ::  Remainder
    End Subroutine
  End Interface

End Module
