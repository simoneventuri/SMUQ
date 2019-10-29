SubModule(Timer_Class) Timer_SubClass

  use Logger_Class                ,only:  Logger

  implicit none

  real(8)       ,parameter              ::  Zero    =       0.0_8

! The Timer object defines some Sub-Timer components. These sub-Timers are made to be stated/stopped sequencally.
! This enable to perfomed the timing of different part of the code.
!
! call Timer%Start
! ...
! call Timer%Start
! ...
! call Timer%Stop
! ...

  contains

Module Procedure InitializeTimer
  integer                                                               ::  i                               ! Index of sub-timers objects
  call InitializeMainTimer( This, Name, Start, Locked )
  if ( present(SubTimerNames) ) then
    do i = 1,size(SubTimerNames)
      call This%AddSubTimer( Name=trim(SubTimerNames(i)) )
    end do
  end if
End Procedure

Subroutine InitializeMainTimer( This, Name, Start, Locked )
  type(Timer_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Timer object
  character(*)                                ,optional ,intent(in)     ::  Name                            !< Timer name
  logical                                     ,optional ,intent(in)     ::  Start                           !< Indicator whether the Timer has to be started during the initialization
  logical                                     ,optional ,intent(in)     ::  Locked                          !< Timer locking indicator after creation
  This%Name     =       ""                                                                                      ! Initializing the timer name to an empty string
  if ( present(Name) ) This%Name = Name                                                                         ! Setting the Timer name to the input value if present
  call This%CPU%Initialize( Name="CPU" )                                                                        ! Initializing the cpuTimer time object
  call This%SYS%Initialize( Name="SYS" )                                                                        ! Initializing the System_Time time object
!   if ( allocated(This%SubTimers) ) deallocate(This%SubTimers)
  if ( associated(This%SubTimers) ) deallocate(This%SubTimers); nullify(This%SubTimers)
  allocate( This%SubTimers(0) )
!   call This%Date_Time%Initialize(   Name="Date_and_Time"  )                                                     ! Initializing the Date_Time time object
  call This%Reset()                                                                                             ! Resetting the Timer
  This%Locked           =       .False.
  This%iSubTimer        =       0
  allocate( This%SubTimers(0) )
  if ( present(Start) ) then
    if (Start) call This%Start()
  end if
  if ( present(Locked) ) then
    if ( Locked ) call This%Lock()
  end if
End Subroutine

Module Procedure FinalizeTimer
  call This%Stop()
!   if ( associated( This%SubTimers ) ) deallocate( This%SubTimers )

!     character(:)        ,allocatable                    ::  if ( allocated(This%Name                                            !< Name of current Timer object
!     Type(cpuTime_Type)                                  ::  This%CPU                                             !< Time object associated to the CPU time
!     Type(sysTime_Type)                                  ::  This%SYS                                             !< Time object associated to the system_clock time
!     logical                                             ::  This%Running   =   .False.                           !< Status of the Timer object
!     logical                                             ::  This%Locked    =   .False.                           !< Status of the Timer object
!     integer                                             ::  This%NCount    =   0                                 !< Count of the Timer object
!     integer                                             ::  This%iSubTimer =   0                                 !< Index of the current SubTimer (actually, this index referes to the last SubTimer which has been started)
!     type(Timer_Type)    ,dimension(:)   ,pointer        ::  SubTimers
!   write(*,"('[FinalizeTimer]: associated(This%SubTimers) = ',g0)") associated(This%SubTimers)
!   if (associated(This%SubTimers)) then
!     write(*,"('[FinalizeTimer]: size(This%SubTimers) = ',g0)") size(This%SubTimers)
!   end if
!   do i = 1,size(This%SubTimers)
!     call This%SubTimers(i)%Stop()               !     call This%StopSubTimer( i )      ! Identical but should the direct way is preferred
!   end do

  if ( associated(This%SubTimers) ) nullify( This%SubTimers )
End Procedure

! This procedure starts the Timer if it is in an unlocked state.
! If the Timer is locked, then nothing is done and the procedure is exited.
! If the Timer is in unlocked state, then the following actions are permfored:
!  - the Running indicator is set to true,
!  - the Count counter is incremented and,
!  - each individual timer are started
! That's it.
Module Procedure StartTimer
  if ( This%Locked ) return                                                                                     ! Exiting the procedure if the Timer is locked
  This%Running          =       .True.                                                                          ! Setting the Timer status as running
  This%NCount           =       This%NCount + 1                                                                 ! Incrementing the Timer count
  call This%CPU%Start_Timing()                                                                                  ! Setting the start time for the cpuTime Time object
  call This%SYS%Start_Timing()                                                                                  ! Setting the start count for the System_Time Time objects
!   This%Date_Time%Start          =       Get_Date_Time()                                                         ! Setting the start time for the Date_Time Time objects
End Procedure

! This procedure stops the Timer if it is in an unlocked state and if it is not currently running.
! If the Timer is locked or if it is not currently running, then nothing is done and the procedure is exited.
! Otherwise, the following actions are permfored:
!  - the Running indicator is set to false,
!  - the Count counter is incremented and,
!  - each sub-timer are stopped
!  - each individual timer computes the elapsed time from the time the Timer was started from the current time.
Module Procedure StopTimer
  integer                                                               ::  i
  if ( .not. This%Running ) return                                                                              ! Exiting the procedure if the Timer is not running
  if ( This%Locked ) return                                                                                     ! Exiting the procedure if the Timer is locked
  This%Running                  =       .False.
  do i = 1,size(This%SubTimers)
    call This%SubTimers(i)%Stop()               !     call This%StopSubTimer( i )      ! Identical but should the direct way is preferred
  end do
  call This%CPU%Stop_Timing()
  call This%SYS%Stop_Timing()

  if ( present(Time) ) then
    Time    =   This%GetTime()
  end if

End Procedure

Module Procedure ResetTimer
  integer                                                               ::  i
  This%Running          =       .False.
  This%Locked           =       .False.
  This%NCount           =       0
  This%iSubTimer        =       0
  call This%CPU%ResetTime()
  call This%SYS%ResetTime()
  do i = 1,size(This%SubTimers)
    call This%SubTimers(i)%Reset()
  end do
End Procedure

Module Procedure LockTimer
  This%Locked           =       .True.                                                                  ! Setting the locking indicator to true
End Procedure

Module Procedure UnLockTimer
  This%Locked           =       .False.                                                                 ! Setting the locking indicator to true
End Procedure


Module Procedure Tag
!   use TimerUtilities_Library       ,only:  GetCPUTime, GetSystemClockTime
!   integer       ::  This_NTags
  if ( .not. This%Running ) return                                                                              ! Exiting the procedure if the Timer is not running
  return
  if (present(Name)) then; end if
! !   @TODO: Timer tagging not implemented
!   This_NTags    = 0
!   This_NTags    =       This_NTags + 1                                                                          ! Incrementing the numbers of tagged time regions
!   call move alloc( This%Tags, Tags_Local
!   allocate( This%Tagged(This_NTags) )
!   This%Tagged(This_NTags)%NAme = Name
!   This%Running                  =       .False.
!   This%CPU%dt   =       This%CPU%GetElapsedTime() + ( GetCPUTime() - This%CPU%Start )
!   This%SYS%dt   =       This%SYS%GetElapsedTime() + Get_SYS_Time( int(This%SYS%Start) )
End Procedure


! ==============================================================================================================
!    PROCEDURES TO GATHER INFORMATION ABOUT THE TIMER PROPERTIES
! ==============================================================================================================
Module Procedure IsRunning
  Running = This%Running
End Procedure

Module Procedure GetNumberSubTimers
  NSubTimer = 0
  if ( associated(This%SubTimers) ) NSubTimer = size(This%SubTimers)
End Procedure

Module Procedure GetNamesLength
  integer                                                       ::  i
  Length        =       len_trim(This%Name)
  if ( .Not. associated(This%SubTimers) ) return
  do i = 1,size(This%SubTimers)
    Length      =       max( Length, This%SubTimers(i)%GetNamesLength() )
  end do
End Procedure

Module Procedure  GetName
  if ( .Not. present(ID) ) then
    Name    =   This%Name
  else
    Name    =   ""
    if ( This%IsValidSubTimerIndex(ID) ) Name = This%SubTimers(ID)%Name
  end if
End Procedure

Module Procedure GetTime
  Time      =   This%GetSystemTime()
End Procedure

Module Procedure GetCPUTime
  if ( .Not. present(ID) ) then
    Time    =   This%CPU%GetTime()
  else
    Time    =   0.0_8
    if ( This%IsValidSubTimerIndex(ID) ) Time = This%SubTimers(ID)%CPU%GetTime()
  end if
End Procedure

Module Procedure GetSystemTime
  if ( .Not. present(ID) ) then
    Time    =   This%SYS%GetTime()
  else
    Time    =   0.0_8
    if ( This%IsValidSubTimerIndex(ID) ) Time = This%SubTimers(ID)%SYS%GetTime()
  end if
End Procedure

Module Procedure GetElapsedTime
  Time      =   This%GetSystemElapsedTime(ID)
End Procedure

Module Procedure GetCPUElapsedTime
  if ( .Not. present(ID) ) then
    Time    =   This%CPU%GetElapsedTime()
  else
    Time    =   0.0_8
    if ( This%IsValidSubTimerIndex(ID) ) Time = This%SubTimers(ID)%CPU%GetElapsedTime()
  end if
End Procedure

Module Procedure GetSystemElapsedTime
  if ( .Not. present(ID) ) then
    Time    =   This%SYS%GetElapsedTime()
  else
    Time    =   0.0_8
    if ( This%IsValidSubTimerIndex(ID) ) Time = This%SubTimers(ID)%SYS%GetElapsedTime()
  end if
End Procedure




! ==============================================================================================================
!    PROCEDURES RELATED TO SUB-TIMERS
! ==============================================================================================================

! This procedure adds a sub-timer to the list of sub-timers associated to the input timer
Module Procedure AddSubTimer
  type(Timer_Type)      ,dimension(:)   ,allocatable                    ::  List_SubTimers
  type(Timer_Type)                                                      ::  New_SubTimer
  call New_SubTimer%Initialize( Name = Name )
  allocate( List_SubTimers, source = [This%SubTimers,New_SubTimer] )
  deallocate( This%SubTimers )
  allocate( This%SubTimers, source = List_SubTimers )
  if ( present(ID) ) ID = size(This%SubTimers)
End Procedure

! This procedure starts a given SubTimer iif the current Timer is running.
! The index of the SubTimer to be started can be explicitly specified using the "ID" optional argument.
! Note that if this index is out of bounds then the pocedure is simply exited, no error is reported.
! If no "ID" optional argument is provided, then the first SubTimer is started.
Module Procedure StartSubTimer
  integer                                                               ::  i                               ! Local value of the index of the SubTimer to be started
  if ( .not. This%Running ) return                                                                              ! Exiting the procedure if the Timer is not running
  if (present(ID)) then; i = ID                                                                                 ! Setting the SubTimer index to the input value if present...
  else;                  i = 1;  end if                                                                         ! ... or to the first SubTimer is absent
  if ( .Not. This%IsValidSubTimerIndex(i) ) return                                                              ! Exiting the procedure is the selected SubTimer index is out of bounds
  call This%SubTimers(i)%Start()                                                                                ! Starting the selected SubTimer
  This%iSubTimer        =       i                                                                               ! Saving the index of current SubTimer, which corresponds to the last SubTimer which has been started
End Procedure

! This procedure stops a given SubTimer iif the current Timer is running.
! The index of the SubTimer to be stopped can be explicitly specified using the "ID" optional argument.
! Note that if this index is out of bounds then the pocedure is simply exited, no error is reported.
! IF no "ID" optional argument is provided, then the last SubTimer which has been started is stopped, this
! particular SubTimer being retrieved using the This%iSubTimer component.
! Here, there is a problem with the variable "iSubTimer" since once this procedure is called, iSubTimer does not have the correct value.
! However, it a simple de-incrementation of its value will not work.
! I must think of a clever way to deal with it!
! In the meantime, This procedure should not be used. Insteard, the Start/Next procedure should be use.
! A possible way (but not fully satisfactory) is to set the This%iSubTimer variable to 0 once  a SubTimer is stopped.
Module Procedure StopSubTimer
  integer                                                               ::  i                               ! Local value of the index of the SubTimer to be stopped
  if ( .not. This%Running ) return                                                                              ! Exiting the procedure if the Timer is not running
  if (present(ID)) then; i = ID                                                                                 ! Setting the SubTimer index to the input value if present...
  else;                  i = This%iSubTimer;  end if                                                            ! ... or to the last SubTimer which has been started is absent, whose index if given by This%iSubTimer
  if ( .Not. This%IsValidSubTimerIndex(i) ) return                                                              ! Exiting the procedure is the selected SubTimer index is out of bounds
  call This%SubTimers(i)%Stop()                                                                                 ! Stopping the selected SubTimer
  This%iSubTimer        =       0                                                                               ! Setting the zero the index of the last SubTimer which has been started since this information has been lost
End Procedure

! This procedure stops the SubTimer which is currently running and starts the next one, if any.
Module Procedure NextSubTimer
  integer                                                               ::  i                               ! SubTimer index
  if ( .not. This%Running ) return                                                                              ! Exiting the procedure if the Timer is not running
  i     =       This%iSubTimer                                                                                  ! Saving the index of the last started SubTimer
  call This%StopSubTimer( ID=i )                                                                               ! Stopping the last SubTimer which has been started
  This%iSubTimer        =       i + 1                                                                           ! Incrementing the index of the last started SubTimer
  call This%StartSubTimer( ID=This%iSubTimer )                                                                 ! ... and starting it
End Procedure

! This procedure sets the time of a Timer object as the sum of the individual times stores in the SubTimers components.
! The resulting elapsed time of the Timer object is then exactly the summ of all the elapsed times on the SubTimer.
Module Procedure SetTimeFromSubTimers
  if ( size(This%SubTimers) /= 0 ) then
    call This%CPU%SetElapsedTime( sum( This%SubTimers(:)%CPU%GetElapsedTime() ) )
    call This%SYS%SetElapsedTime( sum( This%SubTimers(:)%SYS%GetElapsedTime() ) )
!     call This%DaT%SetElapsedTime( sum( This%SubTimers(:)%DaT%GetElapsedTime() )
  else
    call This%CPU%SetElapsedTime( Zero )
    call This%SYS%SetElapsedTime( Zero )
!     call This%DaT%SetElapsedTime( Zero )
  end if
End Procedure

Module Procedure IsSubTimerRunning
  Running   =   .False.
  if ( .Not. This%IsValidSubTimerIndex(ID) ) return                                                              ! Exiting the procedure is the selected SubTimer index is out of bounds
  Running   =   This%SubTimers(ID)%Running
End Procedure

Module Procedure IsValidSubTimerIndex
  IsValid   =   .True.
  if ( (ID > size(This%SubTimers)) .or. (ID <=0) ) IsValid = .False.
End Procedure

! ==============================================================================================================
!    PROCEDURES TO OUTPUT THE TIMER PROPERTIES
! ==============================================================================================================

Module Procedure OutputToString

  use String_Library      ,only:  Convert_To_String, Add_Line_To_String

  logical                                                               ::  CPUTime_
  logical                                                               ::  SystemTime_
  logical                                                               ::  SubTimers_
  logical                                                               ::  Remainder_
  integer                                                               ::  i
  integer                                                               ::  Length
!   real(8)                                                               ::  RemainderTime
!   real(8)                                                               ::  Percentage
!   character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  Line
  character(:)  ,allocatable                                            ::  TimeString
  character(*)                                              ,parameter  ::  RemainderName = "<Remainder>"

! ==============================================================================================================
!    SETTING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  CPUTime_    =   .False.
  SystemTime_ =   .True.
  SubTimers_  =   .True.
  Remainder_  =   .True.
  if ( present(CPUTime) )     CPUTime_    = CPUTime
  if ( present(SystemTime) )  SystemTime_ = SystemTime
  if ( present(SubTimers) )   SubTimers_  = SubTimers
  if ( present(Remainder) )   Remainder_  = Remainder
! ==============================================================================================================

! ==============================================================================================================
!    SETTING THE TIMING ASSOCIATED TO CURRENT TIMER
! ==============================================================================================================
  Length          =       This%GetNamesLength()
  if ( (size(This%SubTimers)>0) .and. SubTimers_ .and. Remainder_ ) Length = max(Length,len_trim(RemainderName))
  Line            =       Convert_To_String( This%Name, Len=Length )
  if (CPUTime_) then
  associate( Time => This%CPU )
    TimeString    =   Time%GetName() // ": " // Time%GetTimeInSeconds( Fmt=Time%GetFormat() ) // " => " // Time%GetTimeInDHMS()
    Line          =   Line // "   " // TimeString
  end associate
  end if
  if (SystemTime_) then
  associate( Time => This%SYS )
    TimeString    =   Time%GetName() // ": " // Time%GetTimeInSeconds( Fmt=Time%GetFormat() ) // " => " // Time%GetTimeInDHMS()
    Line          =   Line // "   " // TimeString
  end associate
  end if
  call Add_Line_To_String( Lines, Line )
! ==============================================================================================================

! ==============================================================================================================
!    SETTING THE TIMING ASSOCIATED TO THE SUBTIMERS, IF REQUIRED
! ==============================================================================================================
! In the '' procedure we need to call 'GetElapsedTime'  and not 'GetTime' so that the percentage is computed
! using the total time at the moment when the timer was stopped, not the current time.
  if (SubTimers_) then
    do i = 1,size(This%SubTimers)
    associate( SubTimer => This%SubTimers(i) )
      Line    =       "-> "   // Convert_To_String( SubTimer%Name, Len=Length )
      if (CPUTime_) then
      associate( TotTime => This%CPU, SubTime => SubTimer%CPU )
        TimeString    =   SubTime%GetTimeInSeconds( Fmt=TotTime%GetFormat() ) // " => " // SubTime%GetPercentageString(TotTime%GetElapsedTime(),Length=len(TotTime%GetTimeInDHMS()))
        Line          =   Line // "     "  // TimeString
      end associate
      end if
      if (SystemTime_) then
      associate( TotTime => This%SYS, SubTime => SubTimer%SYS )
        TimeString    =   SubTime%GetTimeInSeconds( Fmt=TotTime%GetFormat() ) // " => " // SubTime%GetPercentageString(TotTime%GetElapsedTime(),Length=len(TotTime%GetTimeInDHMS()))
        Line          =   Line // "     " // TimeString
      end associate
      end if
      call Add_Line_To_String( Lines, Line )
    end associate
    end do
! ==============================================================================================================

! ! ==============================================================================================================
! !    SETTING THE TIMING ASSOCIATED TO THE REMAINDER, IF REQUIRED
! ! ==============================================================================================================
!     if (Remainder_) then
!       Line    =       "-> "   // Convert_To_String( RemainderName, Len=Length )
!       if (CPUTime_) then
!       associate( TotTime => This%CPU, SubTimers => This%SubTimers(:)%CPU )
!         RemainderTime =   TotTime%GetElapsedTime() - sum(SubTimers%GetElapsedTime())
!         Percentage    =   100 * RemainderTime / TotTime%GetElapsedTime()
!         TimeString    =   Convert_To_String(RemainderTime,Fmt=TotTime%GetFormat() ) // "s"
!         TimeString    =   TimeString // " => "
!         String        =   Convert_To_String( Percentage, Fmt="(f6.2)" ) // "%"
!         String        =   Convert_To_String( String, Len=len(TotTime%GetTimeInDHMS()) )
!         TimeString    =   TimeString // String
!         Line          =   Line // "     "  // TimeString
!       end associate
!       end if
!       if (SystemTime_) then
!       associate( TotTime => This%SYS, SubTimers => This%SubTimers(:)%SYs )
!         RemainderTime =   TotTime%GetElapsedTime() - sum(SubTimers%GetElapsedTime())
!         Percentage    =   100 * RemainderTime / TotTime%GetElapsedTime()
!         TimeString    =   Convert_To_String(RemainderTime,Fmt=TotTime%GetFormat() ) // "s"
!         TimeString    =   TimeString // " => "
!         String        =   Convert_To_String( Percentage, Fmt="(f6.2)" ) // "%"
!         String        =   Convert_To_String( String, Len=len(TotTime%GetTimeInDHMS()) )
!         TimeString    =   TimeString // String
!         Line          =   Line // "     "  // TimeString
!       end associate
!       end if
!       call Add_Line_To_String( Lines, Line )
!     end if
! ! ==============================================================================================================
  end if

End Procedure

Module Procedure OutputToFile
  use ,intrinsic :: iso_fortran_env ,only:  Output_Unit
  integer                                                               ::  i
  character(:)  ,dimension(:)   ,allocatable                            ::  Lines
  call This%OutputToString( Lines, CPUTime, SystemTime, SubTimers )
  do i = 1,size(Lines)
    write(Unit,"(2x,a)") Lines(i)
  end do
End Procedure

Module Procedure OutputToLogger
  use Logger_Class                ,only:  Logger
  integer                                                               ::  i
  character(:)  ,dimension(:)   ,allocatable                            ::  Lines
  call This%OutputToString( Lines, CPUTime, SystemTime, SubTimers )
  do i = 1,size(Lines)
    call Logger%Write( Lines(i) )
  end do
End Procedure

End SubModule
