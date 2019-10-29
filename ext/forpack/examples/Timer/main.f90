! cd /home/blopez/Codes/ForPack/build/forpack-1.0-Serial-Debug-ifort-17.1/Timer/bin; ./TimerExample.x

Program TimerExample

  use Logger_Class      ,only:  Logger
  use Timer_Library     ,only:  Timer_Type
  use Timing_Utilities  ,only:  DoSomething
  use iso_fortran_env

  implicit none

  integer(int64),parameter  ::    TotalIterations = 1e6 ! 1e6 gives a 10s run
  character(*)  ,parameter  ::    ProcName = 'TimerExample'
  logical       ,parameter  ::    i_Debug_Loc = .True.

  if (i_Debug_Loc) call Logger%Entering( ProcName )
!
!   if (i_Debug_Loc) call Logger%Write( "Calling SimpleExample" )
!   call SimpleExample( i_Debug=i_Debug_Loc )


  if (i_Debug_Loc) call Logger%Write( "Calling AdvancedExample" )
  call AdvancedExample( i_Debug=i_Debug_Loc )

  if (i_Debug_Loc) call Logger%Exiting()

  contains

Subroutine SimpleExample( i_Debug )
  logical                                     ,optional ,intent(in)     ::    i_Debug                         !< Debugging indicator
  logical                                                               ::    i_Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::    ProcName = 'SimpleExample'
  type(Timer_Type)                                                      ::    Timer

  i_Debug_Loc = .False.; if (present(i_Debug)) i_Debug_Loc = i_Debug
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  call Timer%Initialize( Name = "Temporal_Loop", Start = .True. )
!   call sleep(5)
  call DoSomething( 100, TotalIterations, i_Debug=i_Debug_Loc )
  call Timer%Stop()
  call Timer%Output()

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine

Subroutine AdvancedExample( i_Debug )

  logical                                     ,optional ,intent(in)     ::    i_Debug                         !< Debugging indicator

  logical                                                               ::    i_Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::    ProcName = 'AdvancedExample'
  type(Timer_Type)                                                      ::    Timer
  integer                                                               ::    i
  character(10) ,dimension(5)   ::  SubTimerNames = ["SubTimer-1","SubTimer-2","SubTimer-3","SubTimer-4","SubTimer-5"]
  integer       ,dimension(5)   ::  SubTimerTimes = [5,          15,          15,          20,          30]


  i_Debug_Loc = .False.; if (present(i_Debug)) i_Debug_Loc = i_Debug
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  call Timer%Initialize(   Name = "Timer with 5 SubTimers" )
  call Timer%AddSubTimer( Name = SubTimerNames(1) )
  call Timer%AddSubTimer( Name = SubTimerNames(2) )
  call Timer%AddSubTimer( Name = SubTimerNames(3) )
  call Timer%AddSubTimer( Name = SubTimerNames(4) )
  call Timer%AddSubTimer( Name = SubTimerNames(5) )

  call Logger%Write( "Timer%GetNumberSubTimers() = ", Timer%GetNumberSubTimers() )

! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%Start" )
  call Timer%Start()
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%NextSubTimer" )
  call Timer%NextSubTimer()
  call DoSomething( SubTimerTimes(1), TotalIterations, i_Debug=i_Debug_Loc )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetCPUTime()    = ", Timer%GetCPUTime()   , Fmt="f15.3" )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetSystemTime() = ", Timer%GetSystemTime(), Fmt="f15.3" )
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%NextSubTimer" )
  call Timer%NextSubTimer()
  call DoSomething( SubTimerTimes(2), TotalIterations, i_Debug=i_Debug_Loc )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetCPUTime()    = ", Timer%GetCPUTime()   , Fmt="f15.3" )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetSystemTime() = ", Timer%GetSystemTime(), Fmt="f15.3" )
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%NextSubTimer" )
  call Timer%NextSubTimer()
  call DoSomething( SubTimerTimes(3), TotalIterations, i_Debug=i_Debug_Loc )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetCPUTime()    = ", Timer%GetCPUTime()   , Fmt="f15.3" )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetSystemTime() = ", Timer%GetSystemTime(), Fmt="f15.3" )
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%NextSubTimer" )
  call Timer%NextSubTimer()
  call DoSomething( SubTimerTimes(4), TotalIterations, i_Debug=i_Debug_Loc )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetCPUTime()    = ", Timer%GetCPUTime()   , Fmt="f15.3" )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetSystemTime() = ", Timer%GetSystemTime(), Fmt="f15.3" )
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================
  if (i_Debug_Loc) call Logger%Write( "*********************************************" )
  if (i_Debug_Loc) call Logger%Write( "Timer%NextSubTimer" )
  call Timer%NextSubTimer()
  call DoSomething( SubTimerTimes(5), TotalIterations, i_Debug=i_Debug_Loc )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetCPUTime()    = ", Timer%GetCPUTime()   , Fmt="f15.3" )
  if (i_Debug_Loc) call Logger%Write( "Timer%GetSystemTime() = ", Timer%GetSystemTime(), Fmt="f15.3" )
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%IsRunning() = ", Timer%IsRunning()  )
!     do i = 1,Timer%GetNumberSubTimers()
!       call Logger%Write( "i = ", i, "Timer%IsSubTimerRunning(i) = ", Timer%IsSubTimerRunning(i) )
!     end do
!   end if
! ==================================================================================================


  call Timer%NextSubTimer()
  call DoSomething( 100-sum(SubTimerTimes), TotalIterations, i_Debug=i_Debug_Loc )

  call Timer%Stop()
  call Timer%Output()
!
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%GetElapsedTime()  = ", Timer%GetElapsedTime(),  Fmt="f15.3" )
!     call Logger%Write( "Timer%GetElapsedTime(1) = ", Timer%GetElapsedTime(1), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetElapsedTime(2) = ", Timer%GetElapsedTime(2), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetElapsedTime(3) = ", Timer%GetElapsedTime(3), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetElapsedTime(4) = ", Timer%GetElapsedTime(4), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetElapsedTime(5) = ", Timer%GetElapsedTime(5), Fmt="f15.3" )
!   end if
!
!   if (i_Debug_Loc) then
!     call Logger%Write( "Timer%GetCPUElapsedTime()  = ", Timer%GetCPUElapsedTime(),  Fmt="f15.3" )
!     call Logger%Write( "Timer%GetCPUElapsedTime(1) = ", Timer%GetCPUElapsedTime(1), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetCPUElapsedTime(2) = ", Timer%GetCPUElapsedTime(2), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetCPUElapsedTime(3) = ", Timer%GetCPUElapsedTime(3), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetCPUElapsedTime(4) = ", Timer%GetCPUElapsedTime(4), Fmt="f15.3" )
!     call Logger%Write( "Timer%GetCPUElapsedTime(5) = ", Timer%GetCPUElapsedTime(5), Fmt="f15.3" )
!   end if
!
!   if (i_Debug_Loc) then
!     call Logger%Write( Timer%GetName()            // ": ", Timer%GetCPUElapsedTime(),  Fmt="f15.3" )
!     call Logger%Write( "-> " // Timer%GetName(1) // ": ", Timer%GetCPUElapsedTime(1), Fmt="f15.3" )
!     call Logger%Write( "-> " // Timer%GetName(2) // ": ", Timer%GetCPUElapsedTime(2), Fmt="f15.3" )
!     call Logger%Write( "-> " // Timer%GetName(3) // ": ", Timer%GetCPUElapsedTime(3), Fmt="f15.3" )
!     call Logger%Write( "-> " // Timer%GetName(4) // ": ", Timer%GetCPUElapsedTime(4), Fmt="f15.3" )
!     call Logger%Write( "-> " // Timer%GetName(5) // ": ", Timer%GetCPUElapsedTime(5), Fmt="f15.3" )
!   end if

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine

End Program
