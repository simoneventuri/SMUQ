SubModule(sysTime_Class) sysTime_SubClass

  implicit none

  contains

Module Procedure StartSystemClockTiming
  use TimerUtilities_Library       ,only:  GetInitialSystemClockCount
  This%InitialClockCount    =   GetInitialSystemClockCount()
End Procedure

Module Procedure StopSystemClockTiming
  use TimerUtilities_Library       ,only:  GetSystemClockTime
  real(8)                                               ::  dt
  dt                =       GetSystemClockTime( This%InitialClockCount )
  This%ElapsedTime  =       This%ElapsedTime + dt
End Procedure

Module Procedure ResetSystemClockTime
  This%ElapsedTime  =   0.0_8
End Procedure

Module Procedure GetCurrentSystemClockTime
  use TimerUtilities_Library       ,only:  GetSystemClockTime
  real(8)                                               ::  dt
  dt                =       GetSystemClockTime( This%InitialClockCount )
  Time              =       This%ElapsedTime + dt
End Procedure

End SubModule