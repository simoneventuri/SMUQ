SubModule(cpuTime_Class) cpuTime_SubClass

  use TimerUtilities_Library         ,only:  GetCPUTime

  implicit none

  contains

Module Procedure Start_CPU_Timing
  This%t1       =       GetCPUTime()
End Procedure

Module Procedure Stop_CPU_Timing
  This%t2           =       GetCPUTime()
  This%ElapsedTime  =       This%ElapsedTime + ( This%t2 - This%t1 )
End Procedure

Module Procedure Reset_CPU_Time
  This%t1           =   0.0_8                                                                                   ! Initializing the time start time
  This%t2           =   0.0_8                                                                                   ! Initializing the time elapsed time
  This%ElapsedTime  =   0.0_8
End Procedure

Module Procedure Get_CPU_Timing
  Time          =       GetCPUTime() - This%t1
End Procedure

End SubModule