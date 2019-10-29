Module Timer_Library

  use Time_Parameters
  use TimerUtilities_Library  ,only:  GetCPUTime, GetInitialSystemClockCount, GetSystemClockTime, Convert_Secondes_To_HMD
  use AbstractTime_Class      ,only:  AbstractTime_Type
  use cpuTime_Class           ,only:  cpuTime_Type
  use sysTime_Class           ,only:  sysTime_Type
  use Timer_Class             ,only:  Timer_Type
  use IterStat_Class          ,only:  IterStat_Type

  implicit none

  private
  public  ::  GetCPUTime, GetInitialSystemClockCount, GetSystemClockTime, Convert_Secondes_To_HMD
  public  ::  AbstractTime_Type
  public  ::  cpuTime_Type
  public  ::  sysTime_Type
  public  ::  Timer_Type
  public  ::  IterStat_Type

End Module