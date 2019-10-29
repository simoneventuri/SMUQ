Module TimerUtilities_Library

  implicit none

  private
  public  ::  GetCPUTime
!   public  ::  Get_SYS_Time
  public  ::  GetInitialSystemClockCount
  public  ::  GetSystemClockTime
  public  ::  Convert_Secondes_To_HMD

!   Interface             Get_SYS_Time
!     Module Procedure    Get_SYS_Time_Start
!     Module Procedure    Get_SYS_Time_Stop
!   End Interface

  Interface
    Module Function GetCPUTime() result(Time)
      real(8)                                                               ::  Time                    !< CPU time counter in seconds
    End Function
    Module Function GetInitialSystemClockCount() result(InitialClockCount)
      integer                                                               ::  InitialClockCount       !< Initial system time counter
    End Function
    Module Function GetSystemClockTime( InitialClockCount ) result(Time)
      integer                                               ,intent(in)     ::  InitialClockCount       !< System time counter
      real(8)                                                               ::  Time                    !< CPU time counter in seconds
    End Function
    Pure Module Subroutine ConvertSecondes( TimeInSeconds, Seconds, Minutes, Hours )
      real(8)                       ,intent(in)     ::  TimeInSeconds
      integer             ,optional ,intent(out)    ::  Seconds
      integer             ,optional ,intent(out)    ::  Minutes
      integer             ,optional ,intent(out)    ::  Hours
    End Subroutine
    Pure Module Function Convert_Secondes_To_HMD( TimeInSeconds ) result(String)
      real(8)                       ,intent(in)     ::  TimeInSeconds
      character(:)  ,allocatable                    ::  String
    End Function
  End Interface

End Module