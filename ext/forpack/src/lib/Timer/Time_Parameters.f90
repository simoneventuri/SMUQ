Module Time_Parameters

  implicit none

  public
  real(8)   ,parameter      ,public ::  SecondsPerMinute  =   60.0_8          ! Number of seconds per minute
  real(8)   ,parameter      ,public ::  MinutesPerHour    =   60.0_8          ! Number of minutes per hour
  real(8)   ,parameter      ,public ::  HourPerDay        =   24.0_8          ! Number of hours per day
  real(8)   ,parameter      ,public ::  SecondsPerHour    =   SecondsPerMinute * MinutesPerHour         ! Number of seconds per hour
  real(8)   ,parameter      ,public ::  SecondsPerDay     =   SecondsPerHour * HourPerDay         ! Number of seconds per day

End Module