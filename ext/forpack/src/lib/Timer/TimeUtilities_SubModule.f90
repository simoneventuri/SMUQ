SubModule(TimerUtilities_Library) TimerUtilities_SubModule

  implicit none

  Interface           Get_Exponent
    Module Procedure  Get_Exponent_Integer
    Module Procedure  Get_Exponent_Real8
  End Interface

  contains

Module Procedure GetCPUTime
  call cpu_Time(Time)
End Procedure

Module Procedure GetInitialSystemClockCount
  call system_clock( count=InitialClockCount )
End Procedure

Module Procedure GetSystemClockTime
  integer                                                               ::  CurrentClockCount
  integer                                                               ::  ClockCountRate
  integer                                                               ::  ClockCountMax
  integer                                                               ::  NClockCount
  call system_clock( count=CurrentClockCount, Count_Rate=ClockCountRate, Count_Max=ClockCountMax )
  NClockCount =   CurrentClockCount - InitialClockCount
  if ( CurrentClockCount < InitialClockCount ) NClockCount = NClockCount + ClockCountMax
  Time        =   real(NClockCount) / ClockCountRate
End Procedure

Pure Module Subroutine ConvertSecondes( TimeInSeconds, Seconds, Minutes, Hours )
  use Time_Parameters     ,only:  SecondsPerMinute, SecondsPerHour, SecondsPerDay
  real(8)                       ,intent(in)     ::  TimeInSeconds
  integer             ,optional ,intent(out)    ::  Seconds
  integer             ,optional ,intent(out)    ::  Minutes
  integer             ,optional ,intent(out)    ::  Hours
  integer                                       ::  Seconds_
  integer                                       ::  Minutes_
  integer                                       ::  Hours_
  if      ( TimeInSeconds < SecondsPerMinute ) then
    Hours_        =       0
    Minutes_      =       0
    Seconds_      =       int(TimeInSeconds)
  else if ( TimeInSeconds < SecondsPerHour ) then
    Hours_        =       0
    Minutes_      =       floor( TimeInSeconds/SecondsPerMinute )
    Seconds_      =       floor( mod( TimeInSeconds, SecondsPerMinute ) )
  else if ( TimeInSeconds < SecondsPerDay ) then
    Hours_        =       floor( TimeInSeconds / SecondsPerHour )
    Minutes_      =       floor( mod(TimeInSeconds,SecondsPerHour) / SecondsPerMinute)
    Seconds_      =       floor(mod( mod(TimeInSeconds,SecondsPerHour),SecondsPerMinute) )
  else
    Hours_        =       floor( TimeInSeconds / SecondsPerHour )
    Minutes_      =       floor( mod(TimeInSeconds,SecondsPerHour) / SecondsPerMinute)
    Seconds_      =       floor( mod( mod(TimeInSeconds,SecondsPerHour),SecondsPerMinute) )
  end if
  if ( present( Hours   ) ) Hours   = Hours_
  if ( present( Minutes ) ) Minutes = Minutes_
  if ( present( Seconds ) ) Seconds = Seconds_
End Subroutine

Pure Module Function Convert_Secondes_To_HMD( TimeInSeconds ) result(String)
  use String_Library      ,only:  Convert_To_String, ReplaceCharacter
  real(8)                       ,intent(in)     ::  TimeInSeconds
  character(:)  ,allocatable                    ::  String
  integer                                       ::  Seconds, Minutes, Hours
  character(:)  ,allocatable                    ::  TimeFormat
  call ConvertSecondes( TimeInSeconds, Seconds, Minutes, Hours )
  TimeFormat    =     GetTimeFormat( Seconds, Minutes, Hours )
  String        =     Convert_To_String( [Hours,Minutes,Seconds], Fmt=TimeFormat )
  String        =     ReplaceCharacter( String, " ", "0" )
End Function

Pure Function GetTimeFormat( Seconds, Minutes, Hours ) result(TimeFormat)
  use String_Library      ,only:  Convert_To_String
  integer             ,optional ,intent(in)     ::  Seconds
  integer             ,optional ,intent(in)     ::  Minutes
  integer             ,optional ,intent(in)     ::  Hours
  character(:)  ,allocatable                    ::  TimeFormat
  character(:)  ,allocatable                    ::  SecondsFormat, MinutesFormat, HoursFormat
  integer                                       ::  NumberOfDigits
! Setting the format for seconds
  NumberOfDigits  =     2
  if ( present(Seconds) ) NumberOfDigits = max(NumberOfDigits,Get_Exponent(Seconds)+1)
  SecondsFormat   =     "i" // Convert_To_String( NumberOfDigits ) // ",'s'"
! Setting the format for minutes
  NumberOfDigits  =     2
  if ( present(Minutes) ) NumberOfDigits = max(NumberOfDigits,Get_Exponent(Minutes)+1)
  MinutesFormat   =     "i" // Convert_To_String( NumberOfDigits ) // ",'m'"
! Setting the format for hours
  NumberOfDigits  =     2
  if ( present(Hours) ) NumberOfDigits = max(NumberOfDigits,Get_Exponent(Hours)+1)
  HoursFormat     =     "i" // Convert_To_String( NumberOfDigits ) // ",'h'"
! Setting the time format
  TimeFormat      =     "(" // HoursFormat // "," // MinutesFormat // "," // SecondsFormat // ")"
End Function

! Module Function GetStringTimeInSeconds( Time, Fmt ) result(Str)
!   use String_Library      ,only:  Convert_To_String
!   class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
!   character(*)                                ,optional ,intent(in)     ::  Fmt
!   character(:)  ,allocatable                                            ::  Str
!   character(*)                                              ,parameter  ::  Fmt_Def = "(f15.3)"
!   character(:)  ,allocatable                                            ::  Fmt_Loc
!   Fmt_Loc       =       Fmt_Def
!   if ( present(Fmt) ) Fmt_Loc = Fmt
!   Str           =       Convert_To_String( Time, Fmt=Fmt_Loc ) // "s"
! End Function


Pure Elemental Function Get_Exponent_Real8(Number) result(Expo)
  real(8)                                               ,intent(in)     ::  Number
  integer                                                               ::  Expo
  if ( Number /= 0.0_8 ) then
    Expo        =       floor(log10(abs(Number)))
  else
    Expo        =       0
  end if
End Function

Pure Elemental Function Get_Exponent_Integer(Number) result(Expo)
  integer                                               ,intent(in)     ::  Number
  integer                                                               ::  Expo
  real(8)                                                               ::  Number_
  Number_   =   real( Number, kind=8)
  Expo      =   Get_Exponent(Number_)
End Function

End SubModule