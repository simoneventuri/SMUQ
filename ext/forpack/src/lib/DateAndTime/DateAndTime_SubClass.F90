SubModule(DateAndTime_Class) DateAndTime_SubClass

  implicit none

  integer       ,parameter                      ::  iYear           =   1
  integer       ,parameter                      ::  iMonth          =   2
  integer       ,parameter                      ::  iDay            =   3
  integer       ,parameter                      ::  iMindtUTC       =   4
  integer       ,parameter                      ::  iHour           =   5
  integer       ,parameter                      ::  iMinute         =   6
  integer       ,parameter                      ::  iSecond         =   7
  integer       ,parameter                      ::  iMilliSecond    =   8

  contains

! This procedure get the data associated to the Date and time.
! These data are obtained using the "Date_And_Time" intrinsic. The "Value" optional output argument returns
! a 8-element integer array whose each element corresponds to:
!  (1) the year,
!  (2) the month,
!  (3) the day of the month,
!  (4) the time difference with UTC in minutes,
!  (5) the hour of the day,
!  (6) the minute of the hour,
!  (7) the Second of the minute and,
!  (8) the milliSecond of the Second
Module Procedure InitializeDateAndTime
  integer                                                               ::  i                               !< Index of elements in the Values outputed from the "Date_And_Time" command (1 to 8)
  character(1)                                                          ::  OneChar
  call Date_And_Time( Values = This%Integer_Values )
  do i = 1,size(This%Integer_Values)
    This%Character_Values(i)    =   Convert_to_String( This%Integer_Values(i) )
!     OneChar                     =   This%Character_Values(i)(1:1)
    if ( len_trim(This%Character_Values(i)) == 1 ) then
      OneChar                   =   adjustl( trim(This%Character_Values(i)) )
      This%Character_Values(i)  = '0' // OneChar
    end if
!     if ( len_trim(This%Character_Values(i)) == 1 ) This%Character_Values(i) = '0' // This%Character_Values(i)
  end do
  This%Year             =   trim(This%Character_Values(iYear))
  This%Month            =   trim(This%Character_Values(iMonth))
  This%Day              =   trim(This%Character_Values(iDay))
  This%MindtUTC         =   trim(This%Character_Values(iMindtUTC))
  This%Hour             =   trim(This%Character_Values(iHour))
  This%Minute           =   trim(This%Character_Values(iMinute))
  This%Second           =   trim(This%Character_Values(iSecond))
  This%MilliSecond      =   trim(This%Character_Values(iMilliSecond))
End Procedure

Module Procedure Get_Year
  Year          =   This%Year
End Procedure

Module Procedure Get_Month
  Month         =   This%Month
End Procedure

Module Procedure Get_Day
  Day           =   This%Day
End Procedure

Module Procedure Get_MindtUTC
  MindtUTC      =   This%MindtUTC
End Procedure

Module Procedure Get_Hour
  Hour          =   This%Hour
End Procedure

Module Procedure Get_Minute
  Minute        =   This%Minute
End Procedure

Module Procedure Get_Second
  Second        =   This%Second
End Procedure

Module Procedure Get_MilliSecond
  MilliSecond   =   This%MilliSecond
End Procedure

Module Procedure Get_DayMonthYear
  character(:)  ,allocatable                    ::  Sep
  Sep   =   ""
  if ( present(Separator) ) Sep = Separator
  DayMonthYear      =   This%Day
  DayMonthYear      =   trim(DayMonthYear) // Sep
  DayMonthYear      =   trim(DayMonthYear) // This%Month
  DayMonthYear      =   trim(DayMonthYear) // Sep
  DayMonthYear      =   trim(DayMonthYear) // This%Year
End Procedure

Module Procedure Get_YearMonthDay
  character(:)  ,allocatable                    ::  Sep
  Sep   =   ""
  if ( present(Separator) ) Sep = Separator
  YearMonthDay      =   This%Year
  YearMonthDay      =   trim(YearMonthDay) // Sep
  YearMonthDay      =   trim(YearMonthDay) // This%Month
  YearMonthDay      =   trim(YearMonthDay) // Sep
  YearMonthDay      =   trim(YearMonthDay) // This%Day
End Procedure

Module Procedure Get_HourMinuteSecond
  character(:)  ,allocatable                    ::  Sep
  Sep   =   ""
  if ( present(Separator) ) Sep = Separator
  HourMinuteSecond  =   This%Hour
  HourMinuteSecond  =   trim(HourMinuteSecond) // Sep
  HourMinuteSecond  =   trim(HourMinuteSecond) // This%Minute
  HourMinuteSecond  =   trim(HourMinuteSecond) // Sep
  HourMinuteSecond  =   trim(HourMinuteSecond) // This%Second
End Procedure

Function Convert_To_String( Num ) result(Str)
  integer                                               ,intent(in)     ::  Num                             !< Number to be converted into a string
  character(:)  ,allocatable                                            ::  Str                             !< Character corresponding to the input number
  character(100)                                                        ::  Str_Loc                         ! Local character string required to store the number of a very large string before allocation of the output variable
  write(Str_Loc,"(i0)") Num                                                                                     ! Interger-to-string conversion
  Str   =   trim(adjustl(Str_Loc))                                                                          ! Storing the string in the output variable with the right number of characters
End Function

End SubModule