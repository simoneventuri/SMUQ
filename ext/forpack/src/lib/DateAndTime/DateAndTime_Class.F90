Module DateAndTime_Class

  implicit none

  private
  public  ::  DateAndTime_Type

  Type                                          ::  DateAndTime_Type
    private
    integer             ,dimension(8)           ::  Integer_Values                                          !< Integer values given by the "Date_And_Time" intrinsic command
    character(100)      ,dimension(8)           ::  Character_Values                                        !< Character values given by the "Date_And_Time" intrinsic command after conversion
    character(:)        ,allocatable            ::  Year                                                    !<
    character(:)        ,allocatable            ::  Month                                                   !<
    character(:)        ,allocatable            ::  Day                                                     !<
    character(:)        ,allocatable            ::  MindtUTC                                                !<
    character(:)        ,allocatable            ::  Hour                                                    !<
    character(:)        ,allocatable            ::  Minute                                                  !<
    character(:)        ,allocatable            ::  Second                                                 !<
    character(:)        ,allocatable            ::  MilliSecond                                            !<
  contains
    private
    procedure ,public   ::  Initialize  =>  InitializeDateAndTime
    procedure ,public   ::  Get_Year                                                !< Gets the Year
    procedure ,public   ::  Get_Month                                               !< Gets the Month
    procedure ,public   ::  Get_Day                                                 !< Gets the Day
    procedure ,public   ::  Get_MindtUTC                                            !< Gets the MindtUTC
    procedure ,public   ::  Get_Hour                                                !< Gets the Hour
    procedure ,public   ::  Get_Minute                                              !< Gets the Minute
    procedure ,public   ::  Get_Second                                              !< Gets the Second
    procedure ,public   ::  Get_MilliSecond                                         !< Gets the MilliSecond
    procedure ,public   ::  Get_DayMonthYear                                        !< Gets the Day/Month/Year
    procedure ,public   ::  Get_YearMonthDay                                        !< Gets the
    procedure ,public   ::  Get_HourMinuteSecond                                    !< Gets the Hour/Minute/Second
  End Type

  Interface
    Module Subroutine InitializeDateAndTime( This )
      class(DateAndTime_Type)       ,intent(out)    ::  This                            !< Passed-object dummy argument
    End Subroutine
    Pure Module Function Get_Year( This ) result(Year)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Year
    End Function
    Pure Module Function Get_Month( This ) result(Month)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Month
    End Function
    Pure Module Function Get_Day( This ) result(Day)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Day
    End Function
    Pure Module Function Get_MindtUTC( This ) result(MindtUTC)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  MindtUTC
    End Function
    Pure Module Function Get_Hour( This ) result(Hour)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Hour
    End Function
    Pure Module Function Get_Minute( This ) result(Minute)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Minute
    End Function
    Pure Module Function Get_Second( This ) result(Second)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  Second
    End Function
    Pure Module Function Get_MilliSecond( This ) result(MilliSecond)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                    ::  MilliSecond
    End Function
    Pure Module Function Get_DayMonthYear( This, Separator ) result(DayMonthYear)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)        ,optional ,intent(in)     ::  Separator
      character(:)  ,allocatable                    ::  DayMonthYear
    End Function
    Pure Module Function Get_YearMonthDay( This, Separator ) result(YearMonthDay)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)        ,optional ,intent(in)     ::  Separator
      character(:)  ,allocatable                    ::  YearMonthDay
    End Function
    Pure Module Function Get_HourMinuteSecond( This, Separator ) result(HourMinuteSecond)
      class(DateAndTime_Type)       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)        ,optional ,intent(in)     ::  Separator
      character(:)  ,allocatable                    ::  HourMinuteSecond
    End Function
  End Interface

End Module