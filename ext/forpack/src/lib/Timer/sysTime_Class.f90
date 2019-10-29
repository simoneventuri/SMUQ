Module sysTime_Class

  use AbstractTime_Class  ,only:  AbstractTime_Type

  implicit none

  private
  public  ::  sysTime_Type

  Type  ,extends(AbstractTime_Type)     ::  sysTime_Type
    private
    integer                             ::  InitialClockCount
  contains
    procedure           ,public         ::  Start_Timing    =>      StartSystemClockTiming
    procedure           ,public         ::  Stop_Timing     =>      StopSystemClockTiming
    procedure           ,public         ::  ResetTime       =>      ResetSystemClockTime
    procedure           ,public         ::  GetTime         =>      GetCurrentSystemClockTime
  End Type

  Interface
    Module Subroutine StartSystemClockTiming( This )
      class(sysTime_Type)                   ,intent(inout)  ::  This
    End Subroutine
    Module Subroutine StopSystemClockTiming( This )
      class(sysTime_Type)                   ,intent(inout)  ::  This
    End Subroutine
    Pure Elemental Module Subroutine  ResetSystemClockTime( This )
      class(sysTime_Type)                  ,intent(inout)  ::  This                                            !< Passed-object dummy argument
    End Subroutine
    Module Function GetCurrentSystemClockTime( This ) result(Time)
      class(sysTime_Type)                   ,intent(in)     ::  This                                            !< Passed-object dummy argument
      real(8)                                               ::  Time                                            !< Time in seconds
    End Function
  End Interface

End Module