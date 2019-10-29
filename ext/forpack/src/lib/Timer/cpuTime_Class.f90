Module cpuTime_Class

  use AbstractTime_Class  ,only:  AbstractTime_Type

  implicit none

  private
  public  ::  cpuTime_Type

  Type  ,extends(AbstractTime_Type)     ::  cpuTime_Type
    real(8)                             ::  t1  =   0.0_8                                                   !< Value of start time [s]
    real(8)                             ::  t2  =   0.0_8                                                   !< Value of end time [s]
  contains
    procedure           ,public         ::  Start_Timing  =>  Start_CPU_Timing
    procedure           ,public         ::  Stop_Timing   =>  Stop_CPU_Timing
    procedure           ,public         ::  ResetTime     =>  Reset_CPU_Time
    procedure           ,public         ::  GetTime       =>  Get_CPU_Timing
  End Type

  Interface
    Module Subroutine Start_CPU_Timing( This )
      class(cpuTime_Type)                   ,intent(inout)  ::  This                                            !< Passed-object dummy argument
    End Subroutine
    Module Subroutine Stop_CPU_Timing( This )
      class(cpuTime_Type)                   ,intent(inout)  ::  This                                            !< Passed-object dummy argument
    End Subroutine
    Pure Elemental Module Subroutine  Reset_CPU_Time( This )
      class(cpuTime_Type)                   ,intent(inout)  ::  This                                            !< Passed-object dummy argument
    End Subroutine
    Module Function Get_CPU_Timing( This ) result(Time)
      class(cpuTime_Type)                   ,intent(in)     ::  This                                            !< Passed-object dummy argument
      real(8)                                               ::  Time                                            !< Time in seconds
    End Function
  End Interface

End Module