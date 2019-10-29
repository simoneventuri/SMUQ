Module AbstractTime_Class

  implicit none

  private
  public  ::  AbstractTime_Type

  Type  ,abstract                       ::  AbstractTime_Type
    character(:)        ,allocatable    ::  Name                                                            !< Name of current time
    real(8)                             ::  ElapsedTime  =   0.0_8                                          !< Elapsed time Value [s] (Variable only set oncel the timer is stopped)
  contains
    procedure           ,public                 ::  Initialize      =>      InitializeTime                 ! Initializes the Time object
    procedure           ,public                 ::  Output          =>      Output_Time                     ! Outputs the Time object
    procedure           ,public                 ::  SetElapsedTime
    procedure           ,public                 ::  GetElapsedTime
    procedure           ,public                 ::  GetName
    procedure           ,public                 ::  GetTimeInSeconds
    procedure           ,public                 ::  GetTimeInDHMS
    procedure           ,public                 ::  GetPercentageString
    procedure           ,public                 ::  GetFormat
    procedure(Start_Timing) ,public  ,deferred  ::  Start_Timing                                            ! Starts the timing
    procedure(Stop_Timing)  ,public  ,deferred  ::  Stop_Timing                                             ! Stops the timing
    procedure(ResetTime)    ,public  ,deferred  ::  ResetTime                                              ! Resets the Time object
    procedure(GetTime)      ,public  ,deferred  ::  GetTime                                                 ! Gets the current timing
  End Type

  Abstract Interface
    Subroutine Start_Timing( This )
      import AbstractTime_Type
      class(AbstractTime_Type)     ,intent(inout)  ::  This
    End Subroutine
    Subroutine Stop_Timing( This )
      import AbstractTime_Type
      class(AbstractTime_Type)     ,intent(inout)  ::  This
    End Subroutine
    Pure Elemental Subroutine ResetTime( This )
      import AbstractTime_Type
      class(AbstractTime_Type)     ,intent(inout)  ::  This
    End Subroutine
    Function GetTime( This ) result(Time)
      import AbstractTime_Type
      class(AbstractTime_Type)     ,intent(in)     ::  This
      real(8)                                      ::  Time
    End Function
  End Interface

  Interface
    Pure Module Subroutine  InitializeTime( This, Name )
      class(AbstractTime_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Time object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Timer name
    End Subroutine
   Module Subroutine  Output_Time( This, Unit )
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      integer                                               ,intent(in)     ::  Unit                            !< File unit where the time data need to be outputed
    End Subroutine
    Pure Module Function GetName( This, Length ) result(Name)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      integer                                     ,optional ,intent(in)     ::  Length
      character(:)  ,allocatable                                            ::  Name
    End Function
    Pure Elemental Module Function GetElapsedTime( This ) result(ElapsedTime)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      real(8)                                                               ::  ElapsedTime
    End Function
    Pure Elemental Module Subroutine  SetElapsedTime( This, ElapsedTime )
      class(AbstractTime_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Time object
      real(8)                                               ,intent(in)     ::  ElapsedTime
    End Subroutine
    Module Function GetTimeInDHMS( This ) result(Str)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      character(:)  ,allocatable                                            ::  Str
    End Function
    Module Function GetPercentageString( This, Total, Length ) result(Str)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      real(8)                                               ,intent(in)     ::  Total                           !< Total time
      integer                                     ,optional ,intent(in)     ::  Length
      character(:)  ,allocatable                                            ::  Str
    End Function
    Module Function GetTimeInSeconds( This, Fmt ) result(Str)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      character(*)                                ,optional ,intent(in)     ::  Fmt
      character(:)  ,allocatable                                            ::  Str
    End Function
    Pure Module Function GetFormat( This ) result(Fmt)
      class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
      character(:)  ,allocatable                                            ::  Fmt
    End Function
  End Interface

End Module