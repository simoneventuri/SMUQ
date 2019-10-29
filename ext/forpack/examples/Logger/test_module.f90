Module Test_Module

  use Logger_Class      ,only:  Logger_Type

  implicit none

  logical ,parameter  ::  Debug = .False.

  contains

Subroutine PrintLoggerState( Logger )
  type(Logger_Type)         ,intent(inout)  ::    Logger
  call Logger%Write( "Logger properties:" )
  call Logger%Write( "Logger%On()                             = ", Logger%On()  )
  call Logger%Write( "Logger%GetLogLevel()                    = ", Logger%GetLogLevel()  )
  call Logger%Write( "-> Logger%iLogLev                       = ", Logger%iLogLev )
  call Logger%Write( "-> associated(Logger%CurrentItem)       = ", associated(Logger%CurrentItem) )
  if ( associated(Logger%CurrentItem) ) then
    call Logger%Write( "-> Logger%CurrentItem%Name              = ", Logger%CurrentItem%Name             )
    call Logger%Write( "-> Logger%CurrentItem%Index             = ", Logger%CurrentItem%Index            )
    call Logger%Write( "-> Logger%CurrentItem%Indentation       = ", Logger%CurrentItem%Indentation      )
    call Logger%Write( "-> Logger%CurrentItem%LogLevel          = ", Logger%CurrentItem%LogLevel         )
    call Logger%Write( "-> Logger%CurrentItem%SavedLogLevel     = ", Logger%CurrentItem%SavedLogLevel    )
    call Logger%Write( "-> Logger%CurrentItem%MsgLogLevel       = ", Logger%CurrentItem%MsgLogLevel      )
    call Logger%Write( "-> Logger%CurrentItem%SavedMsgLogLevel  = ", Logger%CurrentItem%SavedMsgLogLevel )
  end if

  write(*,"(a,g0)") "Logger properties:"
  write(*,"(a,g0)") "Logger%On()                             = ", Logger%On()
  write(*,"(a,g0)") "-> Logger%GetPrefix()                   = ", Logger%GetPrefix()
  write(*,"(a,g0)") "Logger%GetLogLevel()                    = ", Logger%GetLogLevel()
  write(*,"(a,g0)") "-> Logger%iLogLev                       = ", Logger%iLogLev
  write(*,"(a,g0)") "-> associated(Logger%CurrentItem)       = ", associated(Logger%CurrentItem)
  if ( associated(Logger%CurrentItem) ) then
    write(*,"(a,g0)") "-> Logger%CurrentItem%Name              = ", Logger%CurrentItem%Name
    write(*,"(a,g0)") "-> Logger%CurrentItem%Index             = ", Logger%CurrentItem%Index
    write(*,"(a,g0)") "-> Logger%CurrentItem%Indentation       = ", Logger%CurrentItem%Indentation
    write(*,"(a,g0)") "-> Logger%CurrentItem%LogLevel          = ", Logger%CurrentItem%LogLevel
    write(*,"(a,g0)") "-> Logger%CurrentItem%SavedLogLevel     = ", Logger%CurrentItem%SavedLogLevel
    write(*,"(a,g0)") "-> Logger%CurrentItem%MsgLogLevel       = ", Logger%CurrentItem%MsgLogLevel
    write(*,"(a,g0)") "-> Logger%CurrentItem%SavedMsgLogLevel  = ", Logger%CurrentItem%SavedMsgLogLevel
  end if
End Subroutine

Subroutine Test_WriteLogger
  type(Logger_Type)         ::    Logger
  call Logger%Entering( 'Test_WriteLogger' )
  call Logger%Write( "This procedure tests the calls to set of 'Logger%Write' procedures" )
  call Logger%Write( "using different configuration. The calls differ by" )
  call Logger%Write( "* the number of arguments" )
  call Logger%Write( "* the type of the arguments" )
  call Logger%Write( "* the rank of the arguments" )


  call Logger%Write( "==> Write_1xV0" )
  call Logger%Write( .True.                 )
  call Logger%Write( huge(int(1,kind=1))    )
  call Logger%Write( huge(int(1,kind=2))    )
  call Logger%Write( huge(int(1,kind=4))    )
  call Logger%Write( huge(int(1,kind=8))    )
  call Logger%Write( huge(real(1,kind=4))   )
  call Logger%Write( huge(real(1,kind=8))   )
  call Logger%Write( huge(real(1,kind=16))  )
  call Logger%Write( 'character'  )

  call Logger%Write( "==> Write_2xV0 <==" )
  call Logger%Write( ".True.                = ", .True.                 )
  call Logger%Write( "huge(int(1,kind=1))   = ", huge(int(1,kind=1))    )
  call Logger%Write( "huge(int(1,kind=2))   = ", huge(int(1,kind=2))    )
  call Logger%Write( "huge(int(1,kind=4))   = ", huge(int(1,kind=4))    )
  call Logger%Write( "huge(int(1,kind=8))   = ", huge(int(1,kind=8))    )
  call Logger%Write( "huge(real(1,kind=4))  = ", huge(real(1,kind=4))   )
  call Logger%Write( "huge(real(1,kind=8))  = ", huge(real(1,kind=8))   )
  call Logger%Write( "huge(real(1,kind=16)) = ", huge(real(1,kind=16))  )
  call Logger%Write( 'character             = ', 'Something'  )

  call Logger%Write( "==> Write_3xV0 <==" )
  call Logger%Write( .True., .False., .True.)
  call Logger%Write( 1, 2, 3 )
  call Logger%Write( 1.0, 2.0, 3.0 )
  call Logger%Write( "a", "b", "c")

  call Logger%Write( "==> Write_1xV0_1xV1 <==" )
  call Logger%Write( "Vector = ", [.False.,.True.])
  call Logger%Write( "Vector = ", [1,2] )
  call Logger%Write( "Vector = ", [1.0,2.0] )
  call Logger%Write( "Vector = ", ["a","b"] )

  call Logger%Write( "==> Write_1xV0_1xV2 <==" )
  call Logger%Write( "Matrix(2,2) = ", reshape([.False.,.True.,.False.,.True.],[2,2]) )
  call Logger%Write( "Matrix(2,2) = ", reshape([1,2,3,4],[2,2]) )
  call Logger%Write( "Matrix(2,2) = ", reshape([1.0,2.0,3.0,4.0] ,[2,2]), Fr="'i=',f3.1:" )
  call Logger%Write( "Matrix(2,2) = ", reshape(["a","b","c","d"] ,[2,2]) )

  call Logger%Write( "Matrix(2,5) = ", reshape([1,2,3,4,5,6,7,8,9,10],[2,5]) )
  call Logger%Write( "Matrix(5,2) = ", reshape([1,2,3,4,5,6,7,8,9,10],[5,2]) )


!   [Test_WriteLogger]: Matrix(5,2) = / 1   6  \
!   [Test_WriteLogger]:               | 2   7  |
!   [Test_WriteLogger]:               | 3   8  |
!   [Test_WriteLogger]:               | 4   9  |
!   [Test_WriteLogger]:               \ 5   10 /

  call Logger%Exiting()
End Subroutine

Subroutine Test_WriteOptions
  type(Logger_Type)         ::    Logger
  call Logger%Entering( 'Test_WriteOptions' )
  call Logger%Write( "This procedure tests the write options." )
  call Logger%Write( "Use of the 'Error' optional argument"     , Error      = .True. )
  call Logger%Write( "Use of the 'Warning' optional argument"   , Warning    = .True. )
  call Logger%Write( "Use of the 'Info' optional argument"      , Info       = .True. )
  call Logger%Write( "Use of the 'Debug' optional argument"     , Debug      = .True. )
  call Logger%Write( "Use of the 'HeavyDebug' optional argument", HeavyDebug = .True. )
  call Logger%Exiting()
End Subroutine

Subroutine Test_NotInitialized
  type(Logger_Type)         ::    Logger
  call Logger%Write( "This procedure tests calls to the Logger when it has not been initialized" )
  call Logger%Write( "or when the procedure 'Logger%Entering' has not been called." )
  call Logger%Write( "The current message is printed before the Logger is initialized" )
  call Logger%Write( "and so it should be printed to the screen and without any prefix." )
  call Logger%Write( "Now, if we call a procedure which calls the Logger%Entering/Exiting," )
  call Logger%Write( "a prefix with the name of the procedure should appears" )
  if (Debug) call PrintLoggerState(Logger)
  call Logger%Write( "Before: 'call MyProc'" )
  call MyProc()
  call Logger%Write( "After: 'call MyProc'" )
  call Logger%Write( "We are now back to the calling procedure and so there should be be no prefix." )
  call Logger%Write( "Exiting the test.." )
  contains
  Subroutine MyProc
    call Logger%Entering( 'MyProc' )
    call Logger%Write( "After: 'call Logger%Entering( 'MyProc' )'" )
    call Logger%Write( "Since we have called the 'Logger%Entering' procedure, the log messages should" )
    call Logger%Write( "be indented and be prefixed with the name of the procedure" )
    call Logger%Write( "Before: 'call Logger%Exiting()'" )
    call Logger%Exiting()
  End Subroutine
End Subroutine

Subroutine Test_DebugMode
  type(Logger_Type)         ::    Logger
  call Logger%Entering( 'Test_DebugMode' )
  call Logger%Write( "This procedure tests the 'DebugMode' option." )
  call Logger%Write( "The call to 'Logger%StartDebugMode' change to Debug the default log level of all" )
  call Logger%Write( "the log message printed using the 'Logger%Write' procedures." )
  call Logger%Write( "This change is effective for all the log message printed in current procedure" )
  call Logger%Write( "until the 'Logger%StopDebugMode' procedure is called." )
  call Logger%Write( "The procedure 'MyProc' defined a Debug section delimited by the call to" )
  call Logger%Write( "the procedures 'Logger%StartDebugMode' and 'Logger%StopDebugMode'." )
  call Logger%Write( "This 'MyProc' procedure prints the 3 lines made of the '*' character:" )
  call Logger%Write( "- the 1st line is outide the debug section" )
  call Logger%Write( "- the 2nd line is inside the debug section" )
  call Logger%Write( "- the 3rd line is outide the debug section" )
  call Logger%Write( "Before: 'call MyProc()' (If absent, the default log level for log messages is LogLevel=3)" )
  call MyProc()
  call Logger%Write( "Before: 'call MyProc(LogLevel=3)'" )
  call MyProc(LogLevel=3)
  call Logger%Write( "Before: 'call MyProc(LogLevel=4)'" )
  call MyProc(LogLevel=4)
  call Logger%Exiting()
  contains
  Subroutine MyProc(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc', LogLevel )
    call Logger%Write( "Line 1: ************** Logger%On() = ", Logger%On() )
    call Logger%StartDebugMode()
    call Logger%Write( "Line 2: ************** Logger%On() = ", Logger%On() )
    call Logger%StopDebugMode()
    call Logger%Write( "Line 3: ************** Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
End Subroutine


Subroutine Test_ProcToLog
  type(Logger_Type)         ::    Logger
  call Logger%Entering( 'Test_ProcToLog' )
  call Logger%Write( "This procedure tests the ability to force the print of log messages from the" )
  call Logger%Write( "name of the procedures containing the log message to forced." )
  call Logger%Write( "The name of the procedures for whcih log messages have to be forced can be" )
  call Logger%Write( "added to the Logger by calling 'Logger%AddProcedureToLog' procedure." )
  call Logger%Write( "This procedure takes only one input argument which can be either a scalar character" )
  call Logger%Write( "or a vector of characters. For example, by calling:" )
  call Logger%Write( "- Logger%AddProcedureToLog( ['MyProc1','MyProc3'] )" )
  call Logger%AddProcedureToLog( ['MyProc1','MyProc3'] )
  call Logger%Write( "- Logger%AddProcedureToLog( 'MyProc5' )" )
  call Logger%AddProcedureToLog( 'MyProc5' )
  call Logger%Write( "the logs from 'MyProc1', 'MyProc3' and 'MyProc5' will be forced," )
  call Logger%Write( "whatever the log levels of the log message printed in these procedures." )
  call Logger%Write( "Calling 'MyProc1(LogLevel=0)'" )
  call MyProc1(LogLevel=0)
  call Logger%Write( "Calling 'MyProc2(LogLevel=0)'" )
  call MyProc2(LogLevel=0)
  call Logger%Write( "Calling 'MyProc3(LogLevel=0)'" )
  call MyProc3(LogLevel=0)
  call Logger%Write( "Calling 'MyProc4(LogLevel=0)'" )
  call MyProc4(LogLevel=0)
  call Logger%Write( "Calling 'MyProc5(LogLevel=0)'" )
  call MyProc5(LogLevel=0)
  call Logger%Exiting()
  contains
  Subroutine MyProc1(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc1', LogLevel )
    call Logger%Write( "Printing a log message. Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc2(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc2', LogLevel )
    call Logger%Write( "Printing a log message. Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc3(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc3', LogLevel )
    call Logger%Write( "Printing a log message. Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc4(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc4', LogLevel )
    call Logger%Write( "Printing a log message. Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc5(LogLevel)
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc5', LogLevel )
    call Logger%Write( "Printing a log message. Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
End Subroutine

Subroutine Test_LogLevelPropagation
  type(Logger_Type)         ::    Logger
  call Logger%Entering( 'Test_LogLevelPropagation' )
  call Logger%Write( "This procedure tests the propagation of the log levels as procedures are calls." )
  call Logger%Write( "the logs from 'MyProc1', 'MyProc3' and 'MyProc5' will be forced," )
  call Logger%Write( "whatever the log levels of the log message printed in these procedures." )
  call Logger%Write( "Calling 'MyProc1()'" )
  call MyProc1()
  call Logger%Write( "Calling 'MyProc2(LogLevel=0)'" )
  call MyProc2(LogLevel=0)
  call Logger%Write( "Calling 'MyProc3(LogLevel=0)'" )
  call MyProc3(LogLevel=3)
  call Logger%Exiting()
  contains
  Subroutine MyProc1( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc1', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Write( "call MyProc1a( LogLevel )" )
    call MyProc1a( LogLevel )
    call Logger%Write( "call MyProc1b()" )
    call MyProc1b()
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc2( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc2', LogLevel=LogLevel )
    call Logger%Write( "call MyProc2a( LogLevel )" )
    call MyProc2a( LogLevel )
    call Logger%Write( "call MyProc2a( LogLevel=5 )" )
    call MyProc2a( LogLevel=5 )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc3( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc3', LogLevel )
    call Logger%Write( "call MyProc3a( LogLevel=Logger%On() )" )
    call MyProc3a( LogLevel=LogLevel )
    call Logger%Write( "call MyProc3b( LogLevel=0 )" )
    call MyProc3b( LogLevel=0 )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc1a( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc1a', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc1b( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc1b', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc2a( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc2a', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc2b( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc2b', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc3a( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc3a', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
  Subroutine MyProc3b( LogLevel )
    integer             ,optional ,intent(in)     ::    LogLevel
    call Logger%Entering( 'MyProc3b', LogLevel )
    call Logger%Write( "Logger%On() = ", Logger%On() )
    call Logger%Exiting()
  End Subroutine
End Subroutine

End Module