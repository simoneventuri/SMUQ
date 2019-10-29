Module Debugger_Library

  use DebugVariable_Class       ,only:  DebugVariable_Type, NewDebugVariable
  use Debugger_Class            ,only:  Debugger_Type, Debugger

  implicit none

  private

  public  ::  DebugVariable_Type, NewDebugVariable
  public  ::  Debugger_Type, Debugger

End Module
