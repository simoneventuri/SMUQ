SubModule(InputReader_Class) InputReader_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_DEBUG, LogLevel_HEAVYDEBUG
  use String_Library        ,only:  String_Type

  implicit none

  logical   ,parameter  ::  DefaultAllowMacro   = .True.
  logical   ,parameter  ::  DefaultAllowEnvVar  = .True.

  contains

# include "InputReader_include.F90"

End SubModule
