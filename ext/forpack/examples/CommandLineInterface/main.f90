Program CommandLineInterfaceExample

  use CommandLineInterface_Library    ,only:  CommandLineInterface_Type
  use Logger_Class    ,only:  Logger

  implicit none

  character(*)  ,parameter  ::    ProcName = 'CommandLineInterfaceExample'
  logical       ,parameter  ::    i_Debug_Loc = .True.
  type(CommandLineInterface_Type)       ::    CommandLineInterface

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Exiting()

End Program
