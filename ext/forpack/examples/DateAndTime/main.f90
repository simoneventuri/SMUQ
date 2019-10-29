Program DateAndTimeExample

  use Logger_Class    ,only:  Logger
  use DateAndTime_Library    ,only:  DateAndTime_Type

  implicit none

  character(*)  ,parameter  ::    ProcName = 'DateAndTimeExample'
  logical       ,parameter  ::    i_Debug_Loc = .True.
  type(DateAndTime_Type)       ::    DateAndTime

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Exiting()

End Program
