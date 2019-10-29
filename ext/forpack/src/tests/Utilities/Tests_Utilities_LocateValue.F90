@test
Subroutine Tests_Utilities_LocateValue()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  LocateValue

  implicit none

  character(*)                                              ,parameter  ::  ProcName='LocateValue'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
