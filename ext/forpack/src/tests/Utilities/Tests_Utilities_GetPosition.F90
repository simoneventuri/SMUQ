@test
Subroutine Tests_Utilities_GetPosition()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  GetPosition

  implicit none

  character(*)                                              ,parameter  ::  ProcName='GetPosition'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
