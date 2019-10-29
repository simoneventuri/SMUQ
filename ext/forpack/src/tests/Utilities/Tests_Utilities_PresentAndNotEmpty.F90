@test
Subroutine Tests_Utilities_PresentAndNotEmpty()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  PresentAndNotEmpty

  implicit none

  character(*)                                              ,parameter  ::  ProcName='PresentAndNotEmpty'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
