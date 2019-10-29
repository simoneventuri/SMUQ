@test
Subroutine Tests_Utilities_ParseNamesValues()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  ParseNamesValues

  implicit none

  character(*)                                              ,parameter  ::  ProcName='ParseNamesValues'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
