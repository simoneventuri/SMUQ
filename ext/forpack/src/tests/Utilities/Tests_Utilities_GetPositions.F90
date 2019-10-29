@test
Subroutine Tests_Utilities_GetPositions()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  GetPositions

  implicit none

  character(*)                                              ,parameter  ::  ProcName='GetPositions'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
