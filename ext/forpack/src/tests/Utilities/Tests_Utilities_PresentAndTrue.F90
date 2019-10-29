@test
Subroutine Tests_Utilities_PresentAndTrue()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  PresentAndTrue

  implicit none

  character(*)                                              ,parameter  ::  ProcName='PresentAndTrue'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
