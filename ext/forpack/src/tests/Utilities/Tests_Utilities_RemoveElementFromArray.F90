@test
Subroutine Tests_Utilities_RemoveElementFromArray()

  use pfunit_mod
  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  RemoveElementFromArray

  implicit none

  character(*)                                              ,parameter  ::  ProcName='RemoveElementFromArray'
  integer                                                   ,parameter  ::  NPad = 100

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

End Subroutine
