Module Test_String_ToBeSorted

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_NiceInteger()

  use String_Library   ,only:  NiceInteger
  use String_Library   ,only:  InLine

  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  Number
  character(:)  ,allocatable                                            ::  ExpectedString, FoundString

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

  Description   =   "'NiceInteger' with scalar input argument"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Number          =   123456789
  ExpectedString  =   "123,456,789"
  FoundString     =   NiceInteger(Number)
  @assertEqual( ExpectedString, FoundString, Description//": Wrong string between Expected/Found. " )
  call Logger%Write( "[ok]" )

End Subroutine

End Module