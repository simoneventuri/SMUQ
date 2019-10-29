@test
Subroutine Test_String_NiceInteger()

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String
  use String_Library    ,only:  NiceInteger

  character(*)                                              ,parameter  ::  ProcName='NiceInteger'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  Number
  character(:)  ,allocatable                                            ::  ExpectedString, FoundString

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

  Number          =   123456789
  ExpectedString  =   "123,456,789"
  FoundString     =   NiceInteger(Number)
  Description   =   "'"//ProcName//"': Number='"//Convert_To_String(Number)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedString, FoundString, Description//": Wrong string between Expected/Found. " )
  call Logger%Write( "[ok]" )

End Subroutine