Module test_string_GetLettersRightOfNumbers

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_GetLettersRightOfNumbers()
  use String_Library   ,only:  GetLettersRightOfNumbers
  character(*)                                              ,parameter  ::  ProcName = "GetLettersRightOfNumbers"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  Expected, Found
  character(:)  ,allocatable                                            ::  String, Separator
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String      =   "N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "123"
  Expected    =   ""
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "100N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "100  N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1E34N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1.12345N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1.12345 N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1.345E+1N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1.34E67  N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "1.N2"
  Expected    =   "N2"
  Found       =   GetLettersRightOfNumbers( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module