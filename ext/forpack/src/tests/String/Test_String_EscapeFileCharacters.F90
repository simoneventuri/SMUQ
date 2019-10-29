@test
Subroutine Test_String_EscapeFileCharacters()
  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Inline, Convert_To_String
  use String_Library    ,only:  EscapeFileCharacters
  implicit none
  character(*)                                              ,parameter  ::  ProcName="EscapeFileCharacters"
  integer                                                   ,parameter  ::  NPad = 100
  logical                                                   ,parameter  ::  Detailed = .True.
  logical                                                               ::  Ok
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String      =   "name(d)"
  Expected    =   "name\(d\)"
  Found       =   EscapeFileCharacters( String )
  Description =   "'"//ProcName//"': 0d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine