Module Test_String_GetNumberOfItems

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_GetNumberOfItems()
  use String_Library   ,only:  GetNumberOfItems
  character(*)                                              ,parameter  ::  ProcName='GetNumberOfItems'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  Expected, Found
  character(:)  ,allocatable                                            ::  String, Separator
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String      =   "abcdefghg"
  Expected    =   1
  Found       =   GetNumberOfItems( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "abc def ghg"
  Expected    =   3
  Found       =   GetNumberOfItems( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "abc   def   ghg"
  Expected    =   3
  Found       =   GetNumberOfItems( String )
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "abc;def; ghg;1 2"
  Expected    =   4
  Found       =   GetNumberOfItems( String, Separator=";" )
  Description =   "'"//ProcName//"': String='"//String//"' Separator='"//Separator//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module