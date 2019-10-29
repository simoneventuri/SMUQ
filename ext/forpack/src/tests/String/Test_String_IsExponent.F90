Module Test_String_IsExponent

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_GetFinalIndexExponentPart()
  use String_Library   ,only:  GetFinalIndexExponentPart
  character(*)                                              ,parameter  ::  ProcName = "GetFinalIndexExponentPart"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String
  integer                                                               ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String    =   ""
  Expected  =   0
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "1"
  Expected  =   0
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E"
  Expected  =   0
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E+"
  Expected  =   0
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E0"
  Expected  =   2
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E0 0"
  Expected  =   2
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E+0"
  Expected  =   3
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E-0"
  Expected  =   3
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E+0 abc"
  Expected  =   3
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "E00125"
  Expected  =   6
  Description   =   "'"//ProcName//"': String='"//String//"''"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetFinalIndexExponentPart( String )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module