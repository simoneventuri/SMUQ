Module Test_String_GetSeconds

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength
  use iso_fortran_env   ,only:  REAL64

  implicit none

  contains

@test
Subroutine Test_GetSeconds()
  use String_Library   ,only:  GetSeconds
  character(*)                                              ,parameter  ::  ProcName='GetSeconds'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String
  integer                                                               ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String    =   "10"
  Expected  =   10.0_REAL64
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "10.123"
  Expected  =   10.123_REAL64
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "10.123s"
  Expected  =   10.123_REAL64
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "2m10s"
  Expected  =   130.0_REAL64
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "1d"
  Expected  =   1.0_REAL64 * 60*60*24
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "1d2h3m4s"
  Expected  =   1.0_REAL64 * ( ( 1 * 24 + 2 ) * 60 + 3 ) * 60 + 4
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String    =   "1e2s"
  Expected  =   100.0_REAL64
  Description =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetSeconds( String )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module