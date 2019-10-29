Module Test_String_IsIntegerNumber

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String

  implicit none

  contains

@test
Subroutine Test_IsIntegerNumber()
  use String_Library   ,only:  IsIntegerNumber
  implicit none
  character(*)                                              ,parameter  ::  ProcName='IsIntegerNumber'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  String, Description
  logical                                                               ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String        =   "1"
  Expected      =   .True.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "-99547"
  Expected      =   .True.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "  +57   "
  Expected      =   .True.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1D5"
  Expected      =   .True.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1E+1"
  Expected      =   .True.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1E-1"
  Expected      =   .False.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1.0"
  Expected      =   .False.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1.0"
  Expected      =   .True.
  Found         =   IsIntegerNumber( String, Equiv=.True. )
  Description   =   "'"//ProcName//"': String='"//String//"', Equiv=True"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "abc"
  Expected      =   .False.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1.0E5"
  Expected      =   .False.
  Found         =   IsIntegerNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module