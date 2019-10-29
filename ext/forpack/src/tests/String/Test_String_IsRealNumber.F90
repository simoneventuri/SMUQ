Module Test_String_IsRealNumber

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String

  implicit none

  contains

@test
Subroutine Test_IsRealNumber()
  use String_Library   ,only:  IsRealNumber
  implicit none
  character(*)                                              ,parameter  ::  ProcName='IsRealNumber'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  String, Description
  logical                                                               ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String        =   "1.0"
  Expected      =   .True.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "-2.0  "
  Expected      =   .True.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "  +5.0    "
  Expected      =   .True.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1.0D5"
  Expected      =   .True.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1.0E+1"
  Expected      =   .True.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1"
  Expected      =   .True.
  Found         =   IsRealNumber( String, Equiv=.True. )
  Description   =   "'"//ProcName//"': String='"//String//"', Equiv=True"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String        =   "1"
  Expected      =   .False.
  Found         =   IsRealNumber( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
!   String        =   "1.0"
!   Expected      =   .False.
!   Found         =   IsRealNumber( String )
!   Description   =   "'"//ProcName//"': String='"//String//"'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
! ! =============================================================================
!   String        =   "1.0"
!   Expected      =   .True.
!   Found         =   IsRealNumber( String, Equiv=.True. )
!   Description   =   "'"//ProcName//"': String='"//String//"', Equiv=True"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
! ! =============================================================================
!   String        =   "abc"
!   Expected      =   .False.
!   Found         =   IsRealNumber( String )
!   Description   =   "'"//ProcName//"': String='"//String//"'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
! ! =============================================================================
!   String        =   "1.0E5"
!   Expected      =   .False.
!   Found         =   IsRealNumber( String )
!   Description   =   "'"//ProcName//"': String='"//String//"'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
! ! =============================================================================
End Subroutine

End Module