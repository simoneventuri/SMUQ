Module Test_String_IsFunction

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String

  implicit none

  contains

@test
Subroutine Test_IsFunction()
  use String_Library   ,only:  IsFunction
  implicit none
  character(*)                                              ,parameter  ::  ProcName='IsFunction'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  String, Description
  logical                                                               ::  Found, Expected
  character(:)  ,allocatable                                            ::  Name
  logical                                                               ::  CaseSensitive
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String        =   "FctName()"
  Expected      =   .True.
  Found         =   IsFunction( String )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Name          =   "FctName"
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Name          =   "FctName"
  CaseSensitive =   .True.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   "'"//ProcName//"': String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Name          =   "FctName"
  CaseSensitive =   .False.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   "'"//ProcName//"': String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Name          =   "FCTNAME"
  CaseSensitive =   .True.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .False.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   "'"//ProcName//"': String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Name          =   "FCTNAME"
  CaseSensitive =   .False.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   "'"//ProcName//"': String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module