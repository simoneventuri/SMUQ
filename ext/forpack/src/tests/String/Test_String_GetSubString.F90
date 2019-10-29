Module Test_String_GetSubString

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Inline

  implicit none

  contains

@test
Subroutine Test_GetSubString()
  use String_Library   ,only:  GetSubString
  character(*)                                              ,parameter  ::  ProcName = "GetSubString"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String, InBetween(:)
  character(:)  ,allocatable                                            ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String      =   "abcdefgh"
  InBetween   =   [ "abc", "fgh" ]
  Expected    =   "de"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[VAR]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "VAR"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[$[VAR]]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "$[VAR]"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[123$[VAR]XYZ]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "123$[VAR]XYZ"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[$[VAR]]]]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "$[VAR]"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[$[12$[34]]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "$[12$[34]"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "sin(f(x))"
  InBetween   =   [ "(", ")" ]
  Expected    =   "f(x)"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[V1]-$[V2]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "V1"
  Found       =   GetSubString( String, InBetween )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[X$[K1]Y]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "K1"
  Found       =   GetSubString( String, InBetween, Inner=.True. )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"' Inner=.True."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "$[[X$[K1]Y]]"
  InBetween   =   [ "$[", "] " ]
  Expected    =   "K1"
  Found       =   GetSubString( String, InBetween, Inner=.True. )
  Description =   "'"//ProcName//"': String='"//String//"' InBetween='"//Inline(InBetween,Separator=',')//"' Inner=.True."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong last value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module