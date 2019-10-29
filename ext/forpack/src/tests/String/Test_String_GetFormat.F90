Module Test_String_GetFormat

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String

  implicit none

  contains

@test
Subroutine Test_GetFormat()
  use String_Library   ,only:  GetFormat
  character(*)                                              ,parameter  ::  ProcName='GetFormat'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  iVar
  character(:)  ,allocatable                                            ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  iVar      =   1
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   "i1"
  Found     =   GetFormat( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   -1
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   "i1"
  Found     =   GetFormat( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! ! =============================================================================
  iVar      =   11
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   "i2"
  Found     =   GetFormat( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! ! =============================================================================
  iVar      =   123456
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   "i6"
  Found     =   GetFormat( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   0
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   "i1"
  Found     =   GetFormat( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module