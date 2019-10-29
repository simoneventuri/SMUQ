Module Test_String_GetNumberOfDigits

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String
  use iso_fortran_env   ,only:  REAL64

  implicit none

  contains

@test
Subroutine Test_GetNumberOfDigits()
  use String_Library   ,only:  GetNumberOfDigits
  character(*)                                              ,parameter  ::  ProcName='GetNumberOfDigits'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  iVar
  integer                                                               ::  Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  iVar      =   1
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   1
  Found     =   GetNumberOfDigits( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   12
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   2
  Found     =   GetNumberOfDigits( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   123
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   3
  Found     =   GetNumberOfDigits( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   123456789
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   9
  Found     =   GetNumberOfDigits( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  iVar      =   0
  Description =   "'"//ProcName//"': Var='"//Convert_To_String(iVar)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Expected  =   1
  Found     =   GetNumberOfDigits( iVar )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module