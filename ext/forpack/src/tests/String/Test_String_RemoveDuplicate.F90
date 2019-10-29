Module Test_String_RemoveDuplicate

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Inline

  implicit none

  contains

@test
Subroutine Test_RemoveDuplicate()
  use String_Library    ,only:  RemoveDuplicate
  character(*)                                              ,parameter  ::  ProcName='RemoveDuplicate'
  integer                                                   ,parameter  ::  NPad = 100
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  String(:), Found(:), Expected(:)
  character(:)  ,allocatable                                            ::  Description
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String        =   ["A","B","C","C","D"]
  Expected      =   ["A","B","C","D"]
  Found         =   RemoveDuplicate( String )
  Description   =   "'"//ProcName//"': String='"//Inline(String)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( size(Expected), size(Found), Description//": Wrong number of elements between Expected/Found. " )
  @assertEqual( len(Expected) , len(Found) , Description//": Wrong length between Expected/Found. " )
  do i = 1,size(Expected)
    @assertEqual( Expected(i), Found(i), Description//": Wrong value between Expected/Found for element "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

Function s(i) result(str)
  integer                                               ,intent(in)     ::  i
  character(:)  ,allocatable                                            ::  str
  character(10)                                                         ::  s10
  write(s10,"(i0)") i
  Str   =   trim(s10)
End Function

End Module