Module Test_String_VecTrim

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_LenTrim()

  use String_Library   ,only:  LenTrim
  use String_Library   ,only:  InLine

  character(*)                                              ,parameter  ::  ProcName = "LenTrim"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String0d, String1d(:), String2d(:,:)
  integer                                                               ::  Length_Expected, Length_Found
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String0d          =   "123456   "
  Length_Expected   =   6
  Length_Found      =   LenTrim(String0d)
  Description       =   "'"//ProcName//"': String='"//String0d//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Length_Expected, Length_Found, Description//": Wrong length between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String1d          =   ["1234 ","12   ","1    "]
  Length_Expected   =   4
  Length_Found      =   LenTrim(String1d)
  Description       =   "'"//ProcName//"': String->rank-1 array"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Length_Expected, Length_Found, Description//": Wrong length between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String2d          =   reshape(["1234 ","12   ","1    ","1    ","1234 ","123  "],[2,3] )
  Length_Expected   =   4
  Length_Found      =   LenTrim(String2d)
  Description       =   "'"//ProcName//"': String->rank-2 array"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Length_Expected, Length_Found, Description//": Wrong length between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

@test
Subroutine Test_VecTrim()

  use String_Library   ,only:  VecTrim

  character(*)                                              ,parameter  ::  ProcName = "VecTrim"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  i, j
  character(10)                                                         ::  s10
  character(:)  ,allocatable                                            ::  is, js
  character(:)  ,allocatable                                            ::  Initial0d, Found0d, Expected0d
  character(:)  ,allocatable  ,dimension(:)                             ::  Initial1d, Found1d, Expected1d
  character(:)  ,allocatable  ,dimension(:,:)                           ::  Initial2d, Found2d, Expected2d
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  Initial0d   =   "123456   "
  Expected0d  =   "123456"
  Found0d     =   VecTrim(Initial0d)
  Description       =   "'"//ProcName//"': String='"//Initial0d//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected0d, Found0d, Description//": Wrong size between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Initial1d   =   ["123456   ","abc      "]
  Expected1d  =   ["123456"   ,"abc   "]
  Found1d     =   VecTrim(Initial1d)
  if ( allocated(Found1d) ) deallocate(Found1d)
  allocate( Found1d , source = VecTrim(Initial1d) )
  Description       =   "'"//ProcName//"': String->rank-1 array'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( size(Expected1d), size(Found1d), Description//": Wrong size between Expected/Found. " )
  @assertEqual( len(Expected1d), len(Found1d), Description//": Wrong length between Expected/Found. " )
  do i = 1,size(Expected1d)
    write(s10,"(i0)") i; is = trim(s10)
    @assertEqual( Expected1d(i), Found1d(i), Description//": Wrong value between Expected/Found for element "//is//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  Initial2d   =   reshape( ["123456   ","abc      ","1234     ","a        "], [2,2] )
  Expected2d  =   reshape( ["123456"   ,"abc   ","1234  ","a     "], [2,2] )
  Found2d     =   VecTrim(Initial2d)
  Description       =   "'"//ProcName//"': String->rank-2 array'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( size(Expected2d), size(Found2d), Description//": Wrong size between Expected/Found. " )
  @assertEqual( len(Expected2d), len(Found2d), Description//": Wrong length between Expected/Found. " )
  do j = 1,size(Expected2d,2)
    write(s10,"(i0)") j; js = trim(s10)
    do i = 1,size(Expected2d,1)
      write(s10,"(i0)") i; is = trim(s10)
      @assertEqual( Expected2d(i,j), Found2d(i,j), Description//": Wrong value between Expected/Found for element ("//is//","//js//"). " )
    end do
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module