@test
Subroutine Test_String_ReplaceCharacter()
  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Inline, Convert_To_String
  use String_Library    ,only:  ReplaceCharacter
  implicit none
  character(*)                                              ,parameter  ::  ProcName="ReplaceCharacter"
  integer                                                   ,parameter  ::  NPad = 100
  logical                                                   ,parameter  ::  Detailed = .True.
  logical                                                               ::  Ok
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String, Strings(:), Old0d, New0d, Old1d(:), New1d(:)
  character(:)  ,allocatable                                            ::  Found, Expected, Found1d(:), Expected1d(:)
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String      =   "one two three"
  Old0d       =   "one"
  New0d       =   "xxx"
  Expected    =   "xxx two three"
  Found       =   ReplaceCharacter( String, Old0d, New0d )
  Description =   "'"//ProcName//"': 0d String='"//String//"', Old='"//Old0d//"'"//"', New='"//New0d//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "one two three"
  Old0d       =   "e two t"
  New0d       =   " xxx "
  Expected    =   "on xxx hree"
  Found       =   ReplaceCharacter( String, Old0d, New0d )
  Description =   "'"//ProcName//"': 0d String='"//String//"', Old='"//Old0d//"'"//"', New='"//New0d//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "one two three"
  Old0d       =   "e two t"
  New0d       =   " xxx "
  Expected    =   "on xxxhree"
  Found       =   ReplaceCharacter( String, Old0d, New0d, Trimed=.True. )
  Description =   "'"//ProcName//"': 0d String='"//String//"', Old='"//Old0d//"'"//"', New='"//New0d//"', Trimed=.True."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "one two three"
  Old1d       =   [ Character(3) :: "one", "thr" ]
  New1d       =   [ Character(3) :: "xxx", "yyy" ]
  Expected    =   "xxx two yyyee"
  Found       =   ReplaceCharacter( String, Old1d, New1d )
  Description =   "'"//ProcName//"': 0d String='"//String//"', Old='["//Inline(Old1d,Separator=',')//"]'"//"', New='["//Inline(New1d,Separator=',')//"]'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String      =   "abcde abcde"
  Old1d       =   [ Character(1) :: ["b","c","d"] ]
  New1d       =   [ Character(1) :: ["2","3","4"] ]
  Expected    =   "a234e a234e"
  Found       =   ReplaceCharacter( String, Old1d, New1d )
  Description =   "'"//ProcName//"': 0d String='"//String//"', Old='["//Inline(Old1d,Separator=',')//"]'"//"', New='["//Inline(New1d,Separator=',')//"]'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value for function name between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
!   Strings     =   ["abcde","edcba"]
!   Old0d       =   "c"
!   New0d       =   "X"
!   Expected1d  =   ["abXde","edXba"]
!   Found1d     =   ReplaceCharacter( Strings, Old0d, Old0d )
!   Description =   "'"//ProcName//"': 1d Strings='["//Inline(Strings,Separator=',')//"]', Old='"//Old0d//"'"//"', New='"//New0d//"'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( size(Expected1d), size(Found1d), Description//": Wrong number of values between Expected/Found. " )
!   do i = 1,size(Expected1d)
!     @assertEqual( Expected1d(i),  Found1d(i),  Description//": Wrong value between Expected/Found for element "//Convert_To_String(i)//". " )
!   end do
!   call Logger%Write( "[ok]" )
! ! =============================================================================
!   Strings     =   ["abcde","edcba"]
!   Old1d       =   [ Character(1) :: ["b","c","d"] ]
!   New1d       =   [ Character(1) :: ["2","3","4"] ]
!   Expected1d  =   ["a234e","e432a"]
!   Found1d     =   ReplaceCharacter( Strings, Old1d, New1d )
!   Description =   "'"//ProcName//"': 1d Strings='["//Inline(Strings,Separator=',')//"]', Old='["//Inline(Old1d,Separator=',')//"]'"//"', New='["//Inline(New1d,Separator=',')//"]'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( size(Expected1d), size(Found1d), Description//": Wrong number of values between Expected/Found. " )
!   do i = 1,size(Expected1d)
!     @assertEqual( Expected1d(i),  Found1d(i),  Description//": Wrong value between Expected/Found for element "//Convert_To_String(i)//". " )
!   end do
!   call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine