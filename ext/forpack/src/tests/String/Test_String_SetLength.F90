Module Test_String_SetLength

  use pfunit_mod
  use Logger_Class      ,only:  Logger

  implicit none

  contains

@test
Subroutine Test_SetLength()

  use String_Library    ,only:  SetLength
  implicit none


  character(*)                                              ,parameter  ::  ProcName='SetLength'
  integer                                                   ,parameter  ::  NPad = 100
  character(1)                                              ,parameter  ::  Pad = "*"
  integer                                                               ::  Length, InitialLength
  character(:)  ,allocatable                                            ::  Description

!   integer                                                               ::  i, N1
!   character(:)  ,allocatable                                            ::  Initial_0d, Length_Found, Expected_0d
!   character(:)  ,allocatable                                            ::  Initial_1d(:), Found_1d(:), Expected_1d(:)
!   integer                                                               ::  Length_Expected, InitialLength

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

  Block
    character(:)  ,allocatable                                            ::  Initial, Found, Expected
! =============================================================================
    Description   =   "'SetLength' 0d version, No padding, Requested Length < Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   4
    Initial   =   "123456"
    Expected  =   "1234"
    Found     =   SetLength(Initial,Length)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, No padding, Requested Length > Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   8
    Initial   =   "123456"
    Expected  =   "123456  "
    Found     =   SetLength(Initial,Length)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value from. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, No padding, Requested Length = Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   6
    Initial   =   "123456"
    Expected  =   "123456"
    Found     =   SetLength(Initial,Length)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, No padding, Requested Length >> Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   100000
    Initial   =   "123456"
    Expected  =   Initial // repeat(" ",Length-len(Initial))
    Found     =   SetLength(Initial,Length)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length >> Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   0
    Initial   =   "123456"
    Expected  =   ""
    Found     =   SetLength(Initial,Length)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length < Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Length    =   len(Initial)-2
    Initial   =   "123456"
    Expected  =   Initial(1:Length)
    Found     =   SetLength(Initial,Length,Pad=Pad)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length > Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Initial   =   "123456"
    Length    =   len(Initial)+2
    Expected  =   Initial // repeat(Pad,Length-len(Initial))
    Found     =   SetLength(Initial,Length,Pad=Pad)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length = Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Initial   =   "123456"
    Length    =   len(Initial)
    Expected  =   Initial
    Found     =   SetLength(Initial,Length,Pad=Pad)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length >> Initial Length"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Initial   =   "123456"
    Length    =   100000
    Expected  =   Initial // repeat(Pad,Length-len(Initial))
    Found     =   SetLength(Initial,Length,Pad=Pad)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetLength' 0d version, With padding, Requested Length = 0"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Initial   =   "123456"
    Length    =   0
    Expected  =   ""
    Found     =   SetLength(Initial,Length,Pad=Pad)
    @assertEqual( Length, len(Found), Description//": Wrong string Length. ")
    @assertEqual( Expected//"|", Found//"|", Description//": Wrong string value. ")
    call Logger%Write( "[ok]" )
! =============================================================================
  End Block


! ! ********************
! ! Testing SetLength_1d
! ! ********************
!
!   Initial_1d  =   ["123456","abcdef","qwerty"]
!   InitialLength      =   len(Initial_1d)
!   N1          =   size(Initial_1d,1)
!
!   Length    =   InitialLength-2
!   Expected_1d =   ["1234","abcd","qwer"]    ! Initial_1d(:)(1:Length)  <- ! @COMPILER_BUG:gcc-8.2.0:  Correct but fail with
!   Found_1d    =   SetLength(Initial_1d,Length)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, No padding, Requested length < Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, No padding, Requested length < Initial length).")
!   end do
!
!   Length    =   InitialLength+2
!   Expected_1d =   Initial_1d // repeat(" ",Length-InitialLength)
! !   Expected_1d =   Initial_1d // repeat(" ",Length-len(Initial_1d))   ! Very strange: This is very long !!!
!   Found_1d    =   SetLength(Initial_1d,Length)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, No padding, Requested length > Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, No padding, Requested length > Initial length).")
!   end do
!
!   Length    =   InitialLength
!   Expected_1d =   Initial_1d
!   Found_1d    =   SetLength(Initial_1d,Length)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, No padding, Requested length = Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, No padding, Requested length = Initial length).")
!   end do
!
!   Length    =   100000
!   Expected_1d =   Initial_1d // repeat(" ",Length-InitialLength)
!   Found_1d    =   SetLength(Initial_1d,Length)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, No padding, Requested length >> Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, No padding, Requested length >> Initial length).")
!   end do
!
!   Length    =   0
!   Expected_1d =   [("",i=1,N1)]
!   Found_1d    =   SetLength(Initial_1d,Length)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, No padding, Requested length = 0).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, No padding, Requested length = 0).")
!   end do
!
!   Length    =   InitialLength-2
!   Expected_1d =   ["1234","abcd","qwer"]    ! Initial_1d(:)(1:Length)  <- ! @COMPILER_BUG:gcc-8.2.0:  Correct but fail with
!   Found_1d    =   SetLength(Initial_1d,Length,Pad=Pad)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, With padding, Requested length < Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, With padding, Requested length < Initial length).")
!   end do
!
!   Length    =   InitialLength+2
!   Expected_1d =   Initial_1d // repeat(Pad,Length-InitialLength)
!   Found_1d    =   SetLength(Initial_1d,Length,Pad=Pad)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, With padding, Requested length > Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, With padding, Requested length > Initial length).")
!   end do
!
!   Length    =   InitialLength
!   Expected_1d =   Initial_1d
!   Found_1d    =   SetLength(Initial_1d,Length,Pad=Pad)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, With padding, Requested length = Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, With padding, Requested length = Initial length).")
!   end do
!
!   Length    =   100000
!   Expected_1d =   Initial_1d // repeat(Pad,Length-InitialLength)
!   Found_1d    =   SetLength(Initial_1d,Length,Pad=Pad)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, With padding, Requested length >> Initial length).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, With padding, Requested length >> Initial length).")
!   end do
!
!   Length    =   0
!   Expected_1d =   [("",i=1,N1)]
!   Found_1d    =   SetLength(Initial_1d,Length,Pad=Pad)
!   @assertEqual( Length, len(Found_1d), Description//": Wrong string length from "//TestedProc//" (1d version, With padding, Requested length = 0).")
!   do i =1,N1
!     @assertEqual( Expected_1d(i)//"|", Found_1d(i)//"|", Description//": Wrong string value from "//TestedProc//" (1d version, With padding, Requested length = 0).")
!   end do

End Subroutine

End Module