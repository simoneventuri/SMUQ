Module Test_String_Split

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Inline

  implicit none

  contains

! Subroutine Split( Input, LHS, Separator, RHS, EscLHS, EscRHS, i_Length_Eq_Inp, IgnoreBetween, i_Debug )
@test
Subroutine Test_Split()

  use String_Library   ,only:  Split

  character(*)                                              ,parameter  ::  ProcName = "Split"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String, Separator
  character(:)  ,allocatable                                            ::  Escape, IgnoreBetween(:)
  character(:)  ,allocatable                                            ::  LHS_Expected, LHS_Found
  character(:)  ,allocatable                                            ::  RHS_Expected, RHS_Found
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  Separator     =   ":"
  String        =   "abc:def:ghi"
  LHS_Expected  =   "abc"
  RHS_Expected  =   "def:ghi"
  call Split( String, LHS_Found, Separator, RHS_Found )
  Description   =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
  @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Separator     =   ":@#"
  String        =   "abc:@#def:ghi"
  LHS_Expected  =   "abc"
  RHS_Expected  =   "def:ghi"
  call Split( String, LHS_Found, Separator, RHS_Found )
  Description   =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
  @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )
  call Logger%Write( "[ok]" )
! =============================================================================
! ! Same than test-1 but with a LHS escae character.
! ! The split should be be done at the 1st ':' but at the 2nd one.
! ! FAILURE
!   Separator     =   ":"
!   Escape        =   "#"
!   String        =   "abc#:def:ghi"
!   LHS_Expected  =   "abc#:def"
!   RHS_Expected  =   "ghi"
!   Command       =   "call Split( String, LHS, Separator, RHS, EscLHS )"
!   Message       =   "Command='"//Command//"'  String='"//String//"'  Separator='"//Separator//"'  EscLHS='"//Escape//"'."
!   call Logger%Write( Message, NewLine=.True. )
!   call Split( String, LHS_Found, Separator, RHS_Found, EscLHS=Escape )
!   Ok    =   ( LHS_Found == LHS_Expected ) .and. ( RHS_Found == RHS_Expected )
!   if (ok) then; TestStat = "PASSED"; else; TestStat = "FAILED"; end if
!   call Logger%Write( "-> LHS_Found = ", LHS_Found, "LHS_Expected = ", LHS_Expected )
!   call Logger%Write( "-> RHS_Found = ", RHS_Found, "RHS_Expected = ", RHS_Expected )
!   call Logger%Write( "-> ", TestStat )
!   @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
!   @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )

! =============================================================================
  Separator     =   ":"
  Escape        =   "#"
  String        =   "abc:#def:ghi"
  LHS_Expected  =   "abc:#def"
  RHS_Expected  =   "ghi"
  call Split( String, LHS_Found, Separator, RHS_Found, EscRHS=[Escape] )
  Description   =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"//"', EscRHS='"//Escape//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
  @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )
  call Logger%Write( "[ok]" )
! =============================================================================
! Testing with the optional input argument 'IgnoreBetween' with a single element in this array.
! The 1st separator is in between two 'IgnoreBetween' characters so it should be ignored
! and the character should be splitted at the 2nd separator.
  Separator     =   ":"
  IgnoreBetween =   ["@"]
  String        =   "ab@c:d@ef:ghi"
  LHS_Expected  =   "ab@c:d@ef"
  RHS_Expected  =   "ghi"
  call Split( String, LHS_Found, Separator, RHS_Found, IgnoreBetween=IgnoreBetween )
  Description   =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"//"', IgnoreBetween='"//Inline(IgnoreBetween,",")//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
  @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )
  call Logger%Write( "[ok]" )
! =============================================================================
! ! Testing with the optional input argument 'IgnoreBetween' with a two element in this array.
! ! The 1st separator is in between the two elements of the 'IgnoreBetween' array so it
! ! should be ignored and the character should be splitted at the 2nd separator.
!   Separator     =   ":"
!   IgnoreBetween =   ["@","%"]
!   String        =   "ab@c:d%ef:ghi"
!   LHS_Expected  =   "ab@c:d%ef"
!   RHS_Expected  =   "ghi"
!   call Split( String, LHS_Found, Separator, RHS_Found, IgnoreBetween=IgnoreBetween )
!   Description   =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"//"', IgnoreBetween='"//Inline(IgnoreBetween,",")//"'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( LHS_Expected, LHS_Found, Description//": Wrong value between Expected/Found for LHS. " )
!   @assertEqual( RHS_Expected, RHS_Found, Description//": Wrong value between Expected/Found for RHS. " )
!   call Logger%Write( "[ok]" )
! ! =============================================================================
End Subroutine

End Module