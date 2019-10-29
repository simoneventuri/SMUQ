@test
subroutine testString_NumbersLetters()
  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength
  use String_Library    ,only:  ParseNumbersLetters
  implicit none
  character(*)                                              ,parameter  ::  ProcName = "ParseNumbersLetters"
  integer                                                   ,parameter  ::  NPad = 100
  character(10)                                                         ::  Str1, Str2, Str3
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  Message
  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  FoundNumbers, ExpectedNumbers
  character(:)  ,allocatable                                            ::  FoundLetters, ExpectedLetters
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String            =   "1"
  ExpectedNumbers   =   "1"
  ExpectedLetters   =   ""
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "A"
  ExpectedNumbers   =   ""
  ExpectedLetters   =   "A"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "1A"
  ExpectedNumbers   =   "1"
  ExpectedLetters   =   "A"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "1.0A"
  ExpectedNumbers   =   "1.0"
  ExpectedLetters   =   "A"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "500  A1.0"
  ExpectedNumbers   =   "500"
  ExpectedLetters   =   "A1.0"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "1E3 K"
  ExpectedNumbers   =   "1E3"
  ExpectedLetters   =   "K"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "+1E3 K"
  ExpectedNumbers   =   "+1E3"
  ExpectedLetters   =   "K"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "-1E3 K"
  ExpectedNumbers   =   "-1E3"
  ExpectedLetters   =   "K"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "1E+03 Kelvin"
  ExpectedNumbers   =   "1E+03"
  ExpectedLetters   =   "Kelvin"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "1.2E-01 m/s"
  ExpectedNumbers   =   "1.2E-01"
  ExpectedLetters   =   "m/s"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "   1.2E-01 m/s"
  ExpectedNumbers   =   "1.2E-01"
  ExpectedLetters   =   "m/s"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "   1.2E-01m/s"
  ExpectedNumbers   =   "1.2E-01"
  ExpectedLetters   =   "m/s"
  Description       =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseNumbersLetters( String, FoundNumbers, FoundLetters )
! call Logger%Write( "-> Found:    Numbers = ", FoundNumbers,    "Letters = ", FoundLetters )
! call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
  @assertEqual(FoundNumbers,ExpectedNumbers,Description//": Wrong values for 'Numbers'. ")
  @assertEqual(FoundLetters,ExpectedLetters,Description//": Wrong values for 'Letters'. ")
  call Logger%Write( "[ok]" )
! =============================================================================
End subroutine
