Module StringExample_Module

  use String_Library    ,only:  String_Type, Text_Type, SetLength, Convert_To_String
  use Logger_Class      ,only:  Logger

  implicit none

  public
  integer                                                   ,parameter  ::  NPad = 110
  logical                                                   ,parameter  ::  Detailed = .False.

  contains

Subroutine Example_AddFrame()
  use String_Library    ,only:  String_Type
  character(*)                                              ,parameter  ::  ProcName = 'Example_AddFrame'
  character(:)  ,allocatable                                            ::  String, Description, Found(:), Expected(:)
  type(Text_Type)                                                       ::  Text

  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'AddFrame' procedure" )


  String      =   "Toto"
  Text        =   String
  call Text%AddFrame()
  Expected    =   [ Character(len(String)+2) :: &
                    "******"                , &
                    "*Toto*"                , &
                    "******"                  ]
  Found       =   Text%GetLines()

  Description   =   "'Text%AddFrame': w/o argument"
  call Logger%Write( "Testing ", SetLength(Description,NPad,Pad="."), Advance=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (all(Found == Expected)) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "", "-> Found    = ", Found    )
    call Logger%Write( "", "-> Expected = ", Expected )
  end if

  call Logger%Exiting()

End Subroutine

Subroutine Example_RemoveDuplicateCharacters()
  use String_Library    ,only:  SetLength
  use String_Library    ,only:  RemoveDuplicateCharacters
  character(*)                                              ,parameter  ::  ProcName = 'Example_RemoveDuplicateCharacters'
  logical                                                   ,parameter  ::  Detailed = .False.
  character(:)  ,allocatable                                            ::  Description, Prefix
  character(:)  ,allocatable                                            ::  String, SubString, Found, Expected
  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'RemoveDuplicateCharacters'" )
  Prefix        =   Logger%GetPrefix_()//"-> "
! =============================================================================
  SubString     =   "/"
  String        =   "nothing-todo"
  Expected      =   "nothing-todo"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  SubString     =   "/"
  String        =   "///replace-at-start"
  Expected      =   "/replace-at-start"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  SubString     =   "/"
  String        =   "replace-at-end///"
  Expected      =   "replace-at-end/"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  SubString     =   "/"
  String        =   "home/user//doc"
  Expected      =   "home/user/doc"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  SubString     =   "/"
  String        =   "///home/user//doc"
  Expected      =   "/home/user/doc"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  SubString     =   "/"
  String        =   "/home/user//doc////"
  Expected      =   "/home/user/doc/"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   Prefix//"String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found    = ", Found    )
    call Logger%Write( "-> Expected = ", Expected )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  call Logger%Exiting()
End Subroutine


Subroutine Example_ParseNumbersLetters()
  use iso_fortran_env           ,only:  REAL64
  use String_Library            ,only:  ParseNumbersLetters
  character(*)                                              ,parameter  ::  ProcName = 'Example_ParseNumbersLetters'
  logical                                                   ,parameter  ::  Detailed = .False.
  logical                                                               ::  ok
  character(:)  ,allocatable                                            ::  Description, Prefix
  character(:)  ,allocatable                                            ::  NumbersLetters
  character(:)  ,allocatable                                            ::  FoundNumbers, ExpectedNumbers
  character(:)  ,allocatable                                            ::  FoundLetters, ExpectedLetters
  real(REAL64)                                                          ::  FoundNumbersR8, ExpectedNumbersR8
  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'ParseNumbersLetters'" )
  Prefix        =   Logger%GetPrefix_()//"-> "
! =============================================================================
  NumbersLetters    =   "1"
  ExpectedNumbers   =   "1"
  ExpectedLetters   =   ""
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   " 5"
  ExpectedNumbersR8   =   5.0_8
  ExpectedLetters   =   ""
  call ParseNumbersLetters( NumbersLetters, FoundNumbersR8, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbersR8 == ExpectedNumbersR8) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbersR8,    "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbersR8, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "A"
  ExpectedNumbers   =   ""
  ExpectedLetters   =   "A"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "1A"
  ExpectedNumbers   =   "1"
  ExpectedLetters   =   "A"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "1.0A"
  ExpectedNumbers   =   "1.0"
  ExpectedLetters   =   "A"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "500  A1.0"
  ExpectedNumbers   =   "500"
  ExpectedLetters   =   "A1.0"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "1E3 K"
  ExpectedNumbers   =   "1E3"
  ExpectedLetters   =   "K"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "+1E3 K"
  ExpectedNumbers   =   "+1E3"
  ExpectedLetters   =   "K"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "-1E3 K"
  ExpectedNumbers   =   "-1E3"
  ExpectedLetters   =   "K"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "1E+03 Kelvin"
  ExpectedNumbers   =   "1E+03"
  ExpectedLetters   =   "Kelvin"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "1.2E-01 m/s"
  ExpectedNumbers   =   "1.2E-01"
  ExpectedLetters   =   "m/s"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  NumbersLetters    =   "   1.2E-01 m/s"
  ExpectedNumbers   =   "1.2E-01"
  ExpectedLetters   =   "m/s"
  call ParseNumbersLetters( NumbersLetters, FoundNumbers, FoundLetters )
  Description   =   Prefix//"String='"//NumbersLetters//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
  ok                =   (FoundNumbers == ExpectedNumbers) .and. (FoundLetters == ExpectedLetters)
  if (ok) then;  call Logger%Write( "[ok]" )
  else;          call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "-> Found:    Numbers = ", FoundNumbers, "Letters = ", FoundLetters )
    call Logger%Write( "-> Expected: Numbers = ", ExpectedNumbers, "Letters = ", ExpectedLetters )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  call Logger%Exiting()

End Subroutine

!
! Subroutine Example()
!   character(*)                                              ,parameter  ::  ProcName = 'Example'
!   type(String_Type)                                                     ::  String
!   character(:)  ,allocatable                                            ::  Str, Str1, Str2
!   logical                                                               ::  CaseSensitive
!   write(*,*) 'TestString'
!   write(*,*) '=========='
!   Str   =   "Start(He)"
!   write(*,*) 'Testing GetSubString:'
!   String        =   "Start(He)"
!   Str1          =   "START("
!   Str2          =   ")"
!   CaseSensitive =   .False.
!   write(*,*) '-> String%Value = ', String%Value
!   write(*,*) '-> Str1 = ', Str1
!   write(*,*) '-> Str2 = ', Str2
!   write(*,*) '-> CaseSensitive = ', CaseSensitive
!   Str  =  String%GetSubString( Str1,Str2, CaseSensitive=CaseSensitive )
!   write(*,*) '-> result = ', Str
!   write(*,*) '-> expected = ', 'He'
! End Subroutine


Subroutine Example_GetSeconds()
  use iso_fortran_env           ,only:  REAL64
  use String_Library            ,only:  GetSeconds
  character(*)                                              ,parameter  ::  ProcName = 'Example_GetSeconds'
  logical                                                   ,parameter  ::  Detailed = .False.
  logical                                                               ::  Ok
  character(:)  ,allocatable                                            ::  Description, Prefix
  character(:)  ,allocatable                                            ::  String
  real(REAL64)                                                          ::  Found, Expected
  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'GetSeconds'" )
  Prefix        =   Logger%GetPrefix_()//"-> "
! =============================================================================
  String    =   "10"
  Expected  =   10.0_REAL64
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "10.123"
  Expected  =   10.123_REAL64
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "10.123s"
  Expected  =   10.123_REAL64
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "2m10s"
  Expected  =   130.0_REAL64
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "1d"
  Expected  =   1.0_REAL64 * 60*60*24
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "1d2h3m4s"
  Expected  =   1.0_REAL64 * ( ( 1 * 24 + 2 ) * 60 + 3 ) * 60 + 4
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String    =   "1e2s"
  Expected  =   100.0_REAL64
  Found     =   GetSeconds( String )
  Ok        =   Found == Expected
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  call Logger%Exiting()
End Subroutine


Subroutine Example_IsFunction()

  use String_Library            ,only:  IsFunction

  character(*)                                              ,parameter  ::  ProcName = 'Example_IsFunction'
  logical                                                   ,parameter  ::  Detailed = .False.
  logical                                                               ::  Ok
  character(:)  ,allocatable                                            ::  String, Description, Prefix
  logical                                                               ::  Found, Expected
  character(:)  ,allocatable                                            ::  Name
  logical                                                               ::  CaseSensitive
  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'IsFunction'" )
  Prefix        =   Logger%GetPrefix_()//"-> "
! =============================================================================
  String        =   "FctName()"
  Expected      =   .True.
  Found         =   IsFunction( String )
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  Name          =   "FctName"
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name )
  Description   =   Prefix//"String='"//String//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  Name          =   "FctName"
  CaseSensitive =   .True.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   Prefix//"String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  Name          =   "FctName"
  CaseSensitive =   .False.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   Prefix//"String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  Name          =   "FCTNAME"
  CaseSensitive =   .True.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .False.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   Prefix//"String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  Name          =   "FCTNAME"
  CaseSensitive =   .False.
  String        =   "FctName( Iter=1, CPUTime=30s )"
  Expected      =   .True.
  Found         =   IsFunction( String, Name, CaseSensitive=CaseSensitive )
  Description   =   Prefix//"String='"//String//"', Name='"//Name//"', CaseSensitive="//Convert_To_String(CaseSensitive)
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, TestDescription//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  if (Found == Expected) then;  call Logger%Write( "[ok]" )
  else;                         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "  -> Found        = ", Found    , Fr="es15.8" )
    call Logger%Write( "  -> Expected     = ", Expected , Fr="es15.8" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  call Logger%Exiting()
End Subroutine

Subroutine Example_ParseFunction()
  use String_Library    ,only:  SetLength, Convert_To_String, Inline
  use String_Library    ,only:  ParseFunction
  character(*)                                              ,parameter  ::  ProcName = 'Example_ParseFunction'
  logical                                                   ,parameter  ::  Detailed = .False.
  logical                                                               ::  Ok
  character(:)  ,allocatable                                            ::  Description, Prefix
  logical                                                               ::  DefToVal
  character(:)  ,allocatable                                            ::  DefArgNames(:)
  character(:)  ,allocatable                                            ::  String, SubString
  character(:)  ,allocatable                                            ::  FoundFctName, FoundArgNames(:), FoundArgValues(:)
  character(:)  ,allocatable                                            ::  ExpectedFctName, ExpectedArgNames(:), ExpectedArgValues(:)
  call Logger%Entering( ProcName )
  call Logger%Write( "Testing 'ParseFunction'" )
  Prefix        =   Logger%GetPrefix_()//"-> "
! =============================================================================
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   Prefix//"String='"//String//"' w/o arguments"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String            =   "f(x=)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   Prefix//"String='"//String//"' w/o arguments"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  String            =   "f(=x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "" ]
  ExpectedArgValues =   [ Character(4) :: "x"  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   Prefix//"String='"//String//"' w/o arguments"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .True.
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .False.
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x"]
  ExpectedArgValues =   [ Character(4) :: "" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames )
  Description   =   Prefix//"String='"//String//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .True.
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefArgNames       =   ["x"]
  String            =   "f(5,l)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x",""  ]
  ExpectedArgValues =   [ Character(4) :: "5","l" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames )
  Description   =   Prefix//"String='"//String//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["x"]
  String            =   "f(5,l)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x","l" ]
  ExpectedArgValues =   [ Character(4) :: "5",""  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["file"]
  String            =   "f(toto,a=1,b=2,c)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "file","a","b","c" ]
  ExpectedArgValues =   [ Character(4) :: "toto","1","2",""  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   Prefix//"String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength(Description,NPad,Pad="."), Advance=.False., Prefix=.False. )
!   @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
!   call Logger%Write( "[ok]" )
  Ok    =     ( FoundFctName == ExpectedFctName )         &
        .and. ( all(FoundArgNames == ExpectedArgNames) )  &
        .and. ( all(FoundArgValues == ExpectedArgValues) )
  if (ok) then; call Logger%Write( "[ok]" )
  else;         call Logger%Write( "[FAILED]" ); end if
  if (Detailed) then
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
    call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
    call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
    call Logger%Write( "------------------------" )
    call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
    call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
    call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
    call Logger%Write( "------------------------" )
    call Logger%Write( repeat("-",NPad), Prefix=.False. )
  end if
! =============================================================================
  call Logger%Exiting()
End Subroutine


End Module

Program Main

  use Logger_Class          ,only:  Logger
  use StringExample_Module

  implicit none

!   call Logger%Write( "Calling Example_AddFrame" )
!   call Example_AddFrame()

  call Logger%Write( "Calling Example_RemoveDuplicateCharacters" )
  call Example_RemoveDuplicateCharacters()

  call Logger%Write( "Calling Example_ParseNumbersLetters" )
  call Example_ParseNumbersLetters()

  call Logger%Write( "Calling Example_GetSeconds" )
  call Example_GetSeconds()

  call Logger%Write( "Calling Example_IsFunction" )
  call Example_IsFunction()

  call Logger%Write( "Calling Example_ParseFunction" )
  call Example_ParseFunction()

End Program
