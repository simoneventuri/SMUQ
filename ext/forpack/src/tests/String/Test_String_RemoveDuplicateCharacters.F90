Module Test_String_RemoveDuplicateCharacters

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String

  implicit none

  contains

@test
Subroutine Example_RemoveDuplicateCharacters()
  use String_Library    ,only:  SetLength
  use String_Library    ,only:  RemoveDuplicateCharacters
  implicit none
  character(*)                                              ,parameter  ::  ProcName='RemoveDuplicateCharacters'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  String, SubString, Description, Found, Expected
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  SubString     =   "/"
  String        =   "nothing-todo"
  Expected      =   "nothing-todo"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
  SubString     =   "/"
  String        =   "///replace-at-start"
  Expected      =   "/replace-at-start"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
  SubString     =   "/"
  String        =   "replace-at-end///"
  Expected      =   "replace-at-end/"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
  SubString     =   "/"
  String        =   "home/user//doc"
  Expected      =   "home/user/doc"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
  SubString     =   "/"
  String        =   "///home/user//doc"
  Expected      =   "/home/user/doc"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
  SubString     =   "/"
  String        =   "/home/user//doc////"
  Expected      =   "/home/user/doc/"
  Found         =   RemoveDuplicateCharacters( String, SubString )
  Description   =   "'"//ProcName//"': String='"//String//"', SubString='"//SubString//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
!   if (Found == Expected) then;  call Logger%Write( "[ok]" )
!   else
!     call Logger%Write( "[FAILED]" )
!     call Logger%Write( "-> Found    = ", Found    )
!     call Logger%Write( "-> Expected = ", Expected )
!   end if
! =============================================================================
End Subroutine

End Module