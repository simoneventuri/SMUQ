@test
Subroutine Test_String_ParseFunction()

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength, Convert_To_String, Inline
  use String_Library    ,only:  ParseFunction
  implicit none

  character(*)                                              ,parameter  ::  ProcName='ParseFunction'
  integer                                                   ,parameter  ::  NPad = 100

  logical                                                               ::  Ok
  logical                                                               ::  DefToVal
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  DefArgNames(:)
  character(:)  ,allocatable                                            ::  String, SubString, Description
  character(:)  ,allocatable                                            ::  FoundFctName, FoundArgNames(:), FoundArgValues(:)
  character(:)  ,allocatable                                            ::  ExpectedFctName, ExpectedArgNames(:), ExpectedArgValues(:)
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  String            =   "f(x=)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  String            =   "f(=x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "" ]
  ExpectedArgValues =   [ Character(4) :: "x"  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues )
  Description   =   "'"//ProcName//"': String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .True.
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: ""  ]
  ExpectedArgValues =   [ Character(4) :: "x" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .False.
  String            =   "f(x)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x"]
  ExpectedArgValues =   [ Character(4) :: "" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames )
  Description   =   "'"//ProcName//"': String='"//String//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .True.
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["x"]
  String            =   "f(5)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x" ]
  ExpectedArgValues =   [ Character(4) :: "5" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefArgNames       =   ["x"]
  String            =   "f(5,l)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x",""  ]
  ExpectedArgValues =   [ Character(4) :: "5","l" ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames )
  Description   =   "'"//ProcName//"': String='"//String//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["x"]
  String            =   "f(5,l)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "x","l" ]
  ExpectedArgValues =   [ Character(4) :: "5",""  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
  DefToVal          =   .False.
  DefArgNames       =   ["file"]
  String            =   "f(toto,a=1,b=2,c)"
  ExpectedFctName   =   "f"
  ExpectedArgNames  =   [ Character(4) :: "file","a","b","c" ]
  ExpectedArgValues =   [ Character(4) :: "toto","1","2",""  ]
  call ParseFunction( String, FoundFctName, FoundArgNames, FoundArgValues, DefArgNames=DefArgNames, DefToVal=DefToVal )
  Description   =   "'"//ProcName//"': String='"//String//"' DefToVal='"//Convert_To_String(DefToVal)//"' DefArgNames='"//Inline(DefArgNames,Separator=',')//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( ExpectedFctName, FoundFctName, Description//": Wrong value for function name between Expected/Found. " )
  do i = 1,size(FoundArgNames)
    @assertEqual( ExpectedArgNames(i),  FoundArgNames(i),  Description//": Wrong value for argument name between Expected/Found for argument "//Convert_To_String(i)//". " )
    @assertEqual( ExpectedArgValues(i), FoundArgValues(i), Description//": Wrong value for argument value between Expected/Found for argument "//Convert_To_String(i)//". " )
  end do
  call Logger%Write( "[ok]" )
!   Ok    =     ( FoundFctName == ExpectedFctName )         &
!         .and. ( all(FoundArgNames == ExpectedArgNames) )  &
!         .and. ( all(FoundArgValues == ExpectedArgValues) )
!   if (ok) then; call Logger%Write( "[ok]" )
!   else;         call Logger%Write( "[FAILED]" ); end if
!   if (Detailed) then
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> FoundFctName       = ", FoundFctName    )
!     call Logger%Write( "-> FoundArgNames      = ", FoundArgNames    )
!     call Logger%Write( "-> FoundArgValues     = ", FoundArgValues    )
!     call Logger%Write( "------------------------" )
!     call Logger%Write( "-> ExpectedFctName    = ", ExpectedFctName    )
!     call Logger%Write( "-> ExpectedArgNames   = ", ExpectedArgNames    )
!     call Logger%Write( "-> ExpectedArgValues  = ", ExpectedArgValues )
!     call Logger%Write( "------------------------" )
!   end if
! =============================================================================
End Subroutine