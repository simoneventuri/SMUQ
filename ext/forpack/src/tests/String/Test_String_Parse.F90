Module Test_String_Parse

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  integer                                                   ,parameter  ::  NPad = 100

  contains

@test
Subroutine test_Parse()
  use String_Library   ,only:  Parse
  character(*)                                              ,parameter  ::  ProcName='Parse'
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Separator, String
  character(:)  ,allocatable  ,dimension(:)                             ::  Expected(:), Found(:)
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  Separator         =   ":"
  String            =   "abc:def:ghg"
  Expected          =   ["abc","def","ghg"]
  Description       =   "'"//ProcName//"': String='"//String//"', Separator='"//Separator//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Parse( String, Separator, Found )
  @assertEqual( size(Expected), size(Found), Description//": Wrong size between Expected/Found. " )
  @assertEqual( len(Expected), len(Found), Description//": Wrong length between Expected/Found. " )
  do i = 1,size(Expected)
    @assertEqual( Expected(i), Found(i), Description//": Wrong value between Expected/Found for element "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine


@test
Subroutine test_ParseFunction()
  use String_Library   ,only:  ParseFunction
  character(*)                                              ,parameter  ::  ProcName='ParseFunction'
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  i, j
  character(:)  ,allocatable                                            ::  String, FctSep, ArgSep, ValSep
  character(:)  ,allocatable                                            ::  ExpectedName, FoundName
  character(:)  ,allocatable                                            ::  ExpectedArg0, FoundArg0
  character(:)  ,allocatable  ,dimension(:)                             ::  ExpectedArg1, FoundArg1
  character(:)  ,allocatable  ,dimension(:,:)                           ::  ExpectedArg2, FoundArg2
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String            =   "FunctionName(A=1,B='x',C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg0      =   "A=1,B='x',C=78"
  Description       =   "'"//ProcName//"': 0d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg0 )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( ExpectedArg0, FoundArg0, Description//": Wrong value for function argument list. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName[A=1,B='x',C=78]"
  ExpectedName      =   "FunctionName"
  ExpectedArg0      =   "A=1,B='x',C=78"
  FctSep            =   "["
  Description       =   "'"//ProcName//"': 0d String='"//String//"', FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg0, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( ExpectedArg0, FoundArg0, Description//": Wrong value for function argument list. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1,B='x',C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg0      =   "A=1,B='x',C=78"
  FctSep            =   "{"
  Description       =   "'"//ProcName//"': 0d String='"//String//"', FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg0, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( ExpectedArg0, FoundArg0, Description//": Wrong value for function argument list. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName@A=1,B='x',C=78@"
  ExpectedName      =   "FunctionName"
  ExpectedArg0      =   "A=1,B='x',C=78"
  FctSep            =   "@"
  Description       =   "'"//ProcName//"': 0d String='"//String//"', FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg0, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( ExpectedArg0, FoundArg0, Description//": Wrong value for function argument list. " )
  call Logger%Write( "[ok]" )
! =============================================================================
! =============================================================================
! =============================================================================
  String            =   "FunctionName(A=1,B='x',C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  Description       =   "'"//ProcName//"': 1d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1 )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1) , len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName(A=1,B='x',C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  Description       =   "'"//ProcName//"': 1d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1 )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName[A=1,B='x',C=78]"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  FctSep            =   "["
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1,B='x',C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  FctSep            =   "{"
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName@A=1,B='x',C=78@"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  FctSep            =   "@"
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName(A=1;B='x';C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  ArgSep            =   ";"
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1&B='x'&C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  FctSep            =   "{"
  ArgSep            =   "&"
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  FctSep='"//FctSep//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, FctSep=FctSep, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1 B='x' C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg1      =   [ character(5) :: "A=1","B='x'","C=78"]
  FctSep            =   "{"
  ArgSep            =   " "
  Description       =   "'"//ProcName//"': 1d String='"//String//"'  FctSep='"//FctSep//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg1, FctSep=FctSep, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg1), size(FoundArg1), Description//": Wrong value for number of arguments. " )
  @assertEqual( len(ExpectedArg1), len(FoundArg1), Description//": Wrong length of arguments. " )
  do i = 1,size(ExpectedArg1)
    @assertEqual( ExpectedArg1(i), FoundArg1(i), Description//": Wrong value between Expected/Found value for argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
! =============================================================================
! =============================================================================
  String            =   "FunctionName(A=1,B='x',C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  Description       =   "'"//ProcName//"': 2d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2 )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName(A=1,B='x',C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  Description       =   "'"//ProcName//"': 2d String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2 )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName[A=1,B='x',C=78]"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "["
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//""
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1,B='x',C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "{"
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName@A=1,B='x',C=78 @"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "@"
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName(A=1;B='x';C=78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  ArgSep            =   ";"
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1 & B='x' & C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "{"
  ArgSep            =   "&"
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName{A=1   B='x'   C=78}"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "{"
  ArgSep            =   " "
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//"'  ArgSep='"//ArgSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep, ArgSep=ArgSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "FunctionName( A:1;B:'x';C:78)"
  ExpectedName      =   "FunctionName"
  ExpectedArg2      =   reshape( [ character(2) :: "A","1","B","x","C","78"],[2,3] )
  FctSep            =   "("
  ArgSep            =   ";"
  ValSep            =   ":"
  Description       =   "'"//ProcName//"': 2d String='"//String//"'  FctSep='"//FctSep//"'  ArgSep='"//ArgSep//"'  ValSep='"//ValSep//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call ParseFunction( String, FoundName, FoundArg2, FctSep=FctSep, ArgSep=ArgSep, ValSep=ValSep )
  @assertEqual( ExpectedName, FoundName, Description//": Wrong value for function name. " )
  @assertEqual( size(ExpectedArg2,1), size(FoundArg2,1), Description//": Wrong value for argument size for rank-1 (Should always be 2). " )
  @assertEqual( size(ExpectedArg2,2), size(FoundArg2,2), Description//": Wrong value for argument size for rank-2 (Number of arguments). " )
  @assertEqual( len(ExpectedArg2), len(FoundArg2), Description//": Wrong length of arguments. " )
  do j = 1,size(ExpectedArg2,2)
    @assertEqual( ExpectedArg2(1,j), FoundArg2(1,j), Description//": Wrong value between Expected/Found value for name of argument "//s(i)//". " )
    @assertEqual( ExpectedArg2(2,j), FoundArg2(2,j), Description//": Wrong value between Expected/Found value for value of argument "//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

@test
Subroutine test_IsFunction()
  use String_Library   ,only:  IsFunction
  character(*)                                              ,parameter  ::  ProcName='IsFunction'
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  String, Name, FctSep
  logical                                                               ::  Expected, Found
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
  String            =   "Fct(A=1)"
  Expected          =   .True.
  Description       =   "'"//ProcName//"' String='"//String//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found   =   IsFunction( String )
  @assertEqual( Expected, Found, Description//": Wrong value. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "Fct(A=1)"
  Name              =   "Fct"
  Expected          =   .True.
  Description       =   "'"//ProcName//"' String='"//String//"' "//"' Name='"//Name//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found   =   IsFunction( String, Name=Name )
  @assertEqual( Expected, Found, Description//": Wrong value. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  String            =   "Fct(A=1)"
  Name              =   "XXX"
  Expected          =   .False.
  Description       =   "'"//ProcName//"' String='"//String//"' "//"' Name='"//Name//"'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found   =   IsFunction( String, Name=Name )
  @assertEqual( Expected, Found, Description//": Wrong value. " )
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