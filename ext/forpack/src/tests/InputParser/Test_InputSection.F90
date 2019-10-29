Module Test_InputSection

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use Input_Library     ,only:  InputReader_Type, InputSection_Type, InputParameter_Type
  use String_Library    ,only:  SetLength

  implicit none

  integer                                                   ,parameter  ::  NPad = 100

  contains

@test
Subroutine testInputSection_GetFunctionSection()
  use String_Library    ,only:  Convert_To_String
  character(*)                                                ,parameter  ::  ProcName="GetFunctionSection"
  integer                                                                 ::  i
  type(InputReader_Type)                                                  ::  Input
  type(InputSection_Type)                                                 ::  Section
  type(InputParameter_Type)                                               ::  Param
  character(:)  ,allocatable                                              ::  Description, Si, String
  integer                                                                 ::  ExpectedInteger, FoundInteger
  character(:)  ,allocatable                                              ::  ExpectedCharacter, FoundCharacter
  character(:)  ,allocatable  ,dimension(:)                               ::  ExpectedArgNames, FoundChar1d
  call Logger%Write( "Testing 'Input%"//ProcName//"' procedure", NewLine=.True. )
  call Input%Read(                                                            &
    [ character(100) ::                                                       &
      "Start(Main)                                                        " , &
      "  FunctionA( IntVar=1, RealVar=1.0, LogicalVar=T, CharVar=Hello )  " , &
      "  Start(Sub)                                                       " , &
      "    FunctionA( IntVar=2, RealVar=2.0, LogicalVar=F, CharVar=toto ) " , &
      "  End(Sub)                                                         " , &
      "End(Main)                                                          "   &
    ]                                                                         &
    , LogLevel = 0                                                            &
  )
  ExpectedArgNames    =   [ Character(100) :: "IntVar", "RealVar", "LogicalVar", "CharVar" ]
  Section   =   Input%GetFunctionSection( "FunctionA" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Defined'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue(  Section%Defined     , Description//": Wrong value for 'Defined'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Mandatory'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertFalse( Section%Mandatory   , Description//": Wrong value for 'Mandatory'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Empty'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertFalse( Section%Empty       , Description//": Wrong value for 'Empty'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%GetName()'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  ExpectedCharacter =   "FunctionA"
  FoundCharacter    =   Section%GetName()
  @assertEqual( ExpectedCharacter, FoundCharacter, Description//": Wrong value for function name. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%GetNumberOfSubSections()'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  ExpectedInteger   =   0
  FoundInteger      =   Section%GetNumberOfSubSections()
  @assertEqual( ExpectedInteger, FoundInteger, Description//": Wrong value for number of sub-sections. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%GetNumberOfParameters()'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  ExpectedInteger   =   size(ExpectedArgNames)
  FoundInteger      =   Section%GetNumberOfParameters()
  @assertEqual( ExpectedInteger, FoundInteger, Description//": Wrong value for number of parameters. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  do i = 1,Section%GetNumberOfParameters()
    Si                  =   Convert_To_String(i)
    Param               =   Section%GetParameter(i)
    ExpectedCharacter   =   trim( ExpectedArgNames(i) )
    FoundCharacter      =   Param%GetName()
    Description =   "'"//ProcName//"': Checking name of parameter "//Si
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    @assertEqual( ExpectedCharacter, FoundCharacter, Description//": Wrong value for name of argument "//Si//". " )
    call Logger%Write( "[ok]" )
  end do
! =============================================================================
End Subroutine

End Module