Module Test_InputReader_VariableSubstitution

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use Input_Library     ,only:  InputReader_Type
  use String_Library    ,only:  SetLength

  implicit none

  integer                                                   ,parameter  ::  NPad = 100

  contains

@test
Subroutine test_SimpleVariableSubstitution()
  character(*)                                                ,parameter  ::  ProcName="Input%Read"
  type(InputReader_Type)                                                  ::  Input
  logical                                                                 ::  FoundParam
  character(:)  ,allocatable                                              ::  Description
  character(:)  ,allocatable                                              ::  Expected, Found
  call Logger%Write( "Testing macros: simple variable substitution", NewLine=.True. )
  call Input%Read(                &
    [ character(50) ::            &
        "Start(MainSection)   " , &
        "  dum  =   0         " , &
        "  K1   = qwerty      " , &
        "  K2   = azerty      " , &
        "  P1   = $[K1]       " , & ! P1 = qwerty
        "  P2   = X$[K2]Y     " , & ! P2 = XazertyY
        "  P3   = $[K1]-$[K2] " , & ! P3 = qwerty-azerty
        "End(MainSection)     "   &
    ]                             )
! =============================================================================
  Description =   "Checking simple substitution"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P1", Found=FoundParam )
  Expected    =   "qwerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found,    Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution with macro in-between strings"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P2", Found=FoundParam )
  Expected    =   "XazertyY"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found,    Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution with multiple macro on single line"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P3", Found=FoundParam )
  Expected    =   "qwerty-azerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found,    Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine


@test
Subroutine test_MacroRedefinition()
  type(InputReader_Type)                                                  ::  Input
  logical                                                                 ::  FoundParam
  character(:)  ,allocatable                                              ::  Description
  character(:)  ,allocatable                                              ::  Expected, Found
  call Logger%Write( "Testing macros: macro redefinition in sub-sections", NewLine=.True. )
  call Input%Read(                  &
    [ character(50) ::              &
        "Start(MainSection)     " , &
        "  K1   =   qwerty      " , &
        "  K2   =   azerty      " , &
        "  P1   =   $[K1]       " , & ! P1 = qwerty
        "  P2   =   $[K2]       " , & ! P2 = azerty
        "  Start(Subsection)    " , &
        "    K2 =   abcdef      " , &
        "    P1 =   $[K1]       " , & ! P1 = qwerty
        "    P2 =   $[K2]       " , & ! P2 = abcdef
        "  End(Subsection)      " , &
        "  P3   =   $[K1]       " , & ! P3 = qwerty
        "  P4   =   $[K2]       " , & ! P4 = azerty
        "End(MainSection)       "   &
    ]                               )
! =============================================================================
  Description =   "Checking substitution for param. declared before subsec. w/o macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P1", Found=FoundParam )
  Expected    =   "qwerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for param. declared before subsec. with macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P2", Found=FoundParam )
  Expected    =   "azerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for param. declared in subsec. w/o macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P1", SectionName="Subsection", Found=FoundParam )
  Expected    =   "qwerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for param. declared in subsec. with macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P2", SectionName="Subsection", Found=FoundParam )
  Expected    =   "abcdef"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for param. declared after subsec. w/o macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P3", Found=FoundParam )
  Expected    =   "qwerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found,    Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for param. declared after subsec. with macro redefinition"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P4", Found=FoundParam )
  Expected    =   "azerty"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found,    Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

@test
Subroutine test_MultipleVariableSubstitutionSameLine()
  type(InputReader_Type)                                                  ::  Input
  logical                                                                 ::  FoundParam
  character(:)  ,allocatable                                              ::  Description
  character(:)  ,allocatable                                              ::  Expected, Found
  call Logger%Write( "Testing macros: nested macro", NewLine=.True. )
  call Input%Read(                  &
    [ character(50) ::              &
        "Start(MainSection)     " , &
        "  K1   = VAR           " , &
        "  VAR  = abc           " , &
        "  abc  = xyz           " , &
        "  xyz  = ok!           " , &
        "  P1   = $[$[K1]]      " , &
        "  P2   = $[$[$[K1]]]   " , &
        "  P3   = $[$[$[$[K1]]]]" , &
        "End(MainSection)       "   &
    ], LogLevel=0                  )
! =============================================================================
  Description =   "Checking substitution for nested macro (x2)"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P1", Found=FoundParam )
  Expected    =   "abc"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for nested macro (x3)"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P2", Found=FoundParam )
  Expected    =   "xyz"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "Checking substitution for nested macro (x4)"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%GetValue( Found, "P3", Found=FoundParam )
  Expected    =   "ok!"
  @assertEqual( .True., FoundParam, Description//": Wrong value for 'Found' argument. " )
  @assertEqual( Expected, Found   , Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module