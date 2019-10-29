Module Test_InputReader

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use Input_Library     ,only:  InputReader_Type, InputParameter_Type
  use String_Library    ,only:  SetLength

  implicit none

  integer                                                   ,parameter  ::  NPad = 100

  contains

@test
Subroutine testInputParser_Initialization()
  character(*)                                                ,parameter  ::  ProcName="Input%Read"
  type(InputReader_Type)                                                  ::  Input
  character(:)  ,allocatable                                              ::  Description

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

  call Input%Read(                  &
    [ character(100) ::             &
        "Start(S)               " , &
        "  P1       =   Value   " , &
        "  P2       =   Value   " , &
        "  P3       =   Value   " , &
        "  Start(S1)            " , &
        "    P1     =   Value   " , &
        "  End(S1)              " , &
        "  Start(S2)            " , &
        "    P2     =   Value   " , &
        "    Start(S2a)         " , &
        "      P1   =   Value   " , &
        "      P2   =   Value   " , &
        "    End(S2a)           " , &
        "    Start(S2b)         " , &
        "      P1   =   Value   " , &
        "      P1   =   Value   " , &
        "    End(S2b)           " , &
        "  End(S2)              " , &
        "End(S)                 "   &
    ]                               &
    , LogLevel = 0                  &
  )

! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Mandatory'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue( .Not. Input%Mandatory        , Description//": Input%Mandatory should be False." )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Empty'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue( .Not. Input%Empty            , Description//": Input%Empty should be False." )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Defined'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue( Input%Defined                , Description//": Input%Defined should be True." )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Parameters'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue( allocated(Input%Parameters)  , Description//": Input%Parameters should be allocated" )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Sections'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertTrue( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(Input%Sections)    , Description//": Input%Sections should be allocated" )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%NSections'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Input%NSections,    2       , Description//": Wrong number of sections found." )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%NParameters'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Input%NParameters,  3       , Description//": Wrong number of parameters found." )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%GetName'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  @assertEqual( Input%GetName(),    "S"     , Description//": Wrong section name" )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Defined' for 2 param. / 0 sections"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%Read(          &
        [ character(100) :: &
          "Start(S)"      , &
          " A = 1 "       , &
          " B = 1 "       , &
          "End(S)"          &
        ], LogLevel=0 )
  @assertTrue( Input%Defined , Description//": Wrong value for 'Input%Defined'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Defined' for 1 param. / 0 sections"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%Read(          &
        [ character(100) :: &
          "Start(S)"      , &
          " A = 1 "       , &
          "End(S)"          &
        ], LogLevel=0 )
  @assertTrue( Input%Defined , Description//": Wrong value for 'Input%Defined'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'"//ProcName//"': Checking 'Section%Defined' for 0 param. / 0 sections"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Input%Read(          &
        [ character(100) :: &
          "Start(S)"      , &
          "End(S)"          &
        ], LogLevel=0 )
  @assertTrue( Input%Defined , Description//": Wrong value for 'Input%Defined'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
End Subroutine

End Module