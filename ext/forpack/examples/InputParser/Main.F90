Module InputParserExample_Module


  use Input_Library     ,only:  InputReader_Type
  use Logger_Class      ,only:  Logger

  implicit none

  public

  contains

Subroutine GetValueR1d

  character(*)  ,parameter              ::  ProcName='GetValueR1d'
  type(InputReader_Type)                ::  Input
  character(:)  ,allocatable            ::  FileName
  real(8)   ,dimension(:) ,allocatable  ::  VarR1

  call Logger%Entering( ProcName )

  FileName      =   'input.inp'
  call Logger%Write( "Calling Input%Read: FileName = ", FileName )
  call Input%Read( FileName=FileName )

  call Logger%Write( "Calling: call Input%GetValue( VarR1, 'V1' )" )
  call Input%GetValue( VarR1, 'V1' )
  call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  call Logger%Write( "Calling: call Input%GetValue( VarR1, 'V2', Separator=',' )" )
  call Input%GetValue( VarR1, 'V2', Separator=',' )
  call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  call Logger%Write( "Calling: call Input%GetValue( VarR1, 'V1', IncreasingOrder=.True. )" )
  call Input%GetValue( VarR1, 'V1', IncreasingOrder=.True. )
  call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  call Logger%Exiting()

End Subroutine

! Subroutine Example_Old
!   type(InputReader_Type)            ::  Kinetic1, Kinetic2, Kinetics(1)
!   type(InputSection_Type)           ::  Section, Reactions
!   character(:)  ,allocatable        ::  String
!   call Kinetics(1)%Read( FileName='Air11-Park2001.kin' )
!   call Kinetics(1)%GetValue( String, "Name", Mandatory=.True. )
!   Kinetics(1)%Name  =   String
!   Kinetic1%Name     =   Kinetics(1)%Name
!   Kinetic2          =   Kinetics(1)
!   Reactions         =   Kinetic2%GetSection( SectionName="Reactions" )
!   if ( Reactions%Defined ) then
!     call Kinetic1%Add_Section( Reactions, Action="MERGE", i_Debug=.True. )
!   end if
!   Reactions       =   Kinetic1%GetSection( SectionName="Reactions" )
! End Subroutine

! Subroutine Example_Old
!
!   use Input_Library   ,only:  InputReader_Type, InputSection_Type
!   use Logger_Class    ,only:  Logger
!
!   implicit none
!
!   type(InputReader_Type)      ::  Input
!   type(InputSection_Type)     ::  Section, Section2
!   real(8)                     ::  R0d
!
!   call Input%Read( FileName="new_input.dat" )
!
! !   call Logger%Write( "Outputing input file" )
! !   call Logger%Write( "-> Calling Input%Output" )
! !   call Input%Output( Logger%Unit )  ! Use "Logger%GetUnit()" in new version
! !   call Logger%Write( "-> Done outputing input file" )
!
!   call Logger%Write( "-> Calling Input%Output" )
!   call Input%GetValue( R0d, "mu", SectionName="analysis_inputs>param_inputs>param3>param3_distribution_inputs" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Extracting value from Input object", NewLine=.True. )
!   call Logger%Write( "==================================")
!
!   call Logger%Write( "Method 1: Direclty extract the parameter value from the Input object (Recommended)", NewLine=.True. )
!   call Logger%Write( "-> Calling Input%Output" )
!   call Input%GetValue( R0d, "mu", SectionName="analysis_inputs>param_inputs>param3>param3_distribution_inputs" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Method 2: Extracting the last sub-section from the Input object, an then extracing the parameter value from it ", NewLine=.True. )
!   Section   =   Input%GetSection( "param3_distribution_inputs", FromSubSection="analysis_inputs>param_inputs>param3" )
!   call Section%GetValue( R0d, "mu" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Method 3: Extracting all sections last sub-section (NOT Recommended)", NewLine=.True. )
!   Section   =   Input%GetSection( "analysis_inputs" )
!   Section   =   Section%GetSection( "param_inputs" )
!   Section   =   Section%GetSection( "param3" )
!   call Section%GetValue( R0d, "mu" )
!   call Logger%Write( "-> R0d = ", R0d )
!
! End Subroutine

Subroutine WriteInput

  character(*)  ,parameter              ::  ProcName='WriteInput'
  type(InputReader_Type)                ::  Input
  character(:)  ,allocatable            ::  Lines(:), FileName
  integer                               ::  i, Unit

  call Logger%Entering( ProcName )

  call Logger%Write( "call Input%Read" )
  call Input%Read( [ character(100) :: &
        "Start(S)               "    , &
        "  P1   =   Value   "    , &
        "  P22  =   Value   "    , &
        "  P333 =   Value   "    , &
        "  Start(S1)            "    , &
        "    P1     =   Value   "    , &
        "  End(S1)              "    , &
        "End(S)                 "      &
  ] )

  Unit      =   6
  FileName  =   "FromInput.txt"

  call Logger%Write( "1. Writing an Input object to a file from a file unit number", NewLine=.True. )
  call Input%Write( Unit )

  call Logger%Write( "2. Writing an Input object to a file from a file unit number w/o alignment", NewLine=.True. )
  call Input%Write( Unit, Align=.False. )

  call Logger%Write( "3. Writing an Input object to a file from a file name", NewLine=.True. )
  call Input%Write( FileName )

  call Logger%Write( "4. Writing an Input object to a string", NewLine=.True. )
  call Input%Write( Lines )
  call Logger%Write( "-> i ", "Lines(i) = ", Lines )

!   call Logger%Write( "5. Writing an Input object to a file using user-defined IO", NewLine=.True. )
!   write(*,*) Input

  call Logger%Exiting()

End Subroutine

Subroutine TestRecursiveSectionWithSameName

  character(*)  ,parameter              ::  ProcName='TestRecursiveSectionWithSameName'
  type(InputReader_Type)                ::  Input
  character(:)  ,allocatable            ::  Lines(:)
  integer                               ::  i

  call Logger%Entering( ProcName )

  call Logger%Write( "call Input%Read" )
  call Input%Read( [ character(100) :: &
        "Start(Main)     "    , &
        "  Start(Sub)    "    , &
        "    Pa1    =   1"    , &
        "    Start(Sub)  "    , &
        "      Pb1  =   2"    , &
        "      Pb2   =  3"    , &
        "    End(Sub)    "    , &
        "    Pa1    =   4"    , &
        "  End(Sub)      "    , &
        "End(Main)       "      &
  ] )

!   write(*,*) Input


  call Logger%Exiting()

End Subroutine


Subroutine TestWriteEmptySection

  character(*)  ,parameter              ::  ProcName='TestWriteEmptySection'
  type(InputReader_Type)                ::  Input

  call Logger%Entering( ProcName )

  call Logger%Write( "call Input%Read" )
  call Input%Read( [ character(100) :: &
        "Start(Main)    " , &
        "  Start(Full)  " , &
        "    P1 = V1    " , &
        "  End(Full)    " , &
        "  Start(Empty) " , &
        "  End(Empty)   " , &
        "End(Main)      "  &
  ] )

  call Logger%Write( "call Input%Write( Unit )" )
  call Input%Write( 6 )

  call Logger%Write( "call Input%Write( Unit, ShowEmpty=.True. )" )
  call Input%Write( 6, ShowEmpty=.True. )

  call Logger%Write( "call Input%Write( Unit, ShowEmpty=.False. )" )
  call Input%Write( 6, ShowEmpty=.False. )


  call Logger%Exiting()

End Subroutine


Subroutine TestWriteIncludeExclude

  character(*)  ,parameter              ::  ProcName='TestWriteIncludeExclude'
  type(InputReader_Type)                ::  Input

  call Logger%Entering( ProcName )

  call Logger%Write( "call Input%Read" )
  call Input%Read( [ character(100) :: &
        "Start(Main)    " , &
        "  Start(A)     " , &
        "      pA = 1   " , &
        "    Start(A1)  " , &
        "      pA1 = 1  " , &
        "    End(A1)    " , &
        "    Start(A2)  " , &
        "      pA2 = 2  " , &
        "    End(A2)    " , &
        "  End(A)       " , &
        "  Start(B)     " , &
        "      pB = 1   " , &
        "    Start(B1)  " , &
        "      pB1 = 3  " , &
        "    End(B1)    " , &
        "    Start(B2)  " , &
        "      pB2 = 3  " , &
        "    End(B2)    " , &
        "  End(B)       " , &
        "End(Main)      "   &
  ] )

  call Logger%Write( "call Input%Write(6)", NewLine=.True. )
  call Input%Write(6)

  call Logger%Write( "call Input%Write(6,Include=['Main','A   ')", NewLine=.True. )
  call Input%Write(6,Include=['Main','A   '])

  call Logger%Write( "call Input%Write(6,Exclude=['A1','A2'])", NewLine=.True. )
  call Input%Write(6,Exclude=['A1','A2'])

  call Logger%Write( "call Input%Write(6,Exclude=['A1','A2','B '])", NewLine=.True. )
  call Input%Write(6,Exclude=['A1','A2','B '])

  call Logger%Write( "call Input%Write(6,Exclude=['Main'])", NewLine=.True. )
  call Input%Write(6,Exclude=['Main'])

  call Logger%Exiting()

End Subroutine



Subroutine TestWriteDepth

  character(*)  ,parameter              ::  ProcName='TestWriteDepth'
  type(InputReader_Type)                ::  Input

  call Logger%Entering( ProcName )

  call Logger%Write( "call Input%Read" )
  call Input%Read( [ character(100) :: &
        "Start(Main)    " , &
        "  Start(A)     " , &
        "      pA = 1   " , &
        "    Start(A1)  " , &
        "      pA1 = 1  " , &
        "    End(A1)    " , &
        "    Start(A2)  " , &
        "      pA2 = 2  " , &
        "    End(A2)    " , &
        "  End(A)       " , &
        "  Start(B)     " , &
        "      pB = 1   " , &
        "    Start(B1)  " , &
        "      pB1 = 3  " , &
        "    End(B1)    " , &
        "    Start(B2)  " , &
        "      pB2 = 3  " , &
        "    End(B2)    " , &
        "  End(B)       " , &
        "End(Main)      "   &
  ] )

  call Logger%Write( "call Input%Write(6)", NewLine=.True. )
  call Input%Write(6)

  call Logger%Write( "call Input%Write(6,Depth=1)", NewLine=.True. )
  call Input%Write(6,Depth=1)
!
  call Logger%Write( "call Input%Write(6,Depth=2)", NewLine=.True. )
  call Input%Write(6,Depth=2)

  call Logger%Write( "call Input%Write(6,Depth=3)", NewLine=.True. )
  call Input%Write(6,Depth=3)

  call Logger%Exiting()

End Subroutine

Subroutine TestGetFunctionSection


  use Input_Library     ,only:  InputSection_Type

  character(*)  ,parameter              ::  ProcName='TestGetFunctionSection'
  type(InputReader_Type)                ::  Input
  type(InputSection_Type)               ::  Section

  call Logger%Entering( ProcName )

  call Logger%Write( "Calling Input%Read" )
  call Input%Read(                                                              &
    [ character(100) ::                                                         &
      "Start(Solver)                                                        " , &
      "  StopCondition( Iter=1000, Residual=1e-6, Time=1.2, CPUTime=10min ) " , &
      "End(Solver)                                                          "   &
    ]                                                                           &
    , LogLevel = 0                                                              &
  )

  call Logger%Write( "Calling Input%GetFunctionSection" )
  Section   =   Input%GetFunctionSection( "StopCondition" )

!   write(*,*) Input


  call Logger%Exiting()

End Subroutine

End Module



Program Main

  use InputParserExample_Module
  use Logger_Class      ,only:  Logger

  implicit none

  call Logger%Initialize( & ! FileName, Status, Position, Procedure, Indentation, i_Force_FileName )
          Procedure   =   "Main"  )

!   call Logger%Write( "Calling GetValueR1d", NewLine=.True. )
!   call GetValueR1d()

!   call Logger%Write( "Calling WriteInput", NewLine=.True. )
!   call WriteInput()
!
!   call Logger%Write( "Calling TestRecursiveSectionWithSameName", NewLine=.True. )
!   call TestRecursiveSectionWithSameName()
!
!   call Logger%Write( "Calling TestWriteEmptySection", NewLine=.True. )
!   call TestWriteEmptySection()
!
!   call Logger%Write( "Calling TestWriteIncludeExclude", NewLine=.True. )
!   call TestWriteIncludeExclude()
!
!   call Logger%Write( "Calling TestWriteDepth", NewLine=.True. )
!   call TestWriteDepth()

  call Logger%Write( "Calling TestGetFunctionSection", NewLine=.True. )
  call TestGetFunctionSection()

End Program