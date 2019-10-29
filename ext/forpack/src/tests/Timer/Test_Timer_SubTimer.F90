module Test_Timer_SubTimer

  use pfunit_mod
  use Timer_Library     ,only:  Timer_Type
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  integer                                                   ,parameter  ::  NPad = 100
  logical                                                   ,parameter  ::  Dbg = .False.

  contains

! @Test
! Subroutine test_Timer_StartStop()
!   character(*)                                                ,parameter  ::  ProcName="Timer%AddSubTimer"
!   character(:)  ,allocatable                                              ::  Description
!   character(:)  ,allocatable                                              ::  Output(:)
!   type(Timer_Type)                                                        ::  Timer
!   call Logger%Write( "Testing 'Timer' object", NewLine=.True. )
! ! =============================================================================
!   Description =   "'Timer': Checking 'Timer%Start/Timer%Stop'"
!   call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
!   call Timer%Initialize( Name = "Temporal_Loop" )
!   call Timer%Start()
!   call DoSomething( 100 )
!   call Timer%Stop()
!   @assertEqual( 1 , 1)
!   call Logger%Write( "[ok]" )
! ! =============================================================================
! End Subroutine

@Test
Subroutine test_Timer_5SubTimer()
  character(*)                                                ,parameter  ::  ProcName="Timer%AddSubTimer"
  real(8)       ,allocatable                                              ::  SubTimerTimes(:)
  character(:)  ,allocatable                                              ::  SubTimerNames(:)
  character(:)  ,allocatable                                              ::  Description, Msg, Info
  character(:)  ,allocatable                                              ::  Output(:)
  type(Timer_Type)                                                        ::  Timer
  integer                                                                 ::  i, N
  integer                                                                 ::  iExpected, iFound
  logical                                                                 ::  lExpected, lFound
  real(8)                                                                 ::  rExpected, rFound
  real(8)                                                                 ::  tTot, tSub, trel, PercentageTol


  call Logger%Write( "Testing '"//"Timer"//"' object", NewLine=.True. )

  PercentageTol   =   5

! ! # ifdef GCC_COMPILER
! !   Msg   =   Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. "
! !   @assertEqual( iExpected, iFound, Msg )
! ! # else
!   @assertEqual( iExpected, iFound, Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. " )
! ! # endif

! =============================================================================
! =============================================================================
! =============================================================================
  SubTimerTimes   =   [ 50      , 50      ]
  SubTimerNames   =   [ "Item1" , "Item2" ]
  N               =   size(SubTimerNames)
  call Timer%Initialize( Name = "TimerName" )
  do i = 1,N
    call Timer%AddSubTimer( Name = SubTimerNames(i) )
  end do
  Info    =   "using "//s(N)//" subtimers added with 'AddSubTimer'"
! =============================================================================
  Description =   "'Timer': Checking 'GetNumberSubTimers()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  iExpected   =   N
  iFound      =   Timer%GetNumberSubTimers()
  @assertEqual( iExpected, iFound, Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'IsValidSubTimerIndex()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  do i = 1,N+1
    lExpected   =   i <= N
    lFound      =   Timer%IsValidSubTimerIndex(i)
    @assertEqual( lExpected, lFound, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'GetElapsedTime()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Timer%Start()
  do i = 1,N
    call Timer%NextSubTimer()
    call DoSomething( SubTimerTimes(i) )
  end do
  call Timer%Stop()
  tTot    =   Timer%GetElapsedTime()
  do i = 1,Timer%GetNumberSubTimers()
    rExpected   =   SubTimerTimes(i)
    rFound      =   Timer%GetElapsedTime(i) / tTot * 100
    @assertEqual( rExpected ,rFound , PercentageTol, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================


! =============================================================================
! =============================================================================
! =============================================================================
  SubTimerTimes   =   [ 33.3    , 33.3    , 33.4    ]
  SubTimerNames   =   [ "Item1" , "Item2" , "Item3" ]
  N               =   size(SubTimerNames)
  call Timer%Initialize( Name = "TimerName" )
  do i = 1,N
    call Timer%AddSubTimer( Name = SubTimerNames(i) )
  end do
  Info    =   "using "//s(N)//" subtimers added with 'AddSubTimer'"
! =============================================================================
  Description =   "'Timer': Checking 'GetNumberSubTimers()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  iExpected   =   N
  iFound      =   Timer%GetNumberSubTimers()
  @assertEqual( iExpected, iFound, Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'IsValidSubTimerIndex()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  do i = 1,N+1
    lExpected   =   i <= N
    lFound      =   Timer%IsValidSubTimerIndex(i)
    @assertEqual( lExpected, lFound, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'GetElapsedTime()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Timer%Start()
  do i = 1,N
    call Timer%NextSubTimer()
    call DoSomething( SubTimerTimes(i) )
  end do
  call Timer%Stop()
  tTot    =   Timer%GetElapsedTime()
  do i = 1,Timer%GetNumberSubTimers()
    rExpected   =   SubTimerTimes(i)
    rFound      =   Timer%GetElapsedTime(i) / tTot * 100
    @assertEqual( rExpected ,rFound , PercentageTol, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================



! =============================================================================
! =============================================================================
! =============================================================================
  SubTimerTimes   =   [ 10.0    , 20.0    , 30.0    , 40.0    ]
  SubTimerNames   =   [ "Item1" , "Item2" , "Item3" , "Item4" ]
  N               =   size(SubTimerNames)
  call Timer%Initialize( Name = "TimerName" )
  do i = 1,N
    call Timer%AddSubTimer( Name = SubTimerNames(i) )
  end do
  Info    =   "using "//s(N)//" subtimers added with 'AddSubTimer'"
! =============================================================================
  Description =   "'Timer': Checking 'GetNumberSubTimers()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  iExpected   =   N
  iFound      =   Timer%GetNumberSubTimers()
  @assertEqual( iExpected, iFound, Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'IsValidSubTimerIndex()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  do i = 1,N+1
    lExpected   =   i <= N
    lFound      =   Timer%IsValidSubTimerIndex(i)
    @assertEqual( lExpected, lFound, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'GetElapsedTime()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Timer%Start()
  do i = 1,N
    call Timer%NextSubTimer()
    call DoSomething( SubTimerTimes(i) )
  end do
  call Timer%Stop()
  tTot    =   Timer%GetElapsedTime()
  do i = 1,Timer%GetNumberSubTimers()
    rExpected   =   SubTimerTimes(i)
    rFound      =   Timer%GetElapsedTime(i) / tTot * 100
    @assertEqual( rExpected ,rFound , PercentageTol, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================


! =============================================================================
! =============================================================================
! =============================================================================
  SubTimerTimes   =   [ 10      , 15      , 15      , 20      , 40      ]
  SubTimerNames   =   [ "Item1" , "Item2" , "Item3" , "Item4" , "Item5" ]
  N               =   size(SubTimerNames)
  call Timer%Initialize( Name = "TimerName" )
  do i = 1,N
    call Timer%AddSubTimer( Name = SubTimerNames(i) )
  end do
  Info    =   "using "//s(N)//" subtimers added with 'AddSubTimer'"
! =============================================================================
  Description =   "'Timer': Checking 'GetNumberSubTimers()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  iExpected   =   N
  iFound      =   Timer%GetNumberSubTimers()
  @assertEqual( iExpected, iFound, Description//": Wrong number of sub-timers 'Timer%GetNumberSubTimers()'. " )
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'IsValidSubTimerIndex()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  do i = 1,N+1
    lExpected   =   i <= N
    lFound      =   Timer%IsValidSubTimerIndex(i)
    @assertEqual( lExpected, lFound, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
  call Logger%Write( "[ok]" )
! =============================================================================
  Description =   "'Timer': Checking 'GetElapsedTime()' "//Info
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  call Timer%Start()
  do i = 1,N
    call Timer%NextSubTimer()
    call DoSomething( SubTimerTimes(i) )
  end do
  call Timer%Stop()
  tTot    =   Timer%GetElapsedTime()
  do i = 1,Timer%GetNumberSubTimers()
    rExpected   =   SubTimerTimes(i)
    rFound      =   Timer%GetElapsedTime(i) / tTot * 100
    @assertEqual( rExpected ,rFound , PercentageTol, Description//": Wrong result for 'Timer%IsValidSubTimerIndex(i)' for i="//s(i)//". " )
  end do
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

Subroutine DoSomething( PerTime )
  real(8)                                               ,intent(in)     ::  PerTime
  integer                                                   ,parameter  ::  Ntot = 1e6
  integer                                                               ::  N, i
  real(8)                                                               ::  Output
  N   =   floor( real(Ntot * PerTime) / 100 )
  do i = 1,N
    call Computation( i, Output )
  end do
!   contains
End Subroutine
Subroutine Computation( i, Output )
  integer                                               ,intent(in)     ::  i
  real(8)                                               ,intent(out)    ::  Output
  real(8)                                                               ::  T, a, b, c
  a       =   123.456 * i
  b       =   0.98765 * i
  c       =   3.45678 / i
  T       =   real(i,kind=8)
  Output  =   a * T * T**b * exp(-c/T)
End Subroutine

End Module