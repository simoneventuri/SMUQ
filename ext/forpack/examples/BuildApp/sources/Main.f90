Program Main

  use Logger_Class                ,only:  Logger, LogLevel_INFO, LogLevel_DEBUG
  use FitModelTest_Module         ,only:  Test_Constant, Test_Linear, Test_Quadratic, Test_Arrhenius, Test_NASA9

  implicit none

  character(*)                                              ,parameter  ::  ProcName = 'Main'
  integer                                                   ,parameter  ::  LogLevel = LogLevel_INFO

  call Logger%Initialize( FileName = "Fitter.log" )
  call Logger%Entering( ProcName )

!   call Logger%Write( "Calling Test_Constant", NewLine = .True.)
!   call Test_Constant( LogLevel=LogLevel )
!
  call Logger%Write( "Calling Test_Linear", NewLine = .True.)
  call Test_Linear( LogLevel=LogLevel )
!
!   call Logger%Write( "Calling Test_Quadratic", NewLine = .True.)
!   call Test_Quadratic( LogLevel=LogLevel )
!
!   call Logger%Write( "Calling Test_Arrhenius", NewLine = .True.)
!   call Test_Arrhenius( LogLevel=LogLevel )
!
!   call Logger%Write( "Calling Test_NASA9", NewLine = .True.)
!   call Test_NASA9( LogLevel=LogLevel )

  call Logger%Exiting()

End Program
