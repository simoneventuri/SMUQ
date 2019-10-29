Program Main

  use Test_Module

  implicit none

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_WriteLogger')")
  call Test_WriteLogger

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_WriteOptions')")
  call Test_WriteOptions

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_NotInitialized')")
  call Test_NotInitialized

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_DebugMode')")
  call Test_DebugMode

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_ProcToLog')")
  call Test_ProcToLog

  write(*,"(//,'>>>>>>>>>>>>>>>>> Test_LogLevelPropagation')")
  call Test_LogLevelPropagation

End Program
