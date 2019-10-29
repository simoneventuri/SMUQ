Program Main

  use SystemCommand_Class    ,only:  SystemCommand_Type

  implicit none

  type(SystemCommand_Type)                                             ::    Command


  call Command%cp( "test.txt", '.', i_Debug=.True. )

End Program
