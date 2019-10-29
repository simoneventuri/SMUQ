Module CommandLineInterface_Library

  use CommandLineInterface_Module   ,only:  Get_Command_Args, GetCommandLineArgumentValue
  use CommandLineArgument_Class     ,only:  CommandLineArgument_Type
  use CommandLineInterface_Class    ,only:  CommandLineInterface_Type

  implicit none

  private
  public  ::  Get_Command_Args
  public  ::  GetCommandLineArgumentValue
  public  ::  CommandLineArgument_Type
  public  ::  CommandLineInterface_Type

End Module