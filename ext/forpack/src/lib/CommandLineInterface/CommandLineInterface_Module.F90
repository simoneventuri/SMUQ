Module CommandLineInterface_Module

  implicit none

  private
  public  ::  Get_Command_Line
  public  ::  Get_Command_Args
  public  ::  GetCommandLineArgumentValue

  Interface
    Module Subroutine Get_Command_Line( Command_Line )
      character(:)  ,allocatable                            ,intent(out)    ::  Command_Line
    End Subroutine

    Module Subroutine Get_Command_Args( Arguments )
      character(:)  ,allocatable                            ,intent(out)    ::  Arguments(:)                    !< Vector of character string corresponding the command-line arguments
    End Subroutine

    Module Subroutine GetCommandLineArgumentValue( Names, Value, Found, Debug )
      character(:)  ,allocatable                            ,intent(in)     ::  Names(:)
      character(:)  ,allocatable                            ,intent(out)    ::  Value
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

  End Interface

End Module