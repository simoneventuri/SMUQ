Module CommandLineArgument_Class

  implicit none

  private
  public  ::  CommandLineArgument_Type

  Type                          ::  CommandLineArgument_Type
    character(:)  ,allocatable  ::  Raw
    character(:)  ,allocatable  ::  Name
    character(:)  ,allocatable  ::  Value
  contains
    procedure   ,public   ::  Initialize  =>  InitializeCommandLineArgument
    procedure   ,public   ::  GetName     =>  GetCommandLineArgumentName
    procedure   ,public   ::  GetValue    =>  GetCommandLineArgumentValue
  End Type

  Interface
    Module Subroutine InitializeCommandLineArgument( This, String )
      class(CommandLineArgument_Type)                       ,intent(out)    ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  String                          !< Character string corresponding a single command-line argument
    End Subroutine
    Module Pure Function GetCommandLineArgumentName( This ) result(Name)
      class(CommandLineArgument_Type)                       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Name
    End Function
    Module Pure Function GetCommandLineArgumentValue( This ) result(Value)
      class(CommandLineArgument_Type)                       ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Value
    End Function
  End Interface

End Module