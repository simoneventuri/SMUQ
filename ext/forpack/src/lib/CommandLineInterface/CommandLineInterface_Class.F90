Module CommandLineInterface_Class

  use CommandLineArgument_Class   ,only:  CommandLineArgument_Type

  implicit none

  private
  public  ::  CommandLineInterface_Type
!   public  ::  Get_Command_Line
!   public  ::  Get_Command_Args

  Type  ::  CommandLineInterface_Type
    integer                                       ::  NArg    =   0                                           !< Number of arguments present in the command line when executing the code
    character(:)  ,allocatable                    ::  Command_Line                                            !< Character string corresponding to the full command line by which the program was invoked.
    type(CommandLineArgument_Type)  ,allocatable  ::  Arg(:)
  contains
    procedure   ,public     ::  Initialize      =>  InitializeCommandLineInterface
    generic     ,public     ::  GetArguments    =>  GetArgumentsList, GetArgumentsItem, GetArgumentNameValue
    generic     ,public     ::  ArgumentExists  =>  ArgumentExistsFromIndex, ArgumentExistsFromName
    generic     ,public     ::  Write           =>  WriteCommandLineInterfaceToUnit, WriteCommandLineInterfaceToString
    procedure   ,public     ::  GetValue        =>  GetValueFromName
    procedure   ,private    ::  GetArgumentsList
    procedure   ,private    ::  GetArgumentsItem
    procedure   ,private    ::  GetArgumentNameValue
    procedure   ,private    ::  ArgumentExistsFromIndex
    procedure   ,private    ::  ArgumentExistsFromName
    procedure   ,private    ::  WriteCommandLineInterfaceToUnit
    procedure   ,private    ::  WriteCommandLineInterfaceToString
  End Type

  Interface
    Module Subroutine InitializeCommandLineInterface( This, Debug )
      class(CommandLineInterface_Type)                      ,intent(out)    ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine


    ! **************************************************************************************************************
    !                             PROCEDURES TO EXTRACT PARAMETERS OF AN ARGUMENT
    ! **************************************************************************************************************

    Module Subroutine GetArgumentsList( This, Arguments )
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                            ,intent(out)    ::  Arguments(:)                    !< Vector of character string corresponding the command-line arguments
    End Subroutine

    Module Subroutine GetArgumentsItem( This, iArg, Argument )
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  iArg                            !< Index of the argument to be extracted
      character(:)  ,allocatable                            ,intent(out)    ::  Argument                        !<
    End Subroutine

    Module Subroutine GetArgumentNameValue( This, iArg, Name, Value )
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  iArg                            !< Index of the argument to be extracted
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Value
    End Subroutine



    Module Function GetValueFromName( This, Name, DefaultValue, CaseSensitive, Found ) result(Value)
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                ,optional ,intent(in)     ::  DefaultValue
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                     ,optional ,intent(out)    ::  Found
      character(:)  ,allocatable                                            ::  Value
    End Function

    ! **************************************************************************************************************
    !                             PROCEDURES TO CHECK FOR THE EXISTENCE OF AN ARGUMENT
    ! **************************************************************************************************************

    Module Pure Function ArgumentExistsFromIndex( This, iArg ) result(Exist)
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  iArg                            !< Index of the argument to be tested for exitence
      logical                                                               ::  Exist                           !< Indicator whether current argument exists
    End Function

    Module Pure Function ArgumentExistsFromName( This, Name ) result(Exist)
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the argument to be tested for exitence
      logical                                                               ::  Exist                           !< Indicator whether current argument exists
    End Function


    ! **************************************************************************************************************
    !                             PROCEDURES TO WRITE PARAMETERS OF AN ARGUMENT
    ! **************************************************************************************************************

    Module Subroutine WriteCommandLineInterfaceToUnit( This, Unit )
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  Unit                            !< File unit number where to write
    End Subroutine

    Module Subroutine WriteCommandLineInterfaceToString( This, Lines )
      class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable  ,dimension(:)             ,intent(out)    ::  Lines                           !< Character array where to write the Section object
    End Subroutine

  End Interface

End Module