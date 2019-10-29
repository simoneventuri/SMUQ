Module EnvironmentVariable_Class

  use EnvVar_Class                ,only:  EnvVar_Type
  use EnvironmentVariable_Module  ,only:  GetEnvVarValue, GetEnvVarLength, DoesEnvVarExist

  implicit none

  private
  public  ::  EnvironmentVariable_Type

  Type                              ::  EnvironmentVariable_Type
    private
    type(EnvVar_Type) ,dimension(:) ,allocatable  ::  Items
    integer                                       ::  NItems  = 0
  contains
    private
    procedure   ,public         ::  Initialize  =>    InitializeEnvironmentVariable
    procedure   ,public         ::  Add         =>    AddEnvironmentVariable
    procedure   ,public         ::  Get         =>    GetEnvironmentVariable
    procedure   ,public ,nopass ::  GetValue    =>    GetEnvVarValue
    procedure   ,public ,nopass ::  GetLength   =>    GetEnvVarLength
    procedure   ,public ,nopass ::  Exist       =>    DoesEnvVarExist
  End Type

  Interface
    Module Subroutine InitializeEnvironmentVariable( This, Names, Descriptions )
      class(EnvironmentVariable_Type)                       ,intent(out)    ::  This
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Names
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Descriptions
    End Subroutine
    Module Subroutine AddEnvironmentVariable( This, Name, Description )
      class(EnvironmentVariable_Type)                       ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                ,optional ,intent(in)     ::  Description
    End Subroutine
    Pure Module Subroutine GetEnvironmentVariable( This, Name, Value, Description, Mandatory )
      class(EnvironmentVariable_Type)                       ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Value                           !< Value of the input environment variable
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Description
      logical                                     ,optional ,intent(in)     ::  Mandatory
    End Subroutine
  End Interface

End Module
