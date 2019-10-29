Module EnvVar_Class

  implicit none

  private
  public  ::  EnvVar_Type

  Type                              ::  EnvVar_Type
    logical                         ::  Exist = .False.
    character(:)  ,allocatable      ::  Name
    character(:)  ,allocatable      ::  Value
    character(:)  ,allocatable      ::  Description
  contains
    private
    procedure   ,public         ::  Initialize  =>    InitializeEnvVar
  End Type

  Interface
    Module Subroutine InitializeEnvVar( This, Name, Description )
      class(EnvVar_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                ,optional ,intent(in)     ::  Description
    End Subroutine
  End Interface

End Module
