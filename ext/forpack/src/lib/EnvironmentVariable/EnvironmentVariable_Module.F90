Module EnvironmentVariable_Module

  implicit none

  private
  public  ::  GetEnvVarValue
  public  ::  GetEnvVarLength
  public  ::  DoesEnvVarExist

  Interface
    Module Subroutine GetEnvVarValue( Name, Value, Status, ErrMsg )
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable
      character(:)          ,allocatable                    ,intent(out)    ::  Value                           !< Value of the input environment variable
      integer                                     ,optional ,intent(out)    ::  Status                          !< Status indicator
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg                          !< Error message
    End Subroutine
    Module Subroutine GetEnvVarLength( Name, Length, Status, ErrMsg )
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable
      integer                                               ,intent(out)    ::  Length                          !< Length of the input environment variable
      integer                                     ,optional ,intent(out)    ::  Status                          !< Status indicator
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg                          !< Error message
    End Subroutine
    Module Function DoesEnvVarExist( Name ) result(DoExist)
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable
      logical                                                               ::  DoExist
    End Function
  End Interface

End Module
