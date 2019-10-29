SubModule(EnvironmentVariable_Module) EnvironmentVariable_SubModule

  implicit none

  contains

Module Procedure GetEnvVarValue
  use Error_Class               ,only:  Error
  integer                                                               ::  Length                          ! Length of a environment variable
  integer                                                               ::  Status_                         ! Local status indicator
  character(:)  ,allocatable                                            ::  ErrMsg_                         ! Local error message
  character(*)                                              ,parameter  ::  ProcName='GetEnvVarValue'       ! Name of current procedure
  call Get_Environment_Variable( Name=Name, Length=Length )                                                     ! Getting the length of the environment variable
  allocate( character(Length) :: Value )                                                                        ! Allocating the variable to the correct length
  call Get_Environment_Variable( Name=Name, Status=Status_, Value=Value )                                       ! Getting the environment variable value
  if ( Status_ /= 0 ) Value = ""
  ErrMsg_   =   GetErrorMessage( Name, Status_ )                                                                ! Setting the error message
  if ( present(Status) ) Status = Status_                                                                       ! Setting the status indicator in the output variable if present
  if ( present(ErrMsg) ) ErrMsg = ErrMsg_                                                                       ! Setting the error message in the output variable if present
  if ( ( Status_ /= 0 ) .and. ( .Not. present(Status) ) .and. ( .Not. present(ErrMsg) ) ) then                  ! If an error has occured and neither the status nor the error message are present, then ... raising an error and stopping the code
    call Error%Raise( ErrMsg_, ProcName = ProcName )
  end if
End Procedure

Module Procedure GetEnvVarLength
  use Error_Class               ,only:  Error
  integer                                                               ::  Status_                         ! Local status indicator
  character(:)  ,allocatable                                            ::  ErrMsg_                         ! Local error message
  character(*)                                              ,parameter  ::  ProcName='GetEnvVarLength'      ! Name of current procedure
  call Get_Environment_Variable( Name=Name, Length=Length, Status=Status_ )                                     ! Getting the length of the environment variable
  ErrMsg_   =   GetErrorMessage( Name, Status_ )                                                                ! Setting the error message
  if ( present(Status) ) Status = Status_                                                                       ! Setting the status indicator in the output variable if present
  if ( present(ErrMsg) ) ErrMsg = ErrMsg_                                                                       ! Setting the error message in the output variable if present
  if ( ( Status_ /= 0 ) .and. ( .Not. present(Status) ) .and. ( .Not. present(ErrMsg) ) ) then                  ! If an error has occured and neither the status nor the error message are present, then ... raising an error and stopping the code
    call Error%Raise( ErrMsg_, ProcName = ProcName )
  end if
End Procedure

Module Procedure DoesEnvVarExist
  integer                                                               ::  Status_                         ! Local status indicator
  call Get_Environment_Variable( Name=Name, Status=Status_ )                                                    ! Getting the status of the environment variable
  DoExist   =   ( Status_ == 0 )                                                                                ! Setting the existence indicator
End Procedure

Pure Function GetErrorMessage( Name, Status ) result(ErrMsg)
  character(*)                                          ,intent(in)     ::  Name                            !< Name of the environment variable
  integer                                               ,intent(in)     ::  Status                          !< Status indicator
  character(:)  ,allocatable                                            ::  ErrMsg                          !< Error message
  select case (Status)
    case ( 0);  ErrMsg = ""
    case (-1);  ErrMsg = "The environment variable '" // trim(Name) // "' is too long for the character variable which is truncated."
    case ( 1);  ErrMsg = "The environment variable '" // trim(Name) // "' does not exist."
    case ( 2);  ErrMsg = "The environment variable '" // trim(Name) // "' cannot be determined because current OS does not support environment variables."
    case (3:);  ErrMsg = "An error occured while extracting the environment variable '" // trim(Name) // "'."
  end select
End Function

End SubModule