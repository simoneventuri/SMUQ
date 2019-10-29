SubModule(EnvVar_Class) EnvVar_SubClass

  implicit none

  contains

Module Procedure InitializeEnvVar
  use EnvironmentVariable_Module  ,only:  GetEnvVarValue
  integer                                                               ::  Status_                         ! Local status indicator
  This%Name           =   trim(Name)
  call GetEnvVarValue( This%Name, This%Value, Status_ )
  This%Exist          =   Status_ /= 0
  if ( present(Description) ) then
    This%Description  =   trim(Description)
  else
    This%Description  =   ""
  end if
End Procedure

End SubModule