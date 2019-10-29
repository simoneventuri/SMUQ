SubModule(Version_Class) Version_SubClass

  implicit none

  contains

Module Procedure InitializeVersion
  if ( present(VersionString) ) This%String = VersionString
  if ( present(EnvVar) ) then
    This%String   =   EnvVar ! @TODO
  end if

End Procedure

End SubModule
