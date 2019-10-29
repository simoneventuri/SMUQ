Module Version_Class

  implicit none

  private
  public  ::  Version_Type

  Type    ::  Version_Type
    character(:)  ,allocatable  ::  String
    integer                     ::  Major
    integer                     ::  Minor
    integer                     ::  Patch
  contains
    procedure ::  Initialize => InitializeVersion
!     procedure ::  GetMajor = > GetMajorVersion
  End Type

  Interface
    Module Subroutine InitializeVersion( This, VersionString, EnvVar )
      class(Version_Type)                                   ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  VersionString
      character(*)                                ,optional ,intent(in)     ::  VersionString
      EnvVar
    End Subroutine
!     Module Function GetMajorVersion(This) result(Major)
!       class(Version_Type)   ::  This
!       integer               ::  Major
!     End Function
  End Interface

End Module
