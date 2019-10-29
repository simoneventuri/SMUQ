Module DirectoriesContainer_Class

  use Object_Class      ,only:  Object_Type
  use Directory_Class   ,only:  Directory_Type

  implicit none

  private
  public    ::  DirectoriesContainer_Type

  Type  ,extends(Object_Type)           ::  DirectoriesContainer_Type
!     private
    logical                             ::  Initialized = .False.
    integer                             ::  NDir    =   0
    type(Directory_Type)  ,allocatable  ::  Dirs(:)
  contains
    private
    procedure ,public   ::  Initialize    =>  InitializeDirectoriesContainer
    procedure ,public   ::  Free          =>  FreeDirectoriesContainer
    procedure ,public   ::  Add           =>  AddDirectory
    procedure ,public   ::  GetNumberDirectories
    generic   ,public   ::  GetDirectory  =>  GetDirectoryFromIndex, GetDirectoryFromKey
    procedure ,public   ::  GetDirectories=>  GetDirectoriesFromKey
    procedure ,public   ::  GetIndex      =>  GetDirectoryIndex
    procedure ,public   ::  UpdatePath    =>  UpdateDirectoryPath
    procedure ,public   ::  GetPath       =>  GetDirectoryPath
    procedure ,public   ::  GetPaths      =>  GetDirectoryPaths
    procedure ,public   ::  GetDesc       =>  GetDirectoryDesc
    procedure ,public   ::  GetLengthKey  =>  GetLengthDirKey
    procedure ,public   ::  GetSummary    =>  GetDirectoriesSummary
    procedure ,public   ::  IsDefined     =>  IsDirectoryDefined
    procedure ,public   ::  SubstituteDirectory
    procedure ,public   ::  FindFile
    procedure           ::  GetDirectoryFromIndex
    procedure           ::  GetDirectoryFromKey
  End Type

  Interface

    Module Subroutine InitializeDirectoriesContainer( This )
      class(DirectoriesContainer_Type)                      ,intent(out)    ::  This
    End Subroutine

    Pure Module Subroutine FreeDirectoriesContainer( This )
      class(DirectoriesContainer_Type)                      ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine AddDirectory( This, Directory )
      class(DirectoriesContainer_Type)                      ,intent(inout)  ::  This
      type(Directory_Type)                                  ,intent(in)     ::  Directory
    End Subroutine

    Pure Elemental Module Function GetNumberDirectories( This ) result(NDir)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      integer                                                               ::  NDir
    End Function

    Pure Module Function GetDirectoryFromIndex( This, iDir ) result(Dir)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  iDir
      type(Directory_Type)                                                  ::  Dir
    End Function

    Pure Module Function GetDirectoryFromKey( This, Key ) result(Dir)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      type(Directory_Type)                                                  ::  Dir
    End Function

    Pure Module Function GetDirectoryIndex( This, Key ) result(iDir)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      integer                                                               ::  iDir
    End Function

    Pure Module Subroutine GetDirectoriesFromKey( This, Key, ListDir )
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      type(Directory_Type)  ,allocatable                    ,intent(out)    ::  ListDir(:)
    End Subroutine

    Pure Module Subroutine UpdateDirectoryPath( This, Key, Path )
      class(DirectoriesContainer_Type)                      ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Key
      character(*)                                          ,intent(in)     ::  Path
    End Subroutine

    Pure Module Function GetDirectoryPath( This, Key, Hide ) result(Path)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      logical                                     ,optional ,intent(in)     ::  Hide
      character(:)  ,allocatable                                            ::  Path
    End Function

    Pure Module Subroutine GetDirectoryPaths( This, Key, Paths, Hide )
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      character(:)  ,allocatable                            ,intent(out)    ::  Paths(:)
      logical                                     ,optional ,intent(in)     ::  Hide
    End Subroutine

    Pure Module Function GetDirectoryDesc( This, Key ) result(Desc)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      character(:)  ,allocatable                                            ::  Desc
    End Function

    Pure Module Function GetLengthDirKey( This ) result(Length)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      integer                                                               ::  Length
    End Function

!     Pure
    Module Function GetDirectoriesSummary( This ) result(Summary)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Summary(:)
    End Function

    Pure Module Function SubstituteDirectory( This, InputString ) result(OutputString)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function

    Pure Module Function IsDirectoryDefined( This, Key ) result(Defined)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Key
      logical                                                               ::  Defined
    End Function

!     Module Subroutine FindFile( This, BaseName, FullName, Found, iDir, Dir )
!       class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
!       character(*)                                          ,intent(in)     ::  BaseName                      !< Basename of the file to be found in the directories
!       character(:)  ,allocatable                            ,intent(out)    ::  FullName                      !< Full name of the file found: Basename + Path of the first directory wherre the file is found
!       logical                                     ,optional ,intent(out)    ::  Found                         !< Indicator whether the file has been found
!       integer                                     ,optional ,intent(out)    ::  iDir                          !< Index of the directory where the file has been found (iDir=0 if the file is absent from all directories)
!       type(Directory_Type)                        ,optional ,intent(out)    ::  Dir                           !< Directory object where the file has been found (Empty object if the file is absent from all directories)
!     End Subroutine

    Module Function FindFile( This, BaseName, DirKeys, RecKeys, Recursive, Mandatory, Found, LogLevel ) result(FullName)
      class(DirectoriesContainer_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  BaseName
      character(*)                                          ,intent(in)     ::  DirKeys
      logical                                     ,optional ,intent(in)     ::  RecKeys(:)
      logical                                     ,optional ,intent(in)     ::  Recursive
      logical                                     ,optional ,intent(in)     ::  Mandatory
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  LogLevel
      character(:)  ,allocatable                                            ::  FullName
    End Function


  End Interface

End Module