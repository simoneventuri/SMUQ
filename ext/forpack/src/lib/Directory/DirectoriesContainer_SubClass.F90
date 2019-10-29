SubModule(DirectoriesContainer_Class) DirectoriesContainer_SubClass

  implicit none

  contains

Module Procedure InitializeDirectoriesContainer
  This%Initialized  =   .True.
  This%NDir         =   0
  allocate( This%Dirs(0) )
End Procedure

Module Procedure FreeDirectoriesContainer
  This%Initialized  =   .True.
  This%NDir         =   0
  if ( allocated(This%Dirs) ) deallocate(This%Dirs)
  allocate( This%Dirs(0) )
End Procedure

Module Procedure AddDirectory
  type(Directory_Type)  ,dimension(:) ,allocatable                      ::  ListDir
  if ( .Not. This%Initialized ) call This%Free()
  allocate( ListDir, source = [This%Dirs,Directory] )
  call move_alloc( ListDir, This%Dirs )
  This%NDir   =   size(This%Dirs)
End Procedure

Module Procedure GetNumberDirectories
  NDir  =   This%NDir
End Procedure

Module Procedure GetDirectoryFromIndex
  call Dir%Clear()
  if ( .Not. This%Initialized     ) return
  if ( iDir < lbound(This%Dirs,1) ) return
  if ( iDir > ubound(This%Dirs,1) ) return
  Dir   =   This%Dirs(iDir)
End Procedure

Module Procedure GetDirectoryFromKey
  integer                                                               ::  iDir
  call Dir%Clear()
  if ( .Not. This%Initialized ) return
  iDir    =   This%GetIndex( Key )
  if ( iDir == 0 ) return
  Dir     =   This%GetDirectory( iDir )
End Procedure

Module Procedure GetDirectoryIndex
  iDir  =   0
  if ( .Not. This%Initialized ) return
  do iDir = 1,This%NDir
  associate( Dir => This%Dirs(iDir) )
    if ( Dir%Key == Key ) return
  end associate
  end do
  iDir  =   0
End Procedure

Module Procedure GetDirectoriesFromKey
  integer                                                               ::  iDir
  type(Directory_Type)  ,dimension(:) ,allocatable                      ::  List
  allocate( ListDir(0) )
  if ( .Not. This%Initialized ) return
  do iDir = 1,This%NDir
    if ( This%Dirs(iDir)%Key /= Key ) cycle
    allocate( List, source = [ListDir,This%Dirs(iDir)] )
    call move_alloc( List, ListDir )
  end do
End Procedure

Module Procedure UpdateDirectoryPath
  use File_Library      ,only:  CleanupPath
  integer                                                               ::  iDir
  if ( .Not. This%Initialized ) return
  iDir      =   This%GetIndex(Key)
  if ( iDir == 0 ) return
  This%Dirs(iDir)%Path  =   CleanupPath(Path)
End Procedure

Module Procedure GetDirectoryPath
  integer                                                               ::  iDir
  logical                                                               ::  HidePath
  HidePath  =   This%GetOptArgValue( .False., Hide )
  Path      =   ''
  if ( .Not. This%Initialized ) return
  iDir      =   This%GetIndex(Key)
  if ( iDir == 0 ) return
  Path      =   This%Dirs(iDir)%Path
  if ( HidePath ) Path = This%Dirs(iDir)%Substitute(Path)
End Procedure

Module Procedure GetDirectoryPaths
  use String_Library            ,only:  Add_Line_To_String
  integer                                                               ::  iDir
  logical                                                               ::  HidePath
  character(:)  ,allocatable                                            ::  Path
  HidePath  =   This%GetOptArgValue( .False., Hide )
  allocate( character(0) :: Paths(0) )
  if ( .Not. This%Initialized ) return
  do iDir = 1,This%NDir
    if ( This%Dirs(iDir)%Key /= Key ) cycle
    Path    =   This%Dirs(iDir)%Path
    if ( HidePath ) Path = This%Dirs(iDir)%Substitute(Path)
    call Add_Line_To_String( Paths , Path )
  end do
End Procedure

Module Procedure GetDirectoryDesc
  integer                                                               ::  iDir
  Desc      =   ""
  if ( .Not. This%Initialized ) return
  iDir      =   This%GetIndex(Key)
  if ( iDir == 0 ) return
  Desc      =   This%Dirs(iDir)%Description
End Procedure

Module Procedure GetLengthDirKey
  integer                                                               ::  iDir
  Length    =   0
  do iDir = 1,This%NDir
    Length  =   max(Length,len_trim(This%Dirs(iDir)%Key))
  end do
End Procedure

Module Procedure SubstituteDirectory
  integer                                                               ::  iDir
  integer                                                               ::  iIni, iFin
  integer ,dimension(This%NDir)                                         ::  Lengths
  integer ,dimension(1)                                                 ::  imaxloc
  character(:)  ,allocatable                                            ::  Path
  Lengths         =   0
  OutputString    =   InputString
  do iDir = 1,This%NDir
    Path          =   This%Dirs(iDir)%Path
    iIni          =   index(InputString,Path)  ! Setting index of first (initial) character to be replaced in the old string
    if ( iIni == 0 ) cycle
    iFin     =       iIni - 1 + len(Path)               !  Setting index of last (final) character to be replaced in the old string
    if ( iIni /= 0 ) Lengths(iDir) = iFin - iIni + 1
  end do
    imaxloc       =   maxloc(Lengths,1)
    iDir          =   imaxloc(1)
    OutputString  =   trim( This%Dirs(iDir)%Substitute( InputString ) )
End Procedure

Module Procedure IsDirectoryDefined
  integer                                                               ::  iDir
  Defined   =   .False.
  if ( .Not. This%Initialized ) return
  iDir      =   This%GetIndex(Key)
  if ( iDir == 0 ) return
  Defined   =   len_trim(This%Dirs(iDir)%Path) /= 0
End Procedure

Module Procedure GetDirectoriesSummary
  use String_Library    ,only:  Convert_To_String, SetLength
!   use String_Library    ,only:  Add_Line_To_String
!   use Logger_Class      ,only:  Logger, LogLevel_DEBUG, LogLevel_HEAVYDEBUG
  use Utilities_Library  ,only:  AddElementToArray
  integer                                                               ::  i
  integer                                                               ::  LengthKey
  integer                                                               ::  LengthName
  integer                                                               ::  LengthPath
  character(:)  ,allocatable                                            ::  Line, iPos, Key, Name, Path

  LengthKey   =   This%GetLengthKey()

!   @TODO: Add this in a procedure
  LengthName = 0; do i = 1,This%NDir; LengthName = max(LengthName,len_trim(This%Dirs(i)%PublicName)); end do
  LengthPath = 0; do i = 1,This%NDir; LengthPath = max(LengthPath,len_trim(This%Dirs(i)%Path)); end do

!   call AddElementToArray( "Spark%GetNumberDirectories() = "//Convert_To_String(This%NDir), Summary )

  do i = 1,This%NDir
    iPos  =   Convert_To_String( i              , Len=This%NDir )
    Key   =   SetLength( This%Dirs(i)%Key       , LengthKey )
    Name  =   SetLength( This%Dirs(i)%PublicName, LengthName )
    Path  =   SetLength( This%Dirs(i)%Path      , LengthPath )
    Line  =   "-> i = "//iPos//"   Key = "//Key//"   PublicName = "//Name//"   Path = "//Path
    call AddElementToArray( Line, Summary )
!     call Logger%Write( "-> Line = ", Line )
!     call Logger%Write( "-> i = ", i, "Key = ", This%Dirs(i)%Key, "PublicName = ", This%Dirs(i)%PublicName, "This%Dirs(i)%Path = ", This%Dirs(i)%Path, Fi="i3", F4="a10", F6="a25" )
  end do
!
!   call Logger%Write( "-> size(Summary) = ", size(Summary) )
!   call Logger%Write( "-> Summary = " )
!   call Logger%Write( Summary )
!
!   do i = 1,size(Summary)
!   call Logger%Write( "-> Summary(i) = ", Summary(i) )
!   end do

End Procedure


! Module Procedure FindFile
!   use File_Library      ,only:  FileExist, AddPathToFile
!   integer                                                               ::  i
!   character(:)  ,allocatable                                            ::  Path
!   character(:)  ,allocatable                                            ::  File
!   logical                                                               ::  Found_
!   FullName    =   ""
!   do i = 1,This%NDir
!   associate( CurrentDirectory => This%Dirs(i) )
!     if ( .Not. CurrentDirectory%Initialized ) cycle
!     Path      =   CurrentDirectory%Path
!     File      =   AddPathToFile( Path, BaseName )
!     Found_    =   FileExist(File)
!     if ( .Not. Found_) cycle
!     FullName  =   File
!     if ( present(Found) ) Found =   Found_
!     if ( present(iDir)  ) iDir  =   i
!     if ( present(Dir)   ) Dir   =   CurrentDirectory
!     exit
!   end associate
!   end do
! End Procedure


! @TODO: Add a mandatory/Status/Recursive argument
! @TODO: Use this procedure anywhere when appropriate to find a file
Module Procedure FindFile

  use Logger_Class              ,only:  Logger, LogLevel_HEAVYDEBUG
  use File_Library              ,only:  AddPathToFile, GetAbsolutePath, FileExist
  use String_Library            ,only:  Parse
  use Utilities_Library         ,only:  GetOptArgValue, SetOptArg, PresentAndTrue
  use SystemCommand_Library     ,only:  SystemCommand_Type

!   character(*)                                          ,intent(in)     ::  BaseName
!   character(*)                                          ,intent(in)     ::  DirKeys
!   logical                                     ,optional ,intent(in)     ::  RecKeys(:)
!   logical                                     ,optional ,intent(in)     ::  Recursive
!   logical                                     ,optional ,intent(in)     ::  Mandatory
!   logical                                     ,optional ,intent(out)    ::  Found
!   integer                                     ,optional ,intent(in)     ::  LogLevel
!   character(:)  ,allocatable                                            ::  FullName

  character(*)                                              ,parameter  ::  ProcName='FindFile'
  logical                                                               ::  Found_
  logical                                                               ::  Recursive_
  logical                                                               ::  Mandatory_
  logical ,allocatable                                                  ::  RecKeys_(:)
  integer                                                               ::  NKeys
  integer                                                               ::  i, j
  character(:)  ,allocatable                                            ::  ListKeys(:), Key, DirPath, FileName
  character(:)  ,allocatable                                            ::  ListFiles(:)                    ! List of files output by the 'find' command
  type(SystemCommand_Type)                                              ::  Command                         ! System-Command object

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )

  FullName      =   ""
  Found_        =   .False.

  Recursive_    =   GetOptArgValue( .False., Recursive )
  Mandatory_    =   GetOptArgValue( .False., Mandatory )

  call Parse( DirKeys, ",", ListKeys )
  NKeys   =   size(ListKeys)

  RecKeys_      =   [ (Recursive_, i=1,NKeys) ]
  if ( present(RecKeys) ) then
    do i = 1,min( size(RecKeys) , NKeys )
      RecKeys_(i)   =   RecKeys(i)
    end do
  end if

  call Logger%Write( "Seaching for file with options" )
  call Logger%Write( "-> BaseName     = ", BaseName )
  call Logger%Write( "-> DirKeys      = ", DirKeys )
  call Logger%Write( "-> Mandatory_   = ", Mandatory_ )
  call Logger%Write( "-> Recursive_   = ", Recursive_ )
  call Logger%Write( "-> RecKeys_     = ", RecKeys_ )

  if ( Logger%On() ) then
    call Logger%Write( "Searched locations"  )
    do i = 1,size(ListKeys)
      call Logger%Write( "-> Key = ", ListKeys(i), "RecursiveSearch = ", RecKeys_(i), "Path = ", This%GetPath(ListKeys(i)) )
    end do
  end if

  do i = 1,size(ListKeys)
    Key       =   trim(ListKeys(i))
    call Logger%Write( "-> i = ", i, "Key = ", Key )
    if ( .Not. This%IsDefined(Key) ) then
      call Logger%Write( "  -> Directory key '"//Key//"' not defined => Skipping" )
      cycle
    end if
    DirPath   =   This%GetPath(Key)
    call Logger%Write( "  -> DirPath = ", DirPath )
    if ( .Not. FileExist(DirPath) ) then
      call Logger%Write( "  -> Directory '"//DirPath//"' does not exist => Skipping" )
      cycle
    end if
    if ( RecKeys_(i) ) then
      call Logger%Write( "  -> Recursive search" )
      call Logger%Write( "  -> Calling Command%find: Path = ", DirPath, "Expression = ", BaseName )
      call Command%find( ListFiles, Path=DirPath, Expression=BaseName )
      call Logger%Write( "  -> size(ListFiles) = ", size(ListFiles) )
      select case (size(ListFiles))
        case (0)
          call Logger%Write( "  -> No file found => Skipping" )
          FileName  =   ""
          Found_    =   .False.
          cycle
        case (1)
          call Logger%Write( "  -> One file found => Picking it" )
          FileName  =   ListFiles(1)
          Found_    =   FileExist(FileName)
        case (2:)
          call Logger%Write( "  -> Several files found => Picking first one" )
          call Logger%Write( "  -> i = ", i, "ListFiles = ", ListFiles )
          FileName  =   ListFiles(1)
          Found_    =   FileExist(FileName)
      end select
    else
      call Logger%Write( "  -> Non-recursive search" )
      FileName      =   AddPathToFile(DirPath,BaseName)
      Found_        =   FileExist(FileName)
    end if
    call Logger%Write( "  -> Found_ = ", Found_, "FileName = ", FileName )
    if ( Found_ ) exit
  end do

  call SetOptArg( Found_, Found )

  if ( Found_ ) then
    FullName    =   GetAbsolutePath(FileName)
    call Logger%Write( "File Found_: FullName = ", FullName )
  else
    if ( PresentAndTrue(Mandatory) ) then
!     @TODO: Error
    end if
  end if

  call Logger%Exiting()

End Procedure



End SubModule