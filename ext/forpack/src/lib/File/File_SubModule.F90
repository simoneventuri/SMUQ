SubModule(File_Module) File_SubModule

  implicit none

  character(1)                                              ,parameter  ::  PathSeparator='/'

  contains

! Module Procedure Does_File_Exist
!   if ( present(FileName) ) then                                                                                 ! If the file name is provided in input arguments
!     inquire( File=FileName, Exist=i_Exist )                                                                     ! Setting the file existence indicator
!   else if ( present(FileUnit) ) then                                                                            ! If the file unit is provided in input arguments
!     inquire( Unit=FileUnit, Exist=i_Exist )                                                                     ! Setting the file existence indicator
!   else
!     stop "Error in Does_File_Exist procedure: either the FileName of FileUnit optional argument must be specified"
!   end if
! End Procedure

Module Procedure FileExist
  use iso_c_binding     ,only:  C_NULL_CHAR
  integer(c_int)                                                        ::  Mode_, Exist_, Time_
  integer(c_int)                                            ,parameter  ::  DoExist = 1
  call file_info( DirectoryPath//C_NULL_CHAR, Mode_, Exist_, Time_ )
  Exist   =   Exist_ == DoExist
End Procedure

Module Procedure Is_File_Opened
  if ( present(FileName) ) then                                                                                 ! If the file name is provided in input arguments
    inquire( File=FileName, Opened=i_Opened )                                                                   ! Setting the file open indicator
  else if ( present(FileUnit) ) then                                                                            ! If the file unit is provided in input arguments
    inquire( Unit=FileUnit, Opened=i_Opened )                                                                   ! Setting the file open indicator
  else
    stop "Error in Is_File_Opened procedure: either the FileName of FileUnit optional argument must be specified"
  end if
End Procedure

Module Procedure Copy_File
  integer                                                               ::  ios                             ! Input/Output status
  character(1000)                                                       ::  Line                            ! Character string corresponding a single line in the file
  rewind(FileUnit_Source)
  rewind(FileUnit_Target)
  do                                                                                                            ! Infinit loop for copying data from old to new file
    read(FileUnit_Source,'(a)',Iostat=ios) Line                                                                 ! Reading a single line from old file
    if ( ios < 0 ) exit                                                                                         ! If end-of-file, then exiting reading loop
    write(FileUnit_Target,"(a)")  trim(Line)                                                                    ! Writing current line from the source to the target file
  end do                                                                                                        ! End of infinit loop
End Procedure

Module Procedure Write_At_Top
  use, intrinsic :: iso_fortran_env ,only:  IOStat_End
  integer                                                               ::  iLine                           ! Line index
  integer                                                               ::  ios                             ! Input/Output status
  character(1000)       ,allocatable    ,dimension(:)                   ::  Lines                           ! Temporary character array storing all the lines written in the file (very long length since the max. length of lines in the file is unknown)

! Getting the number of lines in the file
  rewind(Unit)                                                                                                  ! Rewinding the file
  iLine = 0                                                                                                     ! Initializing the line index to zero (Required because of iterative count)
  do                                                                                                            ! Loop on all lines written in the file (loop is exit explicitly when an EOF os reached)
    read( Unit, *, iostat=ios )                                                                                 ! Reading one line of the file
    iLine       =       iLine + 1                                                                               ! Incrementation of line's index
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == 781 ) exit                                                                                      ! Idem => Workaround for coarray bug !!
  end do                                                                                                        ! End of loop on file's lines
  allocate( Lines(iLine) )                                                                                      ! Allocating the temporary variable storing the lines
  Lines =       ""

! Saving the lines in the file in the temporary variable and deleting the lines from the file
  rewind(Unit)
  do iLine = 1,size(Lines)
    read(Unit,"(a)",Iostat=ios) Lines(iLine)
  end do
!
! ! Writing the input character at the top of the file
  rewind(Unit)
  do iLine = 1,size(Array)
    write(Unit,"(a)") trim(Array(iLine))
  end do

! Writing back the initial content of the file into the file
  do iLine = 1,size(Lines)
    write(Unit,"(a)") trim(Lines(iLine))
  end do

End Procedure

Module Procedure GetExtension
  integer                                                               ::  iSeparator
  character(1)  ,parameter                                              ::  Separator='.'
  logical       ,parameter                                              ::  iBackward=.true.
  iSeparator    =       scan( FileName, Separator, Back=iBackward )
  if ( iSeparator == 0 ) then
    Extension   =       ''
  else
    Extension   =       trim(FileName(iSeparator:))
  end if
End Procedure

Module Procedure RemoveExtension
  integer                                                               ::  iSeparator
  character(1)  ,parameter                                              ::  Separator='.'
  logical       ,parameter                                              ::  iBackward=.true.
  iSeparator        =   scan( FileName, Separator, Back=iBackward )
  if ( iSeparator == 0 ) then
    FileName_NoExt  =   trim(FileName)
  else
    FileName_NoExt  =   trim(FileName(:iSeparator-1))
  end if
End Procedure

Module Procedure GetBaseName
  integer                                                               ::  iSeparator
  logical       ,parameter                                              ::  iBackward=.true.
  logical                                                               ::  i_Ext
  i_Ext =       .true.
  if ( present(i_Extension) ) i_Ext = i_Extension
  iSeparator    =       scan( FileName, PathSeparator, Back=iBackward )
  if ( iSeparator == 0 ) then
    BaseName    =       FileName
  else
    BaseName    =       FileName(iSeparator+1:)
  end if
  if (.not.i_Ext) BaseName = RemoveExtension( BaseName )
End Procedure

Module Procedure AddFileSuffix
  use Utilities_Library        ,only:  GetOptArgValue
  character(0)  ,parameter                                              ::  DefaultSeparator=""
  character(:)  ,allocatable                                            ::  Separator_
  Separator_        =   GetOptArgValue( DefaultSeparator, Separator )
  SuffixedFileName  =   RemoveExtension(FileName) // Separator_ // trim(Suffix) // GetExtension(FileName)
End Procedure

Module Procedure AddFilePrefix
  use Utilities_Library        ,only:  GetOptArgValue
  character(0)  ,parameter                                              ::  DefaultSeparator=""
  character(:)  ,allocatable                                            ::  Separator_
  Separator_        =   GetOptArgValue( DefaultSeparator, Separator )
  PrefixedFileName  =   Prefix // Separator_ // trim(FileName)
End Procedure

Module Procedure GetDirectory
  use Utilities_Library        ,only:  PresentAndTrue
  use String_Library          ,only:  Remove_Last_Directory_From_Path
  DirectoryPath   =   Remove_Last_Directory_From_Path( FullPath )
  if ( PresentAndTrue(AbsolutePath) ) DirectoryPath = GetAbsolutePath( DirectoryPath )
End Procedure

Module Procedure AddPathToFile
  FullFileName  =       RemoveTrailingSlash(Path) // PathSeparator // RemoveLeadingSlash(FileName)
End Procedure

Module Procedure AddPathsToFile
  integer                                                               ::  i
  if ( size(Paths) == 0 ) then
    FullFileName  =   ""
  else
    FullFileName  =   trim( Paths(1) )
    do i = 2,size(Paths)
      FullFileName    =   AddPathToFile( FullFileName, trim(Paths(i)) )
!       write(*,*) "   i = ", i, "FullFileName = ", FullFileName
    end do
  end if
End Procedure



Module Procedure RemoveTrailingSlash
  integer                                                               ::  Length
  character(1)                                                          ::  LastChar
  OutputString      =   trim(InputString)
  do
    Length          =   len_trim(OutputString)
    if ( Length == 0 ) exit
    LastChar        =   OutputString(Length:Length)
    if ( LastChar /= PathSeparator ) exit
    if ( Length == 1 ) then
      OutputString  =    ""
      exit
    else
      OutputString  =   OutputString(1:Length-1)
    end if
  end do
End Procedure

Module Procedure RemoveLeadingSlash
  integer                                                               ::  Length
  character(1)                                                          ::  FirstChar
  OutputString      =   trim(InputString)
  do
    Length          =   len_trim(OutputString)
    if ( Length == 0 ) exit
    FirstChar       =   OutputString(1:1)
    if ( FirstChar /= PathSeparator ) exit
    if ( Length == 1 ) then
      OutputString  =    ""
      exit
    else
      OutputString  =   OutputString(2:Length)
    end if
  end do
End Procedure

Module Procedure GetAbsolutePath_0d
# ifdef INTEL_COMPILER
  use ifport  ,only:  GetCWD
# endif
  use String_Library  ,only:  ReplaceCharacter
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  CurrentDirectory_
  character(:)  ,allocatable                                            ::  Directory
  integer                                                               ::  Status
  character( 1000 )                                                     ::  LongString
  AbsolutePath    =   ""
  if ( len_trim(FileName) == 0 ) return
  if ( present(CurrentDirectory) ) then
    CurrentDirectory_ =   CurrentDirectory
  else
    Status            =   GetCWD( LongString )
    CurrentDirectory_ =   trim(LongString)
  end if
  Directory           =   CurrentDirectory_
  Name                =   ReplaceCharacter( FileName, "/./", PathSeparator )
  do
    if ( len(Name) < 1 ) then
      AbsolutePath    =   Directory
    else if ( Name(1:1) == PathSeparator ) then
      AbsolutePath    =   Name
    else if ( Name(1:2) == "./" ) then
      if ( len(Name) >=3 ) then
        Name          =   RemoveUpperDirectory(Name)
      else
        Name          =   ""
      end if
      cycle
    else if ( Name(1:3) == "../" ) then
      Name            =   RemoveUpperDirectory(Name)
      Directory       =   RemoveLowerDirectory(Directory)
      cycle
    else
      AbsolutePath    =   AddPathToFile( Directory, Name )
    end if
    exit
  end do
  AbsolutePath        =   CleanupPath(AbsolutePath)
End Procedure

Module Procedure GetAbsolutePath_1d
  use Utilities_Library        ,only:  AddElementToArray
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  AbsolutePath                    !< Name of file/directory using its absolute path
  if ( size(FileNames) == 0 ) then
#   ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!       allocate( AbsolutePaths, source = FileNames )   !@COMPILER_BUG: gcc-7.3.0: ICE
!       allocate( character(0) :: AbsolutePaths(0) )    !@COMPILER_BUG: gcc-7.3.0: ICE
      Block
        character(:)  ,allocatable  ::  AbsolutePathsTmp(:)
        allocate( character(0) :: AbsolutePathsTmp(0) )
        call move_alloc( AbsolutePathsTmp, AbsolutePaths )
      End Block
#   else
      allocate( AbsolutePaths, source = FileNames )
#   endif
    return
  end if
  do i = 1,size(FileNames)
    AbsolutePath    =   GetAbsolutePath( trim(FileNames(i)), CurrentDirectory )
    call AddElementToArray( AbsolutePath, AbsolutePaths )
  end do
End Procedure

! This procedure will clean-up a path.
! It will do the following:
! * The "/.." characters will be removed by from the path and the directory just before
!   this key will be removed.
! !@TODO: Replace '~/' by the content of the HOME env. var
Module Procedure CleanupPath
  use String_Library  ,only:  ReplaceCharacter, RemoveDuplicateCharacters
  integer                                                               ::  i, j, k,l
  integer                                                               ::  is, ie
  character(:)  ,allocatable                                            ::  Left, Center, Right
  character(:)  ,allocatable                                            ::  Key,Str, Final, String
  character(:)  ,allocatable                                            ::  Prefix

  String  =   InpPath
  String  =   ReplaceCharacter( String, "/./", PathSeparator )
  String  =   RemoveDuplicateCharacters(String,PathSeparator)

  Prefix  =   ""

! First, find the prefix which need to be be kept. This prefix
! corresponds to parent directories "../" which cannot be replaced
! since we have no info on their names.
! We also remove the "./"
  do

    Key       =   "../"
    is        =   1
    ie        =   min(len(Key),len(String))
    Left      =   String(is:ie)
    if ( ie == len(String) ) then
      Right   =   ""
    else
      is      =   len(Key)+1
      ie      =   len(String)
      Right   =   String(is:ie)
    end if
    if ( Left == Key ) then
      Prefix  =   Prefix // Left
      String  =   Right
      cycle
    end if

    Key       =   "./"
    is        =   1
    ie        =   min(len(Key),len(String))
    Left      =   String(is:ie)
    if ( ie == len(String) ) then
      Right   =   ""
    else
      is      =   len(Key)+1
      ie      =   len(String)
      Right   =   String(is:ie)
    end if
    if ( Left == Key ) then
      String  =   Right
      cycle
    end if

    exit

  end do

! For each "/.." directory found, remove it and the parrent directory
  Key         =   "/.."
  do
    i         =   index(String,Key)
    j         =   i + len(Key)
    if ( i == 0 ) exit
    Left    =   RemoveLowerDirectory( String(1:i-1) )
    if ( len(String) < j ) then
      Right   =   ""
    else
      Right   =   String(j:)
    end if
    String    =   Left // Right
  end do

! Add the prefix to be kept
  OutPath       =   Prefix // String
  OutPath       =   RemoveTrailingSlash(OutPath)

! Removing the trailing "/."
  Key   =   "/."
  i     =   len(OutPath)
  if ( i /= 0 ) then
    if ( OutPath(i-1:i) == Key ) then
      OutPath   =   OutPath(1:i-2)
    end if
  end if
End Procedure

! This procedure will split a path into all the folder contained in the path.
Module Procedure SplitPath
  use String_Library  ,only:  ReplaceCharacter
  integer                                                               ::  i, j, k
  integer                                                               ::  NItems
  integer                                                               ::  Length
  character(:)  ,allocatable                                            ::  Left, Right, Path_
  Path_       =   Path
  NItems      =   0
  Length      =   0
  do
    i         =   index(Path_,PathSeparator)
    j         =   i + len(PathSeparator)
    if ( i /= 0 ) then
      Left      =   Path_(1:i-1)
      Right     =   Path_(j:)
      Path_     =   Right
      NItems    =   NItems + 1
      Length    =   max(Length,len(Left))
      cycle
    else
      if ( len(Right) /= 0 ) then
        NItems  =   NItems + 1
        Length  =   max(Length,len(Right))
      end if
      exit
    end if
  end do
# ifdef WORKAROUND_GCC_ALLOCATABLE_OUTPUT_CHARACTER_ARRAY_IN_FUNCTION
  Block
    character(:) ,allocatable   :: WASTR(:)
    allocate( character(Length) :: WASTR(NItems) )
    call move_alloc( WASTR, Folders )
  End Block
# else
  allocate( character(Length) :: Folders(NItems) )
# endif
  Folders(:)  = ""
  Path_       =   Path
  k           =   0
  do
    i         =   index(Path_,PathSeparator)
    j         =   i + len(PathSeparator)
    k         =   k + 1
    if ( i /= 0 ) then
      Left      =   Path_(1:i-1)
      Folders(k) =   Left
      Right     =   Path_(j:)
      Path_     =   Right
      cycle
    else
      if ( len(Right) /= 0 ) Folders(k) = Right
      exit
    end if
  end do
End Procedure

Module Procedure ParentFolder
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Path_
  Path_  =   RemoveTrailingSlash(Path)
  i         =   index(Path_,PathSeparator,back=.True.)
  if ( i /= 0 ) then
    Parent  =   Path_(1:i-1)
    i       =   index(Parent,PathSeparator,back=.True.)
    if ( i /= 0 ) then
      Parent  =   Parent(i+1:)
    end if
  else
    Parent  =   ""
  end if
End Procedure


! This procedure returns an indicator whether the path is absolute.
! It will ajust the path the the LHS and look if the first character is '/'.
! If it is, then the path is an absolute path
Module Procedure IsAbsolutePath
  use String_Library  ,only:  ReplaceCharacter
  integer                                                               ::  i, j
  character(:)  ,allocatable                                            ::  Path_
  IsAbsolute    =   .False.
  if ( len_trim(Path) > 0 ) then
    Path_       =   adjustl(Path)
    if ( Path_(1:1) == PathSeparator ) IsAbsolute = .True.
  end if
End Procedure



!
! Function IsIncludedInPath( Path, ListPaths ) result(Included)
!
!   use File_Library              ,only:  GetAbsolutePath
!   use String_Library            ,only:  GetSubStringIndexes
!
!   character(*)                                          ,intent(in)     ::  Path
!   character(*)                                          ,intent(in)     ::  ListPaths(:)
!   logical                                                               ::  Included
!
!   integer                                                               ::  i
!   character(:)  ,allocatable                                            ::  TargetPath
!   character(:)  ,allocatable                                            ::  CurrentPath
!
!   Included        =   .False.
!   TargetPath      =   GetAbsolutePath( Path )
! !   call Logger%Write( "-> TargetPath = ", TargetPath )
!   do i = 1,size(ListPaths)
!     CurrentPath   =   trim(GetAbsolutePath( ListPaths(i) ))
!     Included      =   IncludeSubString( TargetPath, CurrentPath, CaseSensitive=.True. )
! !     call Logger%Write( "-> i = ", i, "CurrentPath = ", CurrentPath, "Included = ", Included )
!     if (Included) return
!   end do
! End Function



!
! def in_directory(file, directory):
!     directory = os.path.join(os.path.realpath(directory), '')
!     file = os.path.realpath(file)
!
!     #return true, if the common prefix of both is equal to directory
!     #e.g. /a/b/c/d.rst and directory is /a/b, the common prefix is /a/b
!     return os.path.commonprefix([file, directory]) == directory



! This procedure checks whether 'Path' is in a subdirectory of 'Directory'
! Note that 'Path' can be either a file or a directory. For example:
!   Path                                Directory                   Inside
!   -----------------------------------------------------------------------
!   /home/user/example/file.dat         /home/user                  T
!   /a/bc                               /a/b                        F
!   /home/user/example                  /home/user                  T
!   /home/user/example/file.dat         /home/user/example          T
!   /home/user/other/file.dat           /home/user/example          F
!   /home/user                          /home/user                  T
Module Procedure IsPathInDirectory
  use File_Library              ,only:  GetAbsolutePath
  character(:)  ,allocatable                                            ::  AbsPath
  character(:)  ,allocatable                                            ::  AbsDirectory
  integer                                                               ::  L, k
  AbsPath       =   GetAbsolutePath( Path )
  AbsDirectory  =   GetAbsolutePath( Directory )
  Inside        =   (index(AbsPath,AbsDirectory) == 1 )   ! Not suffisent: only say that they have the same prefix but this will wrongly give IsPathInDirectory("/a/bc","/a/b")=True
  if ( .Not. Inside ) return
  if ( len(AbsPath) <= len(AbsDirectory) ) return
  k             =   len(AbsDirectory) + 1
  Inside        =   AbsPath(k:k) == PathSeparator
End Procedure

! This procedure checks whether 'Path' is in one subdirectory of 'Directories'.
Module Procedure IsPathInDirectories
  integer                                                               ::  i
  Inside    =   .False.
  do i = 1,size(Directories)
    Inside  =   IsPathInDirectory( Path, trim(Directories(i)) )
    if ( Inside ) exit
  end do
End Procedure


Module Procedure CommonPathPrefix
!   integer                                                               ::  i, iMin
!   integer                                                               ::  MinLength, Length
!   MinLength   =   len(Paths)
!   do i = 1,size(Paths)
!     Length    =   len_trim(Paths(i))
!     if ( Length <= MinLength ) then
!       MinLength =   Length
!       iMin      =   i
!     end if
!   end do
!   write(*,*) "MinLength = ", MinLength
!   write(*,*) "iMin      = ", iMin
  CommonPath    =   ""
End Procedure




! This procedure returns the input directory/file path in which the lower (ie. at the RHS) directory has been removed.
! If the input path has no '/' character or is equal to the root directorty '/' then an empty string is returned.
Module Procedure RemoveLowerDirectory
  integer                                                               ::  i
  OutDir    =   ""
  i         =   index(InpDir,PathSeparator,back=.True.)
  if ( i > 1 ) OutDir = InpDir(1:i-1)
End Procedure

! This procedure returns the input directory/file path in which the upper (ie. at the LHS) directory has been removed.
! If the input path has no '/' character or is equal to the root directorty '/' then an empty string is returned.
Module Procedure RemoveUpperDirectory
  integer                                                               ::  i
  OutDir    =   ""
  i         =   index(InpDir,PathSeparator,back=.False.)
  if ( i+1 <= len(InpDir) ) OutDir = InpDir(i+1:)
End Procedure

! @TODO
! This procedure returns true if the SubString is present in the string value
Function IncludeSubString( String, SubString, CaseSensitive ) result(Included)
  character(*)                          ,intent(in)     ::  String
  character(*)                          ,intent(in)     ::  SubString
  logical                     ,optional ,intent(in)     ::  CaseSensitive
  logical                                               ::  Included
  Included  =   GetSubStringIndex(String,SubString,CaseSensitive) /= 0
End Function

! @TODO
! This procedure returns the starting position of a substring of string, or zero if it does not occur as a substring
Function GetSubStringIndex( String, SubString, CaseSensitive ) result(i)
  use String_Module     ,only:  UpperCase
  character(*)                          ,intent(in)     ::  String
  character(*)                          ,intent(in)     ::  SubString
  logical                     ,optional ,intent(in)     ::  CaseSensitive
  integer                                               ::  i
  logical                                               ::  CaseSensitive_
  character(:)  ,allocatable                            ::  Value, SubString_
  CaseSensitive_  = .False.
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  if ( CaseSensitive_ ) then
    i             =       index( String, SubString )
  else
    Value         =       UpperCase( String )
    SubString_    =       UpperCase( SubString )
    i             =       index( Value, SubString_ )
  end if
End Function

End SubModule
