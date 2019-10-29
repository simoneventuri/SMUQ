Module File_Module

  USE iso_c_binding   ,only:  C_INT

  implicit none

  private
!   public  ::  Does_File_Exist
  public  ::  FileExist
  public  ::  Is_File_Opened
  public  ::  Copy_File
  public  ::  Write_At_Top
  public  ::  GetExtension
  public  ::  GetBaseName
  public  ::  AddFileSuffix
  public  ::  AddFilePrefix
  public  ::  GetDirectory
  public  ::  RemoveExtension
  public  ::  AddPathToFile
  public  ::  RemoveTrailingSlash
  public  ::  RemoveLeadingSlash
  public  ::  GetAbsolutePath
  public  ::  CleanupPath
  public  ::  IsAbsolutePath
  public  ::  SplitPath
  public  ::  IsPathInDirectory
  public  ::  CommonPathPrefix
  public  ::  ParentFolder
  public  ::  RemoveLowerDirectory
  public  ::  RemoveUpperDirectory
  public  ::  file_info

  Interface             AddPathToFile
    Module Procedure    AddPathToFile
    Module Procedure    AddPathsToFile
  End Interface

  Interface             GetAbsolutePath
    Module Procedure    GetAbsolutePath_0d
    Module Procedure    GetAbsolutePath_1d
  End Interface

  Interface             IsPathInDirectory
    Module Procedure    IsPathInDirectory
    Module Procedure    IsPathInDirectories
  End Interface



  Interface
    Subroutine file_info(filename,mode,exist,time) BIND(C,name="file_info")
      use   iso_c_binding
      character(kind=c_char)                                ,intent(in)     :: filename(*)
      integer(c_int)                                        ,intent(out)    :: mode
      integer(c_int)                                        ,intent(out)    :: exist
      integer(c_int)                                        ,intent(out)    :: time
    End Subroutine
  End Interface

  Interface
!     Module Function Does_File_Exist( FileName, FileUnit ) result(i_Exist)
!       character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of file to be checked for existence
!       integer                                     ,optional ,intent(in)     ::  FileUnit                        !< Unit of file to be checked for existence
!       logical                                                               ::  i_Exist                         !< Existence indicator of the input file
!     End Function

    Module Function FileExist( DirectoryPath ) result(Exist)
      character(*)                                          ,intent(in)     ::  DirectoryPath
      logical                                                               ::  Exist                         !< Existence indicator of the input file
    End Function

    Module Function Is_File_Opened( FileName, FileUnit ) result(i_Opened)
      character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of file to be checked for existence
      integer                                     ,optional ,intent(in)     ::  FileUnit                        !< Unit of file to be checked for existence
      logical                                                               ::  i_Opened                        !< Open indicator of the input file
    End Function

    Module Subroutine Copy_File( FileUnit_Source, FileUnit_Target )
      integer                                               ,intent(in)     ::  FileUnit_Source
      integer                                               ,intent(in)     ::  FileUnit_Target
    End Subroutine

    Module Subroutine Write_At_Top( Unit, Array )
      integer                                               ,intent(in)     ::  Unit                            !< Unit number of the file in which the writing is to be performed
      character(*)  ,dimension(:)                           ,intent(in)     ::  Array                           !< Character array to write at the beginning of the file
    End Subroutine

    Module Function GetExtension( FileName ) result (Extension)
      character(*)                                          ,intent(in)     ::  FileName
      character(:)  ,allocatable                                            ::  Extension
    End Function

    Module Function RemoveExtension( FileName ) result (FileName_NoExt)
      character(*)                                          ,intent(in)     ::  FileName
      character(:)  ,allocatable                                            ::  FileName_NoExt
    End Function

    Module Function GetBaseName( FileName, i_Extension ) result (BaseName)
      character(*)                                          ,intent(in)     ::  FileName
      logical                                     ,optional ,intent(in)     ::  i_Extension
      character(:)  ,allocatable                                            ::  BaseName
    End Function

    Module Function AddFileSuffix( FileName, Suffix, Separator ) result (SuffixedFileName)
      character(*)                                          ,intent(in)     ::  FileName
      character(*)                                          ,intent(in)     ::  Suffix
      character(*)                                ,optional ,intent(in)     ::  Separator
      character(:)  ,allocatable                                            ::  SuffixedFileName
    End Function

    Module Function AddFilePrefix( FileName, Prefix, Separator ) result (PrefixedFileName)
      character(*)                                          ,intent(in)     ::  FileName
      character(*)                                          ,intent(in)     ::  Prefix
      character(*)                                ,optional ,intent(in)     ::  Separator
      character(:)  ,allocatable                                            ::  PrefixedFileName
    End Function

    Module Function GetDirectory( FullPath, AbsolutePath ) result (DirectoryPath)
      character(*)                                          ,intent(in)     ::  FullPath
      logical                                     ,optional ,intent(in)     ::  AbsolutePath
      character(:)  ,allocatable                                            ::  DirectoryPath
    End Function

    Module Function AddPathToFile( Path, FileName ) result (FullFileName)
      character(*)                                          ,intent(in)     ::  Path
      character(*)                                          ,intent(in)     ::  FileName
      character(:)  ,allocatable                                            ::  FullFileName
    End Function

    Module Function AddPathsToFile( Paths ) result (FullFileName)
      character(*)                                          ,intent(in)     ::  Paths(:)
      character(:)  ,allocatable                                            ::  FullFileName
    End Function

    Pure Module Function RemoveTrailingSlash( InputString ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function

    Module Function RemoveLeadingSlash( InputString ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function

    Module Function GetAbsolutePath_0d( FileName, CurrentDirectory ) result(AbsolutePath)
      character(*)                                          ,intent(in)     ::  FileName                        !< Name of file/directory whose absolute path is to be found
      character(*)                                ,optional ,intent(in)     ::  CurrentDirectory
      character(:)  ,allocatable                                            ::  AbsolutePath                    !< Name of file/directory using its absolute path
    End Function

    Module Function GetAbsolutePath_1d( FileNames, CurrentDirectory ) result(AbsolutePaths)
      character(*)  ,dimension(:)                           ,intent(in)     ::  FileNames
      character(*)                                ,optional ,intent(in)     ::  CurrentDirectory
      character(:)  ,dimension(:) ,allocatable                              ::  AbsolutePaths
    End Function

    Pure Module Function CleanupPath( InpPath ) result(OutPath)
      character(*)                                          ,intent(in)     ::  InpPath
      character(:)  ,allocatable                                            ::  OutPath
    End Function

    Pure Module Function IsAbsolutePath( Path ) result(IsAbsolute)
      character(*)                                          ,intent(in)     ::  Path
      logical                                                               ::  IsAbsolute
    End Function

    Module Function CommonPathPrefix( Paths ) result(CommonPath)
      character(*)                                          ,intent(in)     ::  Paths(:)
      character(:)  ,allocatable                                            ::  CommonPath
    End Function

    Module Function SplitPath( Path ) result(Folders)
      character(*)                                          ,intent(in)     ::  Path
      character(:)  ,dimension(:) ,allocatable                              ::  Folders
    End Function

    Module Function ParentFolder( Path ) result(Parent)
      character(*)                                          ,intent(in)     ::  Path
      character(:)  ,allocatable                                            ::  Parent
    End Function

    Module Function IsPathInDirectory( Path, Directory ) result(Inside)
      character(*)                                          ,intent(in)     ::  Path
      character(*)                                          ,intent(in)     ::  Directory
      logical                                                               ::  Inside
    End Function

    Module Function IsPathInDirectories( Path, Directories ) result(Inside)
      character(*)                                          ,intent(in)     ::  Path
      character(*)                                          ,intent(in)     ::  Directories(:)
      logical                                                               ::  Inside
    End Function

    Pure Module Function RemoveLowerDirectory( InpDir ) result(OutDir)
      character(*)                                          ,intent(in)     ::  InpDir
      character(:)  ,allocatable                                            ::  OutDir
    End Function

    Pure Module Function RemoveUpperDirectory( InpDir ) result(OutDir)
      character(*)                                          ,intent(in)     ::  InpDir                        !< Name of file/directory whose absolute path is to be found
      character(:)  ,allocatable                                            ::  OutDir                    !< Name of file/directory using its absolute path
    End Function

  End Interface

End Module