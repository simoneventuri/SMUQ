Module GPF_File_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Term_Class            ,only:  GPF_Term_Type
  use GPF_Output_Class          ,only:  GPF_Output_Type

  implicit none

  private
  public  ::  GPF_File_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_File_Type
!     private
    class(GPF_Term_Type)  ,allocatable  ::  Term                                            !< File Terminal object
    type(GPF_Output_Type)               ::  Output                                          !< File Output object
  contains
    private
    procedure   ,public   ::  Initialize          =>  InitializeFile
    procedure   ,public   ::  Write               =>  WriteFileCommands                     !< Writes the File commands
    procedure   ,public   ::  GetExtension        =>  GetFileExtension                      !< Gets the file extension
    procedure   ,public   ::  GetName             =>  GetFileName                           !< Gets the file name
    procedure   ,public   ::  GetDataFileName     =>  GetDataFileName                       !< Gets the name of the DataFile
    procedure   ,public   ::  GetCommandFileName  =>  GetCommandFileName                    !< Gets the name of the CommandFile
    procedure   ,public   ::  GetDirectory        =>  GetFileDirectory                      !< Gets the fiel Directory
    procedure   ,public   ::  GetFullName         =>  GetFileFullName
    procedure   ,public   ::  GetHardCopy         =>  GetFileHardCopy                       !< Gets the file hardcopy indicator
    procedure   ,public   ::  Set_Command         =>  SetFileCommand                        !< Sets the file command
    generic     ,public   ::  assignment(=)       =>  Assign                                !< Generic procedure for file assignment
    procedure             ::  Assign                                                        !< Assigns a file object
  End Type

  Interface

    Module Subroutine InitializeFile( This, Debug,                        &
                    Name, Directory, HardCopy,                            &
                    Terminal, FontName, FontSize, Enhanced, Color, Size )
      class(GPF_File_Type)                                  ,intent(out)    ::  This              !< File object to be constructed
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
      character(*)                                ,optional ,intent(in)     ::  Name              !< File Name
      character(*)                                ,optional ,intent(in)     ::  Directory         !< Directory Name
      logical                                     ,optional ,intent(in)     ::  HardCopy          !< Hardcopy indicator
      character(*)                                ,optional ,intent(in)     ::  Terminal          !< Terminal
      character(*)                                ,optional ,intent(in)     ::  FontName          !< File Font_Name
      character(*)                                ,optional ,intent(in)     ::  FontSize          !< File Font_Size
      logical                                     ,optional ,intent(in)     ::  Enhanced          !< File Enhancement indicator
      logical                                     ,optional ,intent(in)     ::  Color             !< File color indicator
      character(*)                                ,optional ,intent(in)     ::  Size              !< Terminal size (either "" or "size <XX>{unit},<YY>{unit}")
    End Subroutine

    Module Subroutine WriteFileCommands( This, Unit )
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      integer                                               ,intent(in)     ::  Unit                            !< File unit number
    End Subroutine

    Module Pure Subroutine Assign( This, File )
      class(GPF_File_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the File object
      type( GPF_File_Type)                                  ,intent(in)     ::  File                            !< File structure to be assign to the passed-object dummy argument (Rhs)
    End Subroutine

    Module Pure Function GetFileExtension( This ) result(Extension)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  Extension                       !< Extension
    End Function

    Module Pure Function GetFileName( This ) result(FileName)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  FileName                        !< FileName
    End Function

    Module Pure Function GetDataFileName( This ) result(DataFile)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  DataFile                        !< DataFile
    End Function

    Module Pure Function GetCommandFileName( This ) result(CommandFile)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  CommandFile                     !< CommandFile
    End Function

    Module Pure Function GetFileDirectory( This ) result(Directory)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  Directory                       !< Directory
    End Function

    Module Function GetFileFullName( This ) result(FullName)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      character(:)  ,allocatable                                            ::  FullName
    End Function

    Module Pure Function GetFileHardCopy( This ) result(HardCopy)
      class(GPF_File_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the File object
      logical                                                               ::  HardCopy                        !< Directory
    End Function

    Module Pure Subroutine SetFileCommand( This )
      class(GPF_File_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the File object
    End Subroutine

  End Interface

End Module