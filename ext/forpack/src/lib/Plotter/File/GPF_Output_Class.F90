Module GPF_Output_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Output_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Output_Type
    private
    logical                                             ::  HardCopy                                        !< HardCopy indicator
    character(:)        ,allocatable                    ::  FileName                                        !< Output file name including extension, if provided
    character(:)        ,allocatable                    ::  Name                                            !< Output file name without extension
    character(:)        ,allocatable                    ::  Extension                                       !< Output file extension
    character(:)        ,allocatable                    ::  DataFile                                        !< File name of the data file (name is changed in the GPF_Graph_Info_Class)
    character(:)        ,allocatable                    ::  Directory                                       !< File directory
    character(:)        ,allocatable                    ::  CommandFile                                     !< File name of the command file
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeOutput
    procedure   ,public   ::  Write         =>  WriteOutput             !< Writing File commands
    procedure   ,public   ::  GetName       =>  GetOutputName           !< Gets the Name
    procedure   ,public   ::  GetExtension  =>  GetOutputExtension      !< Gets the File Extension
    procedure   ,public   ::  GetFileName                               !< Gets the File Name
    procedure   ,public   ::  GetDataFileName                           !< Gets the DataFile
    procedure   ,public   ::  GetCommandFileName                        !< Gets the CommandFile
    procedure   ,public   ::  GetDirectory                              !< Gets the Directory
    procedure   ,public   ::  GetHardCopy                               !< Gets the Terminal HardCopy indicator
    procedure   ,public   ::  Set_Command   =>  SetOutputCommand      !< Sets the command string
  End Type

  Interface

    Module Subroutine InitializeOutput( This, FileName, Directory, HardCopy, Debug )
      class(GPF_Output_Type)                                ,intent(out)    ::  This                            !< Output object to be constructed
      character(*)                                ,optional ,intent(in)     ::  FileName                        !< File Name including extension, if provided
      character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory Name
      logical                                     ,optional ,intent(in)     ::  HardCopy                        !< Hardcopy indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine WriteOutput( This, Unit )
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      integer                                               ,intent(in)     ::  Unit                            !< File unit number
    End Subroutine

    Module Pure Function GetOutputName( This ) result(Name)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  Name                            !< Name of the image file
    End Function

    Module Pure Function GetOutputExtension( This ) result(Extension)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  Extension                       !< Extension
    End Function

    Module Pure Function GetFileName( This ) result(FileName)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  FileName                        !< FileName
    End Function

    Module Pure Function GetDataFileName( This ) result(DataFile)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  DataFile                        !< DataFile
    End Function

    Module Pure Function GetCommandFileName( This ) result(CommandFile)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  CommandFile                     !< CommandFile
    End Function

    Module Pure Function GetDirectory( This ) result(Directory)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      character(:)  ,allocatable                                            ::  Directory                       !< Directory
    End Function

    Module Pure Function GetHardCopy( This ) result(HardCopy)
      class(GPF_Output_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Output object
      logical                                                               ::  HardCopy                        !< Terminal HardCopy indicator
    End Function

    Module Subroutine SetOutputCommand( This )
      class(GPF_Output_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Output object
    end Subroutine

  End Interface

End Module