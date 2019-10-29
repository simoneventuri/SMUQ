Module File_Class

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  File_Type

  Type                          ::  File_Type
    integer                     ::  Unit                =   0
    integer                     ::  Status              =   0
    logical                     ::  IgnoreBlankLines    =   .False.
    logical                     ::  i_IgnoreBlankLines  =   .False.
    logical                     ::  i_Comment           =   .False.
    logical                     ::  i_Separator         =   .False.
    logical                     ::  Formatted           =   .True.
    character(:)  ,allocatable  ::  Name
    character(:)  ,allocatable  ::  ErrMsg
    character(:)  ,allocatable  ::  Comment
    character(:)  ,allocatable  ::  Separator
  contains
    procedure   ,public   ::  Open          =>  OpenFile
    procedure   ,public   ::  Rewind        =>  RewindFile
    procedure   ,public   ::  Close         =>  Close_File
    generic     ,public   ::  Write         =>  Write_CHAR_0d
    procedure   ,public   ::  SetProperties =>  SetFileProperties
    procedure   ,public   ::  GetData       =>  GetFileData_REAL64
    procedure   ,public   ::  Write_CHAR_0d
    procedure   ,public   ::  GetStatus
    procedure   ,public   ::  GetNumberOfLines
    procedure   ,public   ::  GetNumberOfColumns
  End Type

  Interface

    Module Subroutine OpenFile( This, FileName, ReadOnly, WriteOnly, iostat )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  FileName                        !< File name including relative path and extension
      logical                                     ,optional ,intent(in)     ::  ReadOnly
      logical                                     ,optional ,intent(in)     ::  WriteOnly
      integer                                     ,optional ,intent(out)    ::  iostat
    End Subroutine

    Pure Module Subroutine SetFileProperties( This, Comment, Separator, IgnoreBlankLines )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Comment
      character(*)                                ,optional ,intent(in)     ::  Separator                          !< Separation character string
      logical                                     ,optional ,intent(in)     ::  IgnoreBlankLines
    End Subroutine

    Module Subroutine Close_File( This, Status, iostat )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character                                   ,optional ,intent(in)     ::  Status
      integer                                     ,optional ,intent(out)    ::  iostat
    End Subroutine

    Module Subroutine RewindFile( This, iostat )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(out)    ::  iostat
    End Subroutine

    Module Subroutine Write_CHAR_0d( This, Variable )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Variable
    End Subroutine

    Pure Elemental Module Function GetStatus( This ) result(Status)
      class(File_Type)                                      ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                               ::  Status
    End Function

    Module Function GetNumberOfLines( This, Comment, IgnoreBlankLines ) result(NumberOfLines)
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(1)                                ,optional ,intent(in)     ::  Comment
      logical                                     ,optional ,intent(in)     ::  IgnoreBlankLines
      integer                                                               ::  NumberOfLines
    End Function

    Module Function GetNumberOfColumns( This, Separator, Comment, IgnoreBlankLines ) result(NumberOfColumns)
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Separator                          !< Separation character string
      character(1)                                ,optional ,intent(in)     ::  Comment
      logical                                     ,optional ,intent(in)     ::  IgnoreBlankLines
      integer                                                               ::  NumberOfColumns
    End Function

    Module Subroutine GetFileData_REAL64( This, Data )
      class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      real(REAL64)  ,allocatable                            ,intent(out)    ::  Data(:,:)
    End Subroutine

  End Interface

End Module