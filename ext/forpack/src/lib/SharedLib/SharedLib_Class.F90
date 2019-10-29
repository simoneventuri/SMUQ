Module SharedLib_Class

  use Status_Library            ,only:  Status_Type
  use, intrinsic :: iso_c_binding

  implicit none

  Type  ::  SharedLib_Type
    type(c_ptr)       ::  handle
    type(c_funptr)    ::  proc_addr
!     integer         ::  Status = 0
    type(Status_Type) ::  Status
    character(:)  ,allocatable  ::  FileName
    character(:)  ,allocatable  ::  SymbName
  contains
    private
    procedure ,public ::  Open      =>  OpenSharedLib
    procedure ,public ::  FindSymb  =>  FindSharedLibSymb
    procedure ,public ::  GetProcedure
  End Type

  Abstract Interface
    Subroutine SubNoArg() bind(c)
    End Subroutine
  End Interface

  Interface
    Module Subroutine OpenSharedLib( This, FileName, LogLevel )
      class(SharedLib_Type)                                 ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  FileName
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine FindSharedLibSymb( This, SymbolName, LogLevel )
      class(SharedLib_Type)                                 ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  SymbolName
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine GetProcedure( This, Proc, File, Name, LogLevel )
      class(SharedLib_Type)                                 ,intent(inout)  ::  This
      procedure(SubNoArg) ,bind(c)  ,pointer                ,intent(inout)  ::  Proc
      character(*)                                ,optional ,intent(in)     ::  File
      character(*)                                ,optional ,intent(in)     ::  Name
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

  End Interface

End Module