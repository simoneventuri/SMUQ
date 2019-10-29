Module LoggerUnit_Class

  use ,intrinsic :: iso_fortran_env   ,only:  Output_Unit

  implicit none

  private
  public  ::  LoggerUnit_Type

  Type                          ::  LoggerUnit_Type
    logical                     ::  ToScreen  =   .False.
    logical                     ::  Active    =   .False.                 !< Indicator whether the LoggerUnit is activated
    integer                     ::  Index     =   0                       !< Index of current LoggerUnit
    integer                     ::  Unit      =   Output_Unit             !< Unit number associated to current LoggerUnit
    character(:)  ,allocatable  ::  FileName                              !< Name of the file associated to current LoggerUnit
  contains
    private
    Final               ::  FinalizeLoggerUnit
    procedure ,public   ::  Initialize  =>  InitializeLoggerUnit          !< Initializes a LoggerUnit object
    procedure ,public   ::  Free        =>  FreeLoggerUnit                !< Destroyes a LoggerUnit object
    procedure ,public   ::  Reopen      =>  ReopenLoggerUnit              !< Close and deleted current file, and then reopen it in 'append' mode
    procedure ,public   ::  Close       =>  CloseLoggerUnit               !< Closes the file associated to a LoggerUnit object
    procedure ,public   ::  Backspace   =>  BackspaceLoggerUnit           !< Backspaces the file associated to a LoggerUnit object
    procedure ,public   ::  Rewind      =>  RewindLoggerUnit              !< Rewinds the file associated to a LoggerUnit object
    procedure ,public   ::  Flush       =>  FlushLoggerUnit               !< Flushes the unit associated to a LoggerUnit object
    procedure ,public   ::  GetFileName =>  GetLoggerUnitFileName
    procedure ,public   ::  SetFileName =>  SetLoggerUnitFileName
  End Type


  Interface           LoggerUnit_Type
    Module Procedure  LoggerUnitConstructor
  End Interface

  Interface
    Module Function LoggerUnitConstructor( FileName, Status, Position, ForceFileName ) result(This)
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< Name of the log file
      character(*)                                ,optional ,intent(in)     ::  Status
      character(*)                                ,optional ,intent(in)     ::  Position
      logical                                     ,optional ,intent(in)     ::  ForceFileName
      type(LoggerUnit_Type)                                                 ::  This                                !< Passed-object dummy argument
    End Function
    Module Subroutine InitializeLoggerUnit( This, FileName, Status, Position, ForceFileName )
      class(LoggerUnit_Type)                                ,intent(inout)  ::  This                                !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< Name of the log file
      character(*)                                ,optional ,intent(in)     ::  Status
      character(*)                                ,optional ,intent(in)     ::  Position
      logical                                     ,optional ,intent(in)     ::  ForceFileName
    End Subroutine
    Pure Module Subroutine FreeLoggerUnit( This )
      class(LoggerUnit_Type)                                ,intent(inout)  ::  This                                !< Passed-object dummy argument
    End Subroutine
    Pure Module Subroutine FinalizeLoggerUnit( This )
      type(LoggerUnit_Type)                                 ,intent(inout)  ::  This
    End Subroutine


    Module Subroutine ReopenLoggerUnit( This, FileName )
      class(LoggerUnit_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< Name of the log file
    End Subroutine
    Module Subroutine CloseLoggerUnit( This, Status )
      class(LoggerUnit_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Status                      !< Status of the close instruction
    End Subroutine
    Module Subroutine BackspaceLoggerUnit( This, Status )
      class(LoggerUnit_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(out)    ::  Status                      !< Error status indicator (=0 if everthing is ok, /=0 if error)
    End Subroutine
    Module Subroutine RewindLoggerUnit( This, Status )
      class(LoggerUnit_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                     ,optional ,intent(out)    ::  Status                      !< Error status indicator (=0 if everthing is ok, /=0 if error)
    End Subroutine
    Module Subroutine FlushLoggerUnit( This )
      class(LoggerUnit_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument
    End Subroutine
    Module Function GetLoggerUnitFileName( This, AbsolutePath ) result(FileName)
      class(LoggerUnit_Type)                                ,intent(in)     ::  This
      logical                                     ,optional ,intent(in)     ::  AbsolutePath
      character(:)  ,allocatable                                            ::  FileName
    End Function
    Module Subroutine SetLoggerUnitFileName( This, FileName, ForceFileName )
      class(LoggerUnit_Type)                                ,intent(inout)  ::  This                                !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  FileName                            !< FileName of the log file
      logical                                     ,optional ,intent(in)     ::  ForceFileName
    End Subroutine

  End Interface

End Module
