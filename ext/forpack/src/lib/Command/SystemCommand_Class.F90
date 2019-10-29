Module SystemCommand_Class

  use, intrinsic ::  iso_c_binding

  implicit none

  private
  public  ::  SystemCommand_Type
  public  ::  Select_File

  Type                                  ::  SystemCommand_Type
    logical                             ::  Initialized   =   .False.
    logical                             ::  NoErr         =   .True.
    logical                             ::  FatalError    =   .True.
    integer                             ::  ExitStat      =   0
    integer                             ::  CmdStat       =   0
    character(:)        ,allocatable    ::  Command                         ! Character string containing the command to be executed
    character(:)        ,allocatable    ::  Options                         ! Character string containing the command options
    character(:)        ,allocatable    ::  ErrMsg                          ! Error message if error occured, that is, if Status/=0
    character(:)        ,allocatable    ::  CmdMsg                          ! If an error condition occurs, this CmdMsg is assigned a processor-dependent explanatory message.
    character(:)        ,allocatable    ::  StdOutLine
    character(:)        ,allocatable    ::  StdErrLine


    type(C_PTR)                         ::  file  =   C_NULL_PTR
    integer                             ::  pid   =   -1
    character(:)        ,allocatable    ::  spid
  contains
    private
    Final                               ::  FinalizeSystemCommand
    procedure   ,public                 ::  Initialize  =>    InitializeSystemCommand
    procedure   ,public                 ::  Free        =>    FreeSystemCommand
    procedure   ,public                 ::  RaiseError  =>    RaiseErrorSystemCommand
    procedure   ,public                 ::  Execute     =>    ExecuteCommand
    procedure   ,public                 ::  Set         =>    SetCommand
    procedure   ,public                 ::  Get         =>    GetCommand
    procedure   ,public                 ::  pwd         =>    Execute_pwd
    generic     ,public                 ::  cp          =>    Execute_cp_Single, Execute_cp_Multiple
    procedure   ,public                 ::  mv          =>    Execute_mv
    procedure   ,public                 ::  cd          =>    Execute_cd
    procedure   ,public                 ::  rm          =>    Execute_rm
    procedure   ,public                 ::  mkdir       =>    Execute_mkdir
    procedure   ,public                 ::  ls          =>    Execute_ls
    procedure   ,public                 ::  find        =>    Execute_find
    procedure   ,public                 ::  beep        =>    Execute_beep
    generic     ,public                 ::  ToString    =>    CommandToString, CommandToStrings
    procedure   ,public   ,nopass       ::  Exists      =>    DoesCommandExist
    procedure   ,public   ,nopass       ::  DoesDirectoryExists
    procedure   ,public                 ::  Select_and_Copy_File
    procedure   ,public                 ::  HasError
    procedure                           ::  CommandToString
    procedure                           ::  CommandToStrings
    procedure                           ::  Execute_cp_Single
    procedure                           ::  Execute_cp_Multiple

    procedure   ,public                 ::  Run => RunProcess
    procedure   ,public                 ::  getLine
    procedure   ,public                 ::  process_readline
    procedure   ,public                 ::  GetAllLines
    procedure   ,public                 ::  getPid
    procedure   ,public                 ::  IsActive
    procedure   ,public                 ::  Terminate
  End Type

  Interface

    Pure Elemental Module Subroutine FinalizeSystemCommand( This )
      type(SystemCommand_Type)                              ,intent(inout)  ::  This                              !< Passed-object dummy argument
    End Subroutine

    Pure Elemental Module Subroutine InitializeSystemCommand( This )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
    End Subroutine

    Pure Elemental Module Subroutine FreeSystemCommand( This )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
    End Subroutine

    Module Subroutine RaiseErrorSystemCommand( This, ProcName )
      class(SystemCommand_Type)                             ,intent(in)     ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  ProcName                          !< Procedure in which the error occured
    End Subroutine

    Pure Elemental Module Function HasError( This ) result(Error)
      class(SystemCommand_Type)                             ,intent(in)     ::  This                              !< Passed-object dummy argument
      logical                                                               ::  Error                             !< Indicator whether the Command object has an error
    End Function

    Module Subroutine ExecuteCommand( This, Command )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Command                           !< Command string
    End Subroutine

    Pure Module Subroutine SetCommand( This, Command, StdOut, StdErr, NoErr )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Command                           !< Command string
      character(*)                                ,optional ,intent(in)     ::  StdOut
      character(*)                                ,optional ,intent(in)     ::  StdErr
      logical                                     ,optional ,intent(in)     ::  NoErr
    End Subroutine

    Module Function GetCommand( This ) result(Command)
      class(SystemCommand_Type)                             ,intent(in)     ::  This                              !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Command
    End Function



    ! **************************************************************************************************************
    !                                   PROCEDURES EMULATING SYSTEM COMMANDS
    ! **************************************************************************************************************
    Module Subroutine Execute_pwd( This, Directory, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(:)  ,allocatable                            ,intent(out)    ::  Directory                         !< Current working directory
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_cp_Single( This, Source, Target, Options, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Source                            !< Name of source to be copied (files or directory)
      character(*)                                          ,intent(in)     ::  Target                            !< Name of target (path)
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_cp_Multiple( This, Sources, Target, Options, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)  ,dimension(:)                           ,intent(in)     ::  Sources                           !< Name of sources to be copied (files or directory)
      character(*)                                          ,intent(in)     ::  Target                            !< Name of target (path)
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_mv( This, Source, Target, Options, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Source                            !< Name of source to be moved/renamed (files or directory)
      character(*)                                          ,intent(in)     ::  Target                            !< Name of target (path)
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_cd( This, Directory, Options, Create, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Directory                         !< Name of the directory to go to
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  Create                            !< Indicator whether the Directory should be created if absent (False by default, )
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_rm( This, Directory, Options, FatalError, DryRun, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Directory                         !< Name of the directory/file to eb removed
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  DryRun
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_mkdir( This, Directory, Options, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Directory                         !< Name of the directory to be created
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Command line options
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_ls( This, ListFiles, File, Options, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(:)  ,dimension( : ) ,allocatable            ,intent(out)    ::  ListFiles                         !< List of files output by the ls command
      character(*)                                ,optional ,intent(in)     ::  File                              !< File passed to the ls command
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Options passed to the ls command
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_find( This, ListFiles, Options, Path, Expression, FatalError, AbsolutePath, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(:)  ,dimension( : ) ,allocatable            ,intent(out)    ::  ListFiles                         !< List of files output by the 'find' command
      character(*)                                ,optional ,intent(in)     ::  Options                           !< Options passed to the ls command
      character(*)                                ,optional ,intent(in)     ::  Path                              !<
      character(*)                                ,optional ,intent(in)     ::  Expression                        !<
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  AbsolutePath                      !< Indicator whether the found items should be outputed with an absolute path
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine Execute_beep( This, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    ! **************************************************************************************************************
    !                                   PROCEDURES FOR COMPLEX OPERATIONS (GROUPS OF COMMANDS)
    ! **************************************************************************************************************
    Module Function DoesDirectoryExists( Directory, FatalError, Debug ) result(DirectoryExist)
      character(*)                                          ,intent(in)     ::  Directory                         !< Name of directory to be checked for existence
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
      logical                                                               ::  DirectoryExist                    ! Directory existence indicator
    End Function

    Module Subroutine Select_and_Copy_File( This, SourceDir, File_Pattern, TargetFile, File_Type, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  SourceDir                         !< Name of the source directory where the file to copy have to be searched for
      character(*)                                ,optional ,intent(in)     ::  TargetFile
      character(*)                                ,optional ,intent(in)     ::  File_Pattern
      character(*)                                ,optional ,intent(in)     ::  File_Type
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Function Select_File( ListFiles, File_Type ) result(File)
      character(*)          ,dimension(:)                   ,intent(in)     ::  ListFiles                         !< List of files satisfying a given pattern in the working directory
      character(*)                                ,optional ,intent(in)     ::  File_Type
      character(:)  ,allocatable                                            ::  File
    End Function

    Module Function DoesCommandExist( CommandName, Debug ) result(CommandExist)
      character(*)                                          ,intent(in)     ::  CommandName                       !< Name of the command to be tested for existence
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
      logical                                                               ::  CommandExist                      ! Existence indicator
    End Function

    Module Subroutine CommandToString( This, Command, Output, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Command                           !< Options passed to the ls command
      character(:)  ,allocatable                            ,intent(out)    ::  Output                            !< List of files output by the 'find' command
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

    Module Subroutine CommandToStrings( This, Command, Output, FatalError, Debug )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This                              !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Command                           !< Options passed to the ls command
      character(:)  ,allocatable                            ,intent(out)    ::  Output(:)                         !< List of files output by the 'find' command
      logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
      logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
    End Subroutine

!     Module Subroutine Execute_test( This, ListFiles, Options, Path, Expression, FatalError, Debug )
!       class(SystemCommand_Type)                             ,intent(out)    ::  This                              !< Passed-object dummy argument
!       character(:)  ,dimension( : ) ,allocatable            ,intent(out)    ::  ListFiles                         !< List of files output by the 'find' command
!       character(*)                                ,optional ,intent(in)     ::  Options                           !< Options passed to the ls command
!       character(*)                                ,optional ,intent(in)     ::  Path                              !<
!       character(*)                                ,optional ,intent(in)     ::  Expression                        !<
!       logical                                     ,optional ,intent(in)     ::  FatalError                        !< Fatal error indicator
!       logical                                     ,optional ,intent(in)     ::  Debug                             !< Debugging indicator
!     End Subroutine

  End Interface









  Interface

    Module Subroutine RunProcess( This, Command, RunInBackground, Line, Lines, Debug )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Command
      logical                                     ,optional ,intent(in)     ::  runInBackground
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Line
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Lines(:)
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Function getLine( This, Debug ) result(Line)
      class(SystemCommand_Type)                                             ::  This
      character(:)  ,allocatable                                            ::  Line
      logical                                     ,optional ,intent(in)     ::  Debug
    End Function

    Module Subroutine process_readline( This, Line, ierr )
      class(SystemCommand_Type)                             ,intent(inout)  ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  Line   ! Line must be at least two
      integer                                               ,intent(out)    ::  ierr
    End Subroutine

    Module Function GetAllLines( This, Delim, iErr ) result(String)
      class(SystemCommand_Type)                             ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Delim
      integer                                     ,optional ,intent(out)    ::  iErr
      character(:)  ,allocatable                                            ::  String
    End Function

    Module Pure Function getPid(This) result(pid)
      class(SystemCommand_Type)                             ,intent(in)     ::  This
      integer                                                               ::  pid
    End Function

    Module Function IsActive(This) result(Active)
      class(SystemCommand_Type)                             ,intent(in)     ::  This
      logical                                                               ::  Active
    End Function

    Module Subroutine Terminate(This)
      class(SystemCommand_Type)                             ,intent(inout)  ::  This
    End Subroutine

  End Interface


End Module