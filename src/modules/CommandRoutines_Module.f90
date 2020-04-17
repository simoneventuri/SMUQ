module CommandRoutines_Module

use File_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use ProgramDefs_Class                                             ,only:    ProgramDefs

implicit none

private

public                                                                ::    GetCurrentDirectoryPath
public                                                                ::    CopyDirectory
public                                                                ::    RemoveDirectory
public                                                                ::    MakeDirectory
public                                                                ::    RemovePath
public                                                                ::    ExecuteSysCommand

logical, parameter                                                    ::    Debug_Global = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCurrentDirectoryPath(Path)

    character(:), allocatable, intent(out)                            ::    Path

    character(*), parameter                                           ::    ProcName='GetCurrentDirectoryPath'
    character(:), allocatable                                         ::    CommandLine
    character(1000)                                                   ::    PathLoc
    integer                                                           ::    StatLoc=0

    call GetCWD(PathLoc, StatLoc)

    if (StatLoc /= 0) call Error%Raise('Something went wrong in GetCWD with error code: ' // ConvertToString(Value=StatLoc),   &
                                                                                                               ProcName=ProcName)

    Path = trim(adjustl(PathLoc))

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine CopyDirectory(Source, Destination, ContentsOnly)

    character(*), intent(in)                                          ::    Source
    character(*), intent(in)                                          ::    Destination
    logical, optional, intent(in)                                     ::    ContentsOnly

    character(*), parameter                                           ::    ProcName='CopyDirectory'
    character(:), allocatable                                         ::    CommandLine
    logical                                                           ::    ContentsOnlyLoc
    integer                                                           ::    StatLoc=0

    if (trim(adjustl(Source)) == '/' .or. trim(adjustl(Source)) == '/*')                                                        &
                                                                   call Error%Raise('Attempted to copy root', ProcName=ProcName)

    if (trim(adjustl(Source)) == ProgramDefs%GetRunDir()) call Error%Raise('Attempted to copy the run directory',              &
                                                                                                               ProcName=ProcName)

    ContentsOnlyLoc = .false.
    if (present(ContentsOnly)) ContentsOnlyLoc = ContentsOnly

    CommandLine = 'cp -rf ' // trim(adjustl(Source)) // '/'
    if (ContentsOnlyLoc) CommandLine = CommandLine // '/*'
    CommandLine = CommandLine // ' ' // trim(adjustl(Destination))

    call ExecuteSysCommand(CommandLine, Wait=.true., ExitStatus=StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong copying directory: ' // trim(adjustl(Source)) // ' -> ' //   &
                                                                                   trim(adjustl(Destination)), ProcName=ProcName)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemoveDirectory(Path, ContentsOnly)

    character(*), intent(in)                                          ::    Path
    logical, optional, intent(in)                                     ::    ContentsOnly

    character(*), parameter                                           ::    ProcName='RemoveDirectory'
    character(:), allocatable                                         ::    CommandLine
    logical                                                           ::    ContentsOnlyLoc
    integer                                                           ::    StatLoc=0

    if (trim(adjustl(Path)) == '/' .or. trim(adjustl(Path)) == '/*')                                                            &
                                                                 call Error%Raise('Attempted to delete root', ProcName=ProcName)

    if (trim(adjustl(Path)) == ProgramDefs%GetRunDir()) call Error%Raise('Attempted to delete run the directory',              &
                                                                                                               ProcName=ProcName)

    ContentsOnlyLoc = .false.
    if (present(ContentsOnly)) ContentsOnlyLoc = ContentsOnly

    CommandLine = 'rm -rf ' // trim(adjustl(Path))
    if (ContentsOnlyLoc) CommandLine = CommandLine // '/*'

    call ExecuteSysCommand(CommandLine, Wait=.true., ExitStatus=StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong removing directory: ' // trim(adjustl(Path)),                &
                                                                                                               ProcName=ProcName)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine MakeDirectory(Path, Options)

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options

    character(*), parameter                                           ::    ProcName='MakeDirectory'
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    StatLoc=0

    CommandLine = 'mkdir '
    if (present(Options)) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // trim(adjustl(Path))

    call ExecuteSysCommand(CommandLine, ExitStatus=StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong creating directory: ' // trim(adjustl(Path)),                &
                                                                                                               ProcName=ProcName)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RemovePath(Path, Options)

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options

    character(*), parameter                                           ::    ProcName='RemovePath'
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    StatLoc=0

    CommandLine = 'rm '
    if (present(Options)) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // trim(adjustl(Path))

    call ExecuteSysCommand(CommandLine, ExitStatus=StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong removing the path: ' // trim(adjustl(Path)),                 &
                                                                                                               ProcName=ProcName)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExecuteSysCommand(SysCommand, Wait, ExitStatus) 

    character(*), intent(in)                                          ::    SysCommand
    logical, optional, intent(in)                                     ::    Wait
    integer, optional, intent(out)                                    ::    ExitStatus

    character(*), parameter                                           ::    ProcName='ExecuteSysCommand'
    logical                                                           ::    WaitLoc
    integer                                                           ::    StatLoc=0

    WaitLoc = .true.
    if (present(Wait)) WaitLoc = Wait

    call execute_command_line(trim(adjustl(SysCommand)), wait=WaitLoc, exitstat=StatLoc)

    if (present(ExitStatus)) ExitStatus = StatLoc

    if (StatLoc /= 0 .and. .not. present(ExitStatus)) call Error%Raise('Something went wrong with system command : ' //          &
                                                                                                   SysCommand, ProcName=ProcName)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
