module CommandRoutines_Module

use File_Library
use Logger_Class                                                  ,only: Logger
use Error_Class                                                   ,only: Error

implicit none

private

public                                                                ::    MakeDirectory
public                                                                ::    RemovePath
public                                                                ::    ExecuteSysCommand

logical, parameter                                                    ::    Debug_Global = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine MakeDirectory( Path, Options )

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options

    character(*), parameter                                           ::    ProcName='MakeDirectory'
    character(:), allocatable                                         ::    PathLoc
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    IOLoc=0

    PathLoc = trim(adjustl(Path))

    CommandLine = 'mkdir '
    if ( present(Options) ) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // PathLoc

    call ExecuteSysCommand( CommandLine, ExitStatus=IOLoc )
    if ( IOLoc /= 0 ) call Error%Raise( Line='Something went wrong creating directory: ' // PathLoc, ProcName=ProcName )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine RemovePath( Path, Options )

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options

    character(*), parameter                                           ::    ProcName='RemovePath'
    character(:), allocatable                                         ::    PathLoc
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    IOLoc=0

    PathLoc = trim(adjustl(Path))

    CommandLine = 'rm '
    if ( present(Options) ) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // PathLoc

    call ExecuteSysCommand( CommandLine, ExitStatus=IOLoc )
    if ( IOLoc /= 0 ) call Error%Raise( Line='Something went wrong removing the path: ' // PathLoc, ProcName=ProcName )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ExecuteSysCommand( SysCommand, Wait, ExitStatus ) 

    character(*), intent(in)                                          ::    SysCommand
    logical, optional, intent(in)                                     ::    Wait
    integer, optional, intent(out)                                    ::    ExitStatus

    character(*), parameter                                           ::    ProcName='MakeDirectory'
    logical                                                           ::    WaitLoc=.true.
    integer                                                           ::    IOLoc=0

    if ( present(Wait) ) WaitLoc = Wait

    call execute_command_line( trim(adjustl(SysCommand)), wait=WaitLoc, exitstat=IOLoc )

    if ( present(ExitStatus) ) ExitStatus = IOLoc

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
