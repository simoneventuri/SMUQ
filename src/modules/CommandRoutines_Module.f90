module CommandRoutines_Module

use File_Library
use Logger_Class                                                  ,only: Logger
use Error_Class                                                   ,only: Error

implicit none

private

public                                                                ::    MakeDirectory
public                                                                ::    RemovePath
public                                                                ::    ExecuteSysCommand

logical, parameter                                                    ::    i_Debug_Global = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine MakeDirectory( Path, Options, i_Debug )

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options
    logical, optional, intent(in)                                     ::    i_Debug

    character(*), parameter                                           ::    ProcName='MakeDirectory'
    logical                                                           ::    i_Debug_Loc
    character(:), allocatable                                         ::    PathLoc
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    IOLoc=0

    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug
    if (i_Debug_Loc) call Logger%Entering( ProcName )

    PathLoc = trim(adjustl(Path))

    CommandLine = 'mkdir '
    if ( present(Options) ) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // PathLoc

    call ExecuteSysCommand( CommandLine, ExitStatus=IOLoc )
    if ( IOLoc /= 0 ) call Error%Raise( Line='Something went wrong creating directory: ' // PathLoc, ProcName=ProcName )
      
    if (i_Debug_Loc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine RemovePath( Path, Options, i_Debug )

    character(*), intent(in)                                          ::    Path
    character(*), optional, intent(in)                                ::    Options
    logical, optional, intent(in)                                     ::    i_Debug

    character(*), parameter                                           ::    ProcName='RemovePath'
    logical                                                           ::    i_Debug_Loc
    character(:), allocatable                                         ::    PathLoc
    character(:), allocatable                                         ::    CommandLine
    integer                                                           ::    IOLoc=0

    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug
    if (i_Debug_Loc) call Logger%Entering( ProcName )

    PathLoc = trim(adjustl(Path))

    CommandLine = 'rm '
    if ( present(Options) ) CommandLine = CommandLine // trim(adjustl(Options)) // ' '
    CommandLine = CommandLine // PathLoc

    call ExecuteSysCommand( CommandLine, ExitStatus=IOLoc )
    if ( IOLoc /= 0 ) call Error%Raise( Line='Something went wrong removing the path: ' // PathLoc, ProcName=ProcName )
      
    if (i_Debug_Loc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ExecuteSysCommand( SysCommand, Wait, ExitStatus, i_Debug ) 

    character(*), intent(in)                                          ::    SysCommand
    logical, optional, intent(in)                                     ::    Wait
    integer, optional, intent(out)                                    ::    ExitStatus
    logical, optional ,intent(in)                                     ::    i_Debug

    character(*), parameter                                           ::    ProcName='MakeDirectory'
    logical                                                           ::    i_Debug_Loc
    logical                                                           ::    WaitLoc=.true.
    integer                                                           ::    IOLoc=0

    i_Debug_Loc = i_Debug_Global
    if ( present(i_Debug) ) i_Debug_Loc = i_Debug
    if (i_Debug_Loc) call Logger%Entering( ProcName )

    if ( present(Wait) ) WaitLoc = Wait

    call execute_command_line( trim(adjustl(SysCommand)), wait=WaitLoc, exitstat=IOLoc )

    if ( present(ExitStatus) ) ExitStatus = IOLoc
      
    if (i_Debug_Loc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
