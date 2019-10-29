Module UnixProcess_Class

  use, intrinsic :: iso_c_binding

  implicit none

  private
  public :: UnixProcess_Type
#if defined(Intel) || defined(PGI)
  public :: execute_command_line
#endif


  type UnixProcess_Type
    private
    type(C_PTR) :: file = C_NULL_PTR
    integer :: pid = -1
  contains
    procedure :: getLine
    procedure :: getDelim
    procedure :: isActive
    procedure :: terminate
    procedure :: getPid
  end type

  interface UnixProcess_Type
    module procedure newProcess
  end interface UnixProcess_Type

contains

Function newProcess(command, runInBackground) result(process)
  use PipeInterfaces_Module, only: popen
  type(UnixProcess_Type)                                                ::  process
  character(*)                                          ,intent(in)     ::  command
  logical                                     ,optional ,intent(in)     ::  runInBackground

  character(:)  ,allocatable                                            ::  fullCommand
  character(:)  ,allocatable                                            ::  mode
  character(:)  ,allocatable                                            ::  string

  fullCommand = makeCommand(command, runInBackground)
  mode = nullTerminate('r')

  process%file = popen(fullCommand, mode)

  if (.not. c_associated(process%file)) return

  if (present(runInBackground)) then
    if (runInBackground) then
      string = process%getLine()
      read(string,*) process%pid
    else
      process%pid = -1
    end if
  end if

End Function

! Background commands must return a PID for further interactions.
! Also commands need to be null-terminated to send to C procedures.
Function makeCommand(baseCommand, runInBackground) result(command)
  character(:)  ,allocatable                                            ::  command
  character(*)                                          ,intent(in)     ::  baseCommand
  logical                                     ,optional ,intent(in)     ::  runInBackground
  logical                                                               ::  runInBackground_
  runInBackground_ = .False.
  if (present(runInBackground)) runInBackground_ = runInBackground
  command = baseCommand
  if (runInBackground_) command = command // '& echo $!'
  command = nullTerminate(command)
End Function

logical Function isActive(this)
  class(UnixProcess_Type)                               ,intent(in)     ::  this
  character(1024)                                                       ::  command
  integer                                                               ::  stat, cstat
  if (this%pid >=0) then
    write(command, '("kill -0 ",i0," > /dev/null 2>&1")') this%pid
    call execute_command_line(command, exitStat=stat, cmdStat=cstat)
    isActive = (stat == 0)
  else
    isActive = .False.
  end if
End Function

Subroutine terminate(this)
  class(UnixProcess_Type)                               ,intent(inout)  ::  this
  character(1024)                                                       ::  command
  integer                                                               ::  stat, cstat
  if (this%pid >=0) then
    write(command,'(a,i0,a)') "kill -15 `ps -ef 2> /dev/null | awk '$3 == ",this%pid," {print $2}'` > /dev/null 2>&1"
    call execute_command_line(command, exitStat=stat, cmdStat=cstat)
    write(command, '("kill -15 ",i0," > /dev/null 2>&1; ")') this%pid
    call execute_command_line(command, exitStat=stat, cmdStat=cstat)
  end if
End Subroutine

Function getLine(this) result(line)
  use PipeInterfaces_Module, only: c_getLine => getLine
  use PipeInterfaces_Module, only: free
  class(UnixProcess_Type)                                               :: this
  character(:)  ,allocatable                                            ::  line

  integer, parameter :: MAX_BUFFER_SIZE = 100000
  type(C_PTR)                                                           ::  pBuffer
  character(MAX_BUFFER_SIZE)  ,pointer                                  ::  buffer
  integer(C_SIZE_T)                                                     ::  length
  integer(C_SIZE_T)                                                     ::  rc

  pBuffer = C_NULL_PTR
  rc = c_getline(pBuffer, length, this%file)
  if (length >= MAX_BUFFER_SIZE) then
      print*,'Error - need to increase MAX_BUFFER_SIZE in UnixProcess::getLine().'
  end if

  call c_f_pointer(pBuffer, buffer)
  ! drop newline and delimeter
  line = buffer(1:rc-1)

  call free(pBuffer)

End Function

Function getDelim(this, delimeter) result(line)
  use PipeInterfaces_Module, only: c_getDelim => getDelim
  use PipeInterfaces_Module, only: free
  character(:)  ,allocatable                                            ::  line
  class(UnixProcess_Type) :: this
  character(C_CHAR), intent(in) :: delimeter

  type(C_PTR) :: pBuffer
  integer, parameter :: MAX_BUFFER_SIZE = 100000
  character(MAX_BUFFER_SIZE), pointer :: buffer
  integer(C_SIZE_T) :: length
  integer(C_SIZE_T) :: rc

  integer(C_INT) :: useDelimeter


  pBuffer = C_NULL_PTR
  useDelimeter = ichar(delimeter)
  rc = c_getdelim(pBuffer, length, useDelimeter, this%file)
  if (length >= MAX_BUFFER_SIZE) then
      print*,'Error - need to increase MAX_BUFFER_SIZE in UnixProcess::getLine().'
  end if

  call c_f_pointer(pBuffer, buffer)
  ! drop newline and delimeter
  line = buffer(1:rc-1)

  call free(pBuffer)

End Function

integer Function getPid(this) result(pid)
  class(UnixProcess_Type)                               ,intent(in)     ::  this
  pid = this%pid
End Function


#if defined(Intel) || defined(PGI)
Subroutine execute_command_line(command, exitStat, cmdStat)
#if defined(Intel)
  use ifport
  implicit none
#else
  implicit none
#include <lib3f.h>
#endif
  character(*)                                          ,intent(in)     ::  command
  integer, optional, intent(out) :: exitStat
  integer, optional, intent(out) :: cmdStat

  integer :: exitStat_

  !print *,'z04000<'//trim(command)//'>'
  exitStat_ = system(trim(command))
  if (present(exitStat)) exitStat = exitStat_
  if (present(cmdStat)) cmdStat = 0

End Subroutine
#endif

Function nullTerminate(string) result(nullTerminatedString)
  use, intrinsic :: iso_c_binding ,only: C_NULL_CHAR
  character(*)                                          ,intent(in)     ::  string
  character(:)  ,allocatable                                            ::  nullTerminatedString
  nullTerminatedString  =   trim(string) // C_NULL_CHAR
End Function

End Module