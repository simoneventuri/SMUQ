SubModule(SystemCommand_Class) SystemCommand_Pipe_SubClass

  use Logger_Class      ,only:  Logger

  implicit none

  contains

Module Procedure RunProcess

  use PipeInterfaces_Module   ,only:  popen
  use Utilities_Library       ,only:  PresentAndTrue
!   use String_Library          ,only:  Conver_To_String

  character(*)                                              ,parameter  ::  ProcName = "RunProcess"
  logical                                                               ::  Dbg
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  fullCommand
  character(:)  ,allocatable                                            ::  mode
  character(:)  ,allocatable                                            ::  String

  Dbg   =   PresentAndTrue(Debug)
  if (Dbg) call Logger%Entering( ProcName )

  fullCommand =   makeCommand( command, runInBackground )
  mode        =   nullTerminate('r')

  if (Dbg) call Logger%Write( "Calling popen: fullCommand = ", fullCommand(1:len(fullCommand)-1) )
  This%File   =   popen(fullCommand, mode)
  if (.not. c_associated(This%File)) then
    if (Dbg) call Logger%Write( "-> Error occured" )
    return
  end if

  if (present(runInBackground)) then
    if (runInBackground) then
      String = This%getLine()
      read(String,*) This%pid
      This%spid = trim(String)
    else
      This%pid  = -1
      This%spid = "-1"
    end if
  end if

  if ( present(Lines) ) then
    if (Dbg) call Logger%Write( "Calling GetOutputLines" )
    call GetOutputLines( This, Lines, Debug=.False. )
    if (Dbg) then
!       do i = 1,size(Lines)
        call Logger%Write( "-> i = ", "Lines = ", Lines )
!       end do
    end if
  end if

  if ( present(Line) ) then
    if (Dbg) call Logger%Write( "Calling this%getLine" )
    Line    =   This%getLine( Debug=Debug )
  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure getLine

  use PipeInterfaces_Module, only: c_getLine => getLine, fgets
  use PipeInterfaces_Module, only: free

  integer, parameter :: MAX_BUFFER_SIZE = 100000
  type(C_PTR)                                                           ::  pBuffer
  character(:)  ,allocatable                                            ::  BufferAlloc
  character(MAX_BUFFER_SIZE)  ,pointer                                  ::  Buffer
  integer(C_SIZE_T)                                                     ::  Length
  integer(C_SIZE_T)                                                     ::  rc
  integer(C_INT)   ::  size_

  pBuffer =   C_NULL_PTR
  rc      =   c_getline( pBuffer, Length, This%File )
  !call Logger%Write( "[getLine] Length = ", Length )
  !call Logger%Write( "[getLine] rc = ", rc )
  if (Length >= MAX_BUFFER_SIZE) then
      print*,'Error - need to increase MAX_BUFFER_SIZE in UnixProcess::getLine().'
  end if
  call c_f_pointer( pBuffer, Buffer )
  !call Logger%Write( "[getLine] len_trim(Buffer)  = ", len_trim(Buffer) )
  !call Logger%Write( "[getLine] len(Buffer)       = ", len(Buffer) )
  Line    =   Buffer(1:rc-1)    ! Drop newline and delimeter
  !call Logger%Write( "[getLine] len_trim(Line)    = ", len_trim(Line) )
  !call Logger%Write( "[getLine] len(Line)         = ", len(Line) )
  !call Logger%Write( "[getLine] trim(Line)        = ", trim(Line) )
  call free(pBuffer)










!   pBuffer =   C_NULL_PTR
!   rc      =   c_getline( pBuffer, Length, This%File )
!   !call Logger%Write( "[getLine] Length = ", Length )
!   !call Logger%Write( "[getLine] rc = ", rc )
!   allocate( character(rc) :: BufferAlloc )
!   call c_f_pointer( pBuffer, BufferAlloc )
!   !call Logger%Write( "[getLine] len_trim(BufferAlloc)  = ", len_trim(BufferAlloc) )
!   !call Logger%Write( "[getLine] len(BufferAlloc)       = ", len(BufferAlloc) )
!   Line    =   BufferAlloc(1:rc-1)    ! Drop newline and delimeter
!   !call Logger%Write( "[getLine] len_trim(Line)    = ", len_trim(Line) )
!   !call Logger%Write( "[getLine] len(Line)         = ", len(Line) )
!   !call Logger%Write( "[getLine] trim(Line)        = ", trim(Line) )
!   call free(pBuffer)


!   !call Logger%Write( "[getLine] fgets" )
!   pBuffer   =   fgets( Line, size_, This%File )
!   !call Logger%Write( "[getLine] len_trim(Line)    = ", len_trim(Line) )
!   !call Logger%Write( "[getLine] len(Line)         = ", len(Line) )
!   !call Logger%Write( "[getLine] trim(Line)        = ", trim(Line) )

End Procedure







! Function GetAllLines(cmd,delim,ierr)  result(string)
!
!   character(len=*),intent(in)           :: cmd
!   character(len=:),allocatable          :: string      !! assume will not run out of memory
!   character(len=*),intent(in),optional  :: delim
!   integer,intent(out),optional          :: ierr
!
!   character(len=:),allocatable          :: delim_local
!   integer                               :: ierr_local(3), ierr_read
!   integer                               :: i
!   type(streampointer)                   :: fp
!   character(len=4096)                   :: line        !! assumed long enough
!
!   if(present(delim))then
!     delim_local=delim
!   else
!     delim_local=' '
!   endif
!
!   !! change to stream I/O so do not have to have arbitrary line length limit, or at least make length an option
!   string=''
!   ierr_local(:)=0
!   call process_open_read(cmd,fp,ierr_local(1))
!
!   if(ierr_local(1).eq.0)then
!     do
!       call process_readline(line,fp,ierr_read)  ! read line from command output
!       if(ierr_read.ne.0)then
!         exit
!       endif
!       string=string//trim(line)//delim_local
!     enddo
!     string=trim(string)
!   endif
!
!   call process_close(fp,ierr_local(3)) ! Wrap up
!
!   if(present(ierr))then
!     do i=1,size(ierr_local)
!         if(ierr_local(i).ne.0)then
!           ierr=ierr_local(i)
!           exit
!         endif
!     enddo
!   elseif(any(ierr_local.ne.0))then
!     !!write(*,*)'*M_process::process_readall(3f)* error values=',ierr_local
!     stop
!   endif
!
! End Function

Module Procedure GetAllLines

  character(:)  ,allocatable                                            ::  Separator_
  integer                                                               ::  iErr_local(3), iErr_read, i
  character(:)  ,allocatable                                            ::  SingleLine

  call Logger%Entering( "GetAllLines" )

  Separator_  =   " "
  if ( present(delim) ) Separator_  = Delim

  !! change to stream I/O so do not have to have arbitrary line length limit, or at least make length an option
  String  =   ""
  iErr_local(:)=0
!   call process_open_read(cmd,This%File,iErr_local(1))


!   call Logger%Write( "[getLine] len_trim(Buffer)  = ", len_trim(Buffer) )

!   if(iErr_local(1).eq.0)then
    do
      call Logger%Write( "Calling This%process_readline" )
      call This%process_readline( SingleLine, iErr_read )  ! read line from command output
      call Logger%Write( "-> iErr_read = ", iErr_read )
      if ( iErr_read /= 0) exit
      call Logger%Write( "-> SingleLine = ", SingleLine )
      String  =   String // trim(SingleLine) // Separator_
      call Logger%Write( "-> String = ", String )
    enddo
    String    =   trim(String)

    call Logger%Write( "-> Done: String = ", String )

!   endif

!   call process_close(This%File,iErr_local(3)) ! Wrap up

!   if(present(iErr))then
!     do i=1,size(iErr_local)
!         if(iErr_local(i).ne.0)then
!           iErr=iErr_local(i)
!           exit
!         endif
!     enddo
!   elseif(any(iErr_local.ne.0))then
!     !!write(*,*)'*M_process::process_readall(3f)* error values=',iErr_local
!     stop
!   endif

  call Logger%Exiting()

End Procedure

Module Procedure getPid
  pid   =   This%pid
End Procedure


Module Procedure IsActive
  character(:)  ,allocatable                                            ::  Command
  integer                                                               ::  stat, cstat
  if ( This%pid >=0 ) then
    Command   =   "kill -0 "//This%spid//" > /dev/null 2>&1"

    call execute_command_line( Command, exitStat=stat, cmdStat=cstat )
    Active  =   (stat == 0)
  else
    Active  =   .False.
  end if
End Procedure

Module Procedure Terminate
  character(:)  ,allocatable                                            ::  Command
  integer                                                               ::  stat, cstat
  if ( This%pid >=0 ) then
    Command   =   "kill -15 `ps -ef 2> /dev/null | awk '$3 == "//This%spid//" {print $2}'` > /dev/null 2>&1"
    call execute_command_line(command, exitStat=stat, cmdStat=cstat)
    Command   =   "kill -15 "//This%spid//" > /dev/null 2>&1; "
    call execute_command_line(command, exitStat=stat, cmdStat=cstat)
  end if
End Procedure

Subroutine GetOutputLines( This, Lines, Delim, iErr, Debug )

  use Utilities_Library       ,only:  PresentAndTrue, AddElementToArray, GetOptArgValue

  class(SystemCommand_Type)                             ,intent(inout)  ::  This
  character(:)  ,allocatable                            ,intent(out)    ::  Lines(:)
  character(*)                                ,optional ,intent(in)     ::  Delim
  integer                                     ,optional ,intent(out)    ::  iErr
  logical                                     ,optional ,intent(in)     ::  Debug

  character(*)                                              ,parameter  ::  ProcName = "GetOutputLines"
  character(*)                                              ,parameter  ::  DefaultSeparator = " "
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  Separator_
  integer                                                               ::  iErr_local(3), iErr_read, i
  character(:)  ,allocatable                                            ::  Line

  Dbg   =   PresentAndTrue(Debug)
  if (Dbg) call Logger%Entering( ProcName )


  Separator_  =   GetOptArgValue( DefaultSeparator, Delim )
  iErr_local(:)=0

  if (Dbg) call Logger%Write( "Calling This%process_readline" )
  i   =   0
  do
    call This%process_readline( Line, iErr_read )
    if ( iErr_read /= 0) exit
    i   =   i + 1
    call AddElementToArray( Line, Lines )
    if (Dbg) call Logger%Write( "-> i = ", i, "Line = ", Line )
  enddo
  if (Dbg) call Logger%Write( "-> size(Lines) = ", size(Lines) )

  if (Dbg) call Logger%Exiting()

End Subroutine



Module Subroutine process_readline( This, Line, ierr )

  use PipeInterfaces_Module   ,only:  fgets

  class(SystemCommand_Type)                             ,intent(inout)  ::  This
  character(:)  ,allocatable                            ,intent(out)    ::  Line   ! Line must be at least two
  integer                                               ,intent(out)    ::  ierr

  integer(c_int)                                                        ::  clen
  integer                                                               ::  eos, i, ios
  character(1024)                                                       ::  LongLine

  clen   =   len(LongLine)-1
  LongLine   =   ' '

  do while ( c_associated(fgets(LongLine, clen, This%File)) )
    eos = 2
    do i = 1,clen
      if (LongLine(i:i) == C_NULL_CHAR) then
        eos               =   i - 2  ! assuming line terminator character and line string terminator should not be printed
        LongLine(eos+1:)  =   ' '
        exit
      end if
    end do
    ierr  =   0
    Line  =   trim(LongLine)
    return
  end do

!   ios   =   0
  ierr  =   -1!min(-1,ios)

End Subroutine




! Background commands must return a PID for further interactions.
! Also commands need to be null-terminated to send to C procedures.
Function makeCommand(baseCommand, runInBackground) result(command)
  use Utilities_Library       ,only:  GetOptArgValue
  character(:)  ,allocatable                                            ::  command
  character(*)                                          ,intent(in)     ::  baseCommand
  logical                                     ,optional ,intent(in)     ::  runInBackground
  logical                                                               ::  runInBackground_
  runInBackground_  =   GetOptArgValue(.False.,runInBackground)
  command           =   baseCommand
  if (runInBackground_) command = command // '& echo $!'
  command           =   nullTerminate(command)
End Function

Function nullTerminate(string) result(nullTerminatedString)
  use, intrinsic :: iso_c_binding ,only: C_NULL_CHAR
  character(*)                                          ,intent(in)     ::  string
  character(:)  ,allocatable                                            ::  nullTerminatedString
  nullTerminatedString  =   trim(string) // C_NULL_CHAR
End Function

End SubModule