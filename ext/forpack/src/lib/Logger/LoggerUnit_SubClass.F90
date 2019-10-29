SubModule(LoggerUnit_Class) LoggerUnit_SubClass

  implicit none

! ******************** (copied from Logger_Class)
! ******************** (copied from Logger_Class)


  contains

Module Procedure InitializeLoggerUnit
  use Logger_Tools_Module   ,only:  Get_OptOrDef_Value
  use Logger_Parameters     ,only:  Default_OpenStatus, Valid_OpenStatus, Default_OpenPosition, Valid_OpenPosition
  character(:)  ,allocatable                                            ::  OpenStatus
  character(:)  ,allocatable                                            ::  OpenPosition
  integer                                                               ::  ios
  call This%Free()


  This%FileName     =   "<STDOUT>"
  This%ToScreen     =   .True.

  if ( present(FileName) ) then
  if ( len_trim(FileName) /= 0 ) then
    call This%SetFileName( FileName, ForceFileName )
    OpenStatus      =   Get_OptOrDef_Value( Default_OpenStatus,   Valid_OpenStatus,   Status   )                 ! Getting the open status
    OpenPosition    =   Get_OptOrDef_Value( Default_OpenPosition, Valid_OpenPosition, Position )                 ! Getting the open position
    open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition, IOStat=ios )           ! Opening the log file
    if ( ios /= 0 ) call ErrorOpenLoggerUnit( This )                                                              ! If opening error, then print error message and stop the code
    This%ToScreen   =   .False.
  end if
  end if

  This%Active       =   .True.
End Procedure

Module Procedure LoggerUnitConstructor
  call This%Initialize( FileName, Status, Position, ForceFileName )
End Procedure

Module Procedure FreeLoggerUnit
  This%ToScreen   =   .False.
  This%Active     =   .False.
  This%Index      =   0
  This%Unit       =   Output_Unit
  if ( allocated(This%FileName) ) deallocate(This%FileName)
End Procedure

Module Procedure FinalizeLoggerUnit
  call This%Free()
End Procedure







! This procedure reopens a Logger object. The name of the new file can be either:
! 1) Provided in input as the optional input argument 'FileName'. The path is then converted to
!    an absolute path with respect to current working directory if not already an absolute path.
! 2) Taken from the 'FileName' component already stored inside the object. The basename of the file
!    (ie. without path part) is then extracted and then the absolute with respect to current working
!    directory is considered.
! For the case where the 'FileName' component is re-used (case 2),
! The current Logger is first closed and deleted. Then, it is initialized using the same filename than the
! one it currently has. This will open a new file (with the same name but eventually in a different directory)
! with the poition is set to 'APPEND'.
! This procedure is mainly used when one wnat to change the working directory of the Fortran application.
! Once the application has changed directory, this 'Reopen' procedure is called to delete the logfile in the
! previous working directory, and to open it in the new working directory. The 'APPEND' position ensures
! that the informaton previously written to the logfile are not overwritten.
Module Procedure ReopenLoggerUnit

  use File_Library        ,only:  GetAbsolutePath, GetBaseName

  character(*)                                              ,parameter  ::  OpenStatus   = 'OLD'
  character(*)                                              ,parameter  ::  OpenPosition = 'APPEND'
  integer                                                               ::  ios, OldUnit
  character(:)  ,allocatable                                            ::  FileName_

  if ( present(FileName) ) then
    FileName_   =   FileName
  else
    FileName_   =   GetBaseName(This%FileName)
  end if
  This%FileName =   GetAbsolutePath(FileName_)

  call This%Close( Status="DELETE" )
  open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition, IOStat=ios )
  if ( ios /= 0 ) call ErrorOpenLoggerUnit( This )



! # ifdef GCC_COMPILER
! !
!   OldUnit   =   This%Unit
! !   call This%Close( Status="DELETE" )                                                                            ! Deleting the current Logger (the log file is deleted)
! !
! !   write(*,*) "[ReopenLoggerUnit] ls"; call Execute_Command_Line( 'ls' )
! !
! ! !   Block
! ! !     character(1000)             ::  LongString
! ! !     character(:)  ,allocatable  ::  Command
! ! !     integer                     ::  ExitStat, CmdStat
! ! !     write(*,*) "[ReopenLoggerUnit]: Calling Execute_Command_Line"
! ! !     Command = 'pwd'
! ! !     call Execute_Command_Line( Command, ExitStat=ExitStat, CmdStat=CmdStat, CmdMsg=LongString )
! ! !     write(*,*) "[ReopenLoggerUnit]: -> Command    = ", Command
! ! !     write(*,*) "[ReopenLoggerUnit]: -> ExitStat   = ", ExitStat
! ! !     write(*,*) "[ReopenLoggerUnit]: -> CmdStat    = ", CmdStat
! ! ! !     write(*,*) "[ReopenLoggerUnit]: -> LongString = ", LongString
! ! !   end block
! ! !
! ! !   Block
! ! !     character(1000)             ::  LongString
! ! !     character(:)  ,allocatable  ::  Command
! ! !     integer                     ::  ExitStat, CmdStat
! ! !     write(*,*) "[ReopenLoggerUnit]: Calling Execute_Command_Line"
! ! !     Command = 'ls'
! ! !     call Execute_Command_Line( Command, ExitStat=ExitStat, CmdStat=CmdStat, CmdMsg=LongString )
! ! !     write(*,*) "[ReopenLoggerUnit]: -> Command    = ", Command
! ! !     write(*,*) "[ReopenLoggerUnit]: -> ExitStat   = ", ExitStat
! ! !     write(*,*) "[ReopenLoggerUnit]: -> CmdStat    = ", CmdStat
! ! ! !     write(*,*) "[ReopenLoggerUnit]: -> LongString = ", LongString
! ! !   end block
! !
! !
!   write(*,*) "[ReopenLoggerUnit]: This%Unit = ", This%Unit
!   write(*,*) "[ReopenLoggerUnit]: This%FileName = ", This%FileName
!   write(*,*) "[ReopenLoggerUnit]: OpenStatus = ", OpenStatus
!   write(*,*) "[ReopenLoggerUnit]: OpenPosition = ", OpenPosition
! ! !   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition, IOStat=ios )           ! Opening the log file
! !   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition )           ! Opening the log file
! !   write(*,*) "[ReopenLoggerUnit]: ios = ", ios
! !
!
!
!   write(*,*) "[ReopenLoggerUnit] ls 1 "; call Execute_Command_Line( 'ls' )
!   write(*,*) "[ReopenLoggerUnit]: Calling open (new file)"
!   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition, IOStat=ios )           ! Opening the log file
! !   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition )           ! Opening the log file
!   write(*,*) "[ReopenLoggerUnit]: -> ios = ", ios
!
!
!   write(*,*) "[ReopenLoggerUnit] ls 2"; call Execute_Command_Line( 'ls' )
!
!   write(*,*) "[ReopenLoggerUnit]: Calling close (Old file)"
!   close( OldUnit, Status='DELETE', IOstat=ios )
!   write(*,*) "[ReopenLoggerUnit]: -> ios = ", ios
!
!
!   write(*,*) "[ReopenLoggerUnit] ls 3"; call Execute_Command_Line( 'ls' )
!
! ! !   if ( ios /= 0 ) call ErrorOpenLoggerUnit( This )
! # else
!   call This%Close( Status="DELETE" )
!   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition, IOStat=ios )           ! Opening the log file
! !   open( NewUnit=This%Unit, File=This%FileName, Status=OpenStatus, Position=OpenPosition )           ! Opening the log file
!   if ( ios /= 0 ) call ErrorOpenLoggerUnit( This )                                                              ! If opening error, then print error message and stop the code
! # endif
End Procedure


! This procedure closes a LoggerUnit object.
!   TODO: Add a checking that the "Status" string is a valid close status
Module Procedure CloseLoggerUnit
  character(:)  ,allocatable                                            ::  Status_
  integer                                                               ::  ios
  Status_   =   ""
  if ( present(Status) ) then
    Status_        =       Status
    close( This%Unit, Status=Status_, IOstat=ios )
  else
    close( This%Unit, IOstat=ios  )
  end if
End Procedure

! This procedure backspaces the logfile associated to a LoggerUnit object.
Module Procedure BackspaceLoggerUnit
  integer                                                               ::  ios
  backspace( This%Unit, iostat=ios )
  if ( present(Status) ) Status = ios
End Procedure

! This procedure rewinds the file associated to a LoggerUnit object.
Module Procedure RewindLoggerUnit
  integer                                                               ::  ios
  rewind( This%Unit, iostat=ios )
  if ( present(Status) ) Status = ios
End Procedure

! This procedure flushes the unit associated to a LoggerUnit object.
Module Procedure FlushLoggerUnit
  flush( This%Unit )
End Procedure


Module Procedure GetLoggerUnitFileName
  use Utilities_Library    ,only:  PresentAndTrue
  use File_Library        ,only:  GetAbsolutePath, GetBaseName
  FileName      =   This%FileName
  if ( present(AbsolutePath) ) then
    if ( AbsolutePath ) then
      FileName  =   GetAbsolutePath(FileName)
    else
      FileName  =   GetBaseName(FileName)
    end if
  end if
End Procedure

! This procedure sets the name of the file associated to the logger.
! If a coarray simulation is considered, then the index of the image is added as a suffix to the filename.
! This ensures that all images have different filenames. It is required since different images are not
! allowed to operate on the same file.
Module Procedure SetLoggerUnitFileName

  use File_Library       ,only:  GetAbsolutePath

# ifdef COARRAY
  character(10)                                                         ::  String                          ! Character string required to store the current image index (Required because write cannot make a length allocation)
# endif
  logical                                                               ::  SuffixImageIndex
  if ( .Not. present(FileName) ) then
    This%FileName   =   "<STDOUT>"
  else
    This%FileName   =   trim( adjustl(FileName) )
#   ifdef COARRAY
      if ( This_Image() == 1 ) return                                                                           ! If 1st image is considered, nothing to do so exiting
      SuffixImageIndex = .True.; if ( present(ForceFileName) ) SuffixImageIndex = .Not. ForceFileName
      if ( .Not. SuffixImageIndex) ) return                                                                     ! If image index is not to be added, nothing to do so exiting
      write(String,"(i0)") This_Image()                                                                         ! Converting the image index into a string
      This%FileName =   This%FileName//"_"//trim(adjustl(String))                                               ! Setting the FileName of the log file with the image index
#   endif
  end if
  This%FileName     =   GetAbsolutePath(This%FileName)

End Procedure




! **************************************************************************************************************
! **************************************************************************************************************
!                                           TOOLS (copied from Logger_Class)
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine ErrorOpenLoggerUnit( This )
  class(LoggerUnit_Type)                                ,intent(in)     ::  This                                !< Passed-object dummy argument corresponding
  write(*,"(4x,'[ErrorOpenLoggerUnit]: Error opening the Log file')")
  write(*,"(4x,'[ErrorOpenLoggerUnit]: FileName = ',g0)") This%FileName
  write(*,"(4x,'[ErrorOpenLoggerUnit]: Unit     = ',g0)") This%Unit
  write(*,"(4x,'[ErrorOpenLoggerUnit]: Stopping the code')")
  error stop
End Subroutine


End SubModule
