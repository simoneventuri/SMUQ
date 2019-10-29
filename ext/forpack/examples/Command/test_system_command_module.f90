Module Test_System_Command_Module

  use System_Command_Class    ,only:  System_Command_Type

  implicit none

!   contains
!
! Subroutine Test_System_Command( Logger_Input )
!
!   type(Logger_Type) ,target                             ,intent(in)     ::      Logger_Input                           !< Unit number to be associated to the log file
!
!   type(System_Command_Type)             ::      Command
!
!
!   type(System_Command_Type)                                             ::    Command                         ! Command-Line object
!   integer                                                               ::    Status
!   character(:)  ,allocatable                                            ::    ErrMsg, Directory, FileName
!   character(:)  ,allocatable  ,dimension(:)                             ::    ListFiles
!   logical                                                               ::    Overwrite
!
! ! ==============================================================================================================
! !    SETTING THE LOGGER OBJECT
! ! ==============================================================================================================
!   call Logger%Initialize( "Grouping.log" )                                                                      ! Opening the Log File
!   if (i_Debug_Loc) call Logger%Entering( ProcName)                                                              ! Writtng procedure entering logs
! ! ==============================================================================================================
!
!
  if (i_Debug_Loc) call Logger%Write( "Calling Set_System_Command_Logger" )
  call Set_System_Command_Logger( Logger )

! ==============================================================================================================
!    pwd
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Calling Command%pwd", NewLine=.True. )
  call Command%pwd( Directory, i_Debug=.True. )
  if (i_Debug_Loc) then
    call Logger%Write( "-> Directory = ", Directory )
    call Logger%Write( "-> Command%Status    = ", Command%Status )
    call Logger%Write( "-> Command%ErrMsg    = ", Command%ErrMsg )
  end if
! ==============================================================================================================

! ==============================================================================================================
!    ls
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Calling Command%ls", NewLine=.True. )
  call Command%ls( ListFiles, File="", Options="", i_Debug=.True. )
  if (i_Debug_Loc) then
    call Logger%Write( "-> size(ListFiles) = ", size(ListFiles) )
    do i = 1,size(ListFiles)
      call Logger%Write( "-> i = ", i, "ListFiles(i) = ", ListFiles(i), F2="i6" )
    end do
    call Logger%Write( "-> ListFiles = ", ListFiles )
    call Logger%Write( "-> Command%Status    = ", Command%Status )
    call Logger%Write( "-> Command%ErrMsg    = ", Command%ErrMsg )
  end if
! ==============================================================================================================

! ==============================================================================================================
!    cd
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Calling Command%cd", NewLine=.True. )
  Directory =   "Dir_To_Be_Created"
  call Command%cd( Directory, Create=.True., i_Debug=.True. )
  if (i_Debug_Loc) then
    call Logger%Write( "-> Command%Status    = ", Command%Status )
    call Logger%Write( "-> Command%ErrMsg    = ", Command%ErrMsg )
  end if
! ==============================================================================================================


!
! End Subroutine

End Module