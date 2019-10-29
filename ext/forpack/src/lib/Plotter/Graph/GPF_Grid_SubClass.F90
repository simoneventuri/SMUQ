SubModule(GPF_Grid_Class) GPF_Grid_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeGrid

  character(*)                                              ,parameter  ::  ProcName = "InitializeGrid"
  character(*)                                              ,parameter  ::  Keyword='grid'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

!   if (Dbg) call Logger%Write( "Calling This%Set_Command" )
!   call This%Set_Command()

  This%Command  = "set grid back"
  if ( present(Grid_Command) ) This%Command = "set grid " // trim(Grid_Command)


  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetGridCommand
  This%Command          =       ''                                                                              ! Initializing the command to an empty string: default value => no grid
  if ( len_trim(This%Value) /= 0 )      This%Command = This%Command // This%Value
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command                   ! Setting the command string
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! Subroutine Set_Grid_Value( This, Value )
!   type(GPF_Grid_Type)                                   ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Grid object
!   character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the Grid command
!   This%Value     =       'back'
!   if ( present(Value) )  This%Value = Value
! End Subroutine

End SubModule