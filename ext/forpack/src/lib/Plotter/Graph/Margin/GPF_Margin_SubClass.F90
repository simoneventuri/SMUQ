SubModule(GPF_Margin_Class) GPF_Margin_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! REMARK:
! One could check whether the Value optional input argument contains a string from margin coordinates.
! This could be done using the "Is_Numeric(trim(Value))" instruction.
! However, in doing so, if the Value optional input argument contains the "at screen" keyword, the
! Value variable will not be reconized as a valid margin.
! Therefore, inorder to enable the use of the "at screen" keyword, the correctness of the input argument is
! not check.
Module Procedure InitializeMargin

  character(*)                                              ,parameter  ::  ProcName = "InitializeGrid"
  character(*)                                              ,parameter  ::  Keyword='margin'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetMarginValue" )
  call SetMarginValue( This, Value )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Orientation // Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetMarginCommand
  This%Command          =       ''                                                                              ! Initialisation the command to an empty string: default value
  if ( len_trim(This%Value) /= 0 )      This%Command = 'set ' // This%Keyword  // This%Value                    ! If present optional input argument, then setting the current margin command
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetMarginValue( This, Value )
  type(GPF_Margin_Type)                                 ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Value
  This%Value    =       ''
  if ( present(Value) ) This%Value = Value
End Subroutine

End SubModule