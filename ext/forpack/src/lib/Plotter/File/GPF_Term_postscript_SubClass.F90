SubModule(GPF_Term_POSTSCRIPT_Class) GPF_Term_POSTSCRIPT_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure GetName
  use GPF_Parameters            ,only:  KEY_postscript
  Name    =   KEY_postscript
End Procedure

Module Procedure InitializeTermPOSTSCRIPT

  character(*)                                              ,parameter  ::  ProcName = "InitializeTermPOSTSCRIPT"
  character(*)                                              ,parameter  ::  Keyword='term'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetTerminalExtension" )
  call SetTerminalExtension( This, Extension )

  if (Dbg) call Logger%Write( "Calling This%SetColor" )
  call This%SetColor( Color )

  if (Dbg) call Logger%Write( "Calling GPF_Font_Type" )
  call This%Font%Initialize( FontName, FontSize )

  if (Dbg) call Logger%Write( "Calling This%SetEnhanced" )
  call This%SetEnhanced( Enhanced )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetCommandTermPOSTSCRIPT
  This%Command  =   'set '  //  &
    This%Keyword            //  &
    This%GetName()//" "     //  &
    This%Extension          //  &
    This%Color              //  &
    This%Enhanced           //  &
    This%Font%Command
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetTerminalExtension( This, Extension )
  use GPF_Parameters            ,only:  KEY_eps
  use Utilities_Library         ,only:  GetOptArgValue
  type(GPF_Term_POSTSCRIPT_Type)                        ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  Extension                       !< Terminal extension
  This%Extension    =   GetOptArgValue(KEY_eps,Extension) // " "
End Subroutine

End SubModule