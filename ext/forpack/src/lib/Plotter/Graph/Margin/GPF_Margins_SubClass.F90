SubModule(GPF_Margins_Class) GPF_Margins_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! The optional input argument Top, Bottom, Left and Right must correspond to "<margin>" or "at screen <margin>"
! where <margin> is a real number.
Module Procedure InitializeMargins

  character(*)                                              ,parameter  ::  ProcName = "InitializeMargins"
  character(*)                                              ,parameter  ::  Keyword='margin'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetMarginValue" )
  call This%Top%Initialize( "t", Top, Debug )

  if (Dbg) call Logger%Write( "Calling This%Bottom%Initialize" )
  call This%Bottom%Initialize( "b", Bottom, Debug )

  if (Dbg) call Logger%Write( "Calling This%Left%Initialize" )
  call This%Left%Initialize( "l", Left, Debug )

  if (Dbg) call Logger%Write( "Calling This%Right%Initialize" )
  call This%Right%Initialize( "r", Right, Debug )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure WriteMargins
  if ( .not. This%Presence ) return
  if ( This%Top%Get_Presence()    ) call This%Top%Write( Unit )
  if ( This%Bottom%Get_Presence() ) call This%Bottom%Write( Unit )
  if ( This%Left%Get_Presence()   ) call This%Left%Write( Unit )
  if ( This%Right%Get_Presence()  ) call This%Right%Write( Unit )
End Procedure

! The Margin object does not have a associated command string by itself.
! Instead, all its components, which correspond to Margin-Side objects, each has a command string corresponding
! to individual margin command for each of the four side of a graph.
! As a consequence, the command string of the Margin object is set to an empty string.
! However, in order to know if the command string of individual Margin-Side object must be written to the
! command file, one still need to set the Presence indicator variable associated to the Margin object.
! This Presence indicator  is set to true if at least one Margin-Side object is present.
Module Procedure SetMarginsCommand
  This%Command    =   ''
  This%Presence   =   This%Presence .or. This%Top%Presence
  This%Presence   =   This%Presence .or. This%Left%Presence
  This%Presence   =   This%Presence .or. This%Right%Presence
  This%Presence   =   This%Presence .or. This%Bottom%Presence
End Procedure

End SubModule