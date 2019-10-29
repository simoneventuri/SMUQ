SubModule(GPF_View_Class) GPF_View_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeView

  character(*)                                              ,parameter  ::  ProcName = "InitializeOrigin"
  character(*)                                              ,parameter  ::  Keyword='view'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetViewValue" )
  call SetViewValue( This, Rot_x, Rot_z, Scale, Scale_z )

  if (Dbg) call Logger%Write( "Calling SetViewMap" )
  call SetViewMap( This, i_Map )

  if (Dbg) call Logger%Write( "Calling SetViewEqual" )
  call SetViewEqual( This, i_Equal_xy, i_Equal_xyz )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetViewCommand
  This%Command          =       ''                                                                              ! Initialisation the command to an empty string: default value
  if ( len_trim(This%Value)   /= 0 )    This%Command = This%Command // This%Value                               ! Setting the graph view angles and scales if defined
  if ( len_trim(This%Map)     /= 0 )    This%Command = This%Command // This%Map                                 ! Setting the graph map view if defined
  if ( len_trim(This%Equal)   /= 0 )    This%Command = This%Command // This%Equal                               ! Setting the equal view indicator if defined
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command                   ! Setting the command string
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetViewValue( This, Rot_x, Rot_z, Scale, Scale_z )
  type(GPF_View_Type)                                   ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Rot_x
  character(*)                                ,optional ,intent(in)     ::  Rot_z
  character(*)                                ,optional ,intent(in)     ::  Scale
  character(*)                                ,optional ,intent(in)     ::  Scale_z
  This%Value    =       ''
  if ( present(Rot_x) )   This%Value = Rot_x
  if ( present(Rot_z) )   This%Value = This%Value // ',' // Rot_z
  if ( present(Scale) )   This%Value = This%Value // ',' // Scale
  if ( present(Scale_z) ) This%Value = This%Value // ',' // Scale_z
End Subroutine

Subroutine SetViewMap( This, i_Map )
  use GPF_Class         ,only:  GPF
  type(GPF_View_Type)                                   ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  i_Map
  This%Map      =       ''
  if ( present(i_Map) ) then
    if (i_Map) This%Map = 'map '
  else
    if ( len_trim(GPF%Default%View_Map) /= 0 ) This%Map = 'map '
  end if
End Subroutine

Subroutine SetViewEqual( This, i_Equal_xy, i_Equal_xyz )
  type(GPF_View_Type)                                   ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  i_Equal_xy
  logical                                     ,optional ,intent(in)     ::  i_Equal_xyz
  This%Equal   =       ''
  if ( present(i_Equal_xy) ) then
    if (i_Equal_xy)  This%Equal = 'equal xy'
  end if
  if ( present(i_Equal_xyz) ) then
    if (i_Equal_xyz) This%Equal = 'equal xyz'
  end if
  if ( len_trim(This%Equal) /= 0 ) This%Equal = This%Equal // " "
End Subroutine

End SubModule