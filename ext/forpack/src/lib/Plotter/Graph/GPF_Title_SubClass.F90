SubModule(GPF_Title_Class) GPF_Title_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure InitializeTitle

  character(*)                                              ,parameter  ::  ProcName = "InitializeOrigin"
  character(*)                                              ,parameter  ::  Keyword='title'                 ! Keyword of current command
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  call This%Set_Debug( Debug )
  if (Dbg) call Logger%Write( "Entering" )

  if (Dbg) call Logger%Write( "Calling SetTitleText" )
  call SetTitleText( This, Text )

  if (Dbg) call Logger%Write( "Calling SetTitleOffset" )
  call SetTitleOffset( This, Offset )

  if (Dbg) call Logger%Write( "Calling SetTitleFont" )
  call SetTitleFont( This, FontName, FontSize )

  if (Dbg) call Logger%Write( "Calling SetTitleColor" )
  call SetTitleColor( This, Color )

  if (Dbg) call Logger%Write( "Calling SetTitleEnhanced" )
  call SetTitleEnhanced( This, Enhanced )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetTitleCommand
  This%Command          =   ''
  if ( len_trim(This%Text) /= 0 )               This%Command = This%Command // This%Text
  if ( len_trim(This%Offset%Command) /= 0 )     This%Command = This%Command // This%Offset%Command
  if ( len_trim(This%Font%Command) /= 0 )       This%Command = This%Command // This%Font%Command
  if ( len_trim(This%Color%Command) /= 0 )      This%Command = This%Command // This%Color%Command
  if ( len_trim(This%Enhanced) /= 0 )           This%Command = This%Command // This%Enhanced
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command
  This%Presence         =   ( len_trim(This%Command) /= 0 )
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetTitleText( This, Text )
  use GPF_Tools                 ,only:  Add_Apostroph
  type(GPF_Title_Type)                                  ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Text
  This%Text     =   ''
  if ( present(Text) )  This%Text = Add_Apostroph( Text )
  if ( len_trim(This%Text) /= 0 ) This%Text = This%Text // " "
End Subroutine

Subroutine SetTitleOffset( This, Offset )
  type(GPF_Title_Type)                                  ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Offset
  This%Offset%Command   =   ""
  if ( len_trim(This%Text) /= 0 ) then
    This%Offset         =   GPF_Offset_Type( Offset )
  end if
End Subroutine

Subroutine SetTitleFont( This, FontName, FontSize )
  type(GPF_Title_Type)                                  ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  FontName
  character(*)                                ,optional ,intent(in)     ::  FontSize
  This%Font%Command   =   ""
  if ( len_trim(This%Text) /= 0 ) then
    call This%Font%Initialize( FontName, FontSize )
  end if
End Subroutine

Subroutine SetTitleColor( This, Color )
  type(GPF_Title_Type)                                  ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Color
  character(*)                                              ,parameter  ::  Color_Keyword="tc"
  This%Color%Command    =   ""
  if ( len_trim(This%Text) /= 0 ) then
    This%Color  =   GPF_Colorspec_Type( Color_Keyword, Color )
  end if
End Subroutine

Subroutine SetTitleEnhanced( This, Enhanced )
  use GPF_Parameters            ,only:  KEY_enhanced, KEY_noenhanced
  type(GPF_Title_Type)                                  ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  Enhanced
  This%Enhanced         =   ''
  if ( len_trim(This%Text) /= 0 ) then
    if ( present(Enhanced) ) then
      if (Enhanced) then; This%Enhanced = KEY_enhanced
      else;               This%Enhanced = KEY_noenhanced
      end if
    end if
  end if
End Subroutine

End SubModule