! @TODO: Set_Position Procedure: Write a function that split the input position argument at each blank character and the check that each word is a valid position key.
! @TODO: If only one line is considered or if no line labels are specified, then unset the Key specifiation.
! @TODO: Implenents all the remaing option of the key command
SubModule(GPF_Key_Class) GPF_Key_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeKey

  use GPF_Parameters            ,only:  KEY_key

  character(*)                                              ,parameter  ::  ProcName = "InitializeGrid"
  character(*)                                              ,parameter  ::  Keyword=KEY_key
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling SetKeyFont" )
  call SetKeyFont( This, FontName, FontSize  )

  if (Dbg) call Logger%Write( "Calling SetKeyPosition" )
  call SetKeyPosition( This, Position )

  if (Dbg) call Logger%Write( "Calling SetKeySpacing" )
  call SetKeySpacing( This, Spacing )

  if (Dbg) call Logger%Write( "Calling SetKeyUnset" )
  call SetKeyUnset( This, Unset )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetKeyCommand
  This%Command  =   ''                                                                                      ! Initialisation to empty string: default value
  if (This%Presence)    This%Command = 'set ' // This%Keyword // ' ' // This%Font%Command// This%Position // This%Spacing   ! If key has to be set, then setting key command
  if (This%Unset)       This%Command = 'unset ' // This%Keyword                                                      ! If the key has to be unset
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetKeyFont( This, FontName, FontSize )
  type(GPF_Key_Type)                                    ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  FontName
  character(*)                                ,optional ,intent(in)     ::  FontSize
  This%Font%Command   =   ""
  call This%Font%Initialize( FontName, FontSize)
End Subroutine

Subroutine SetKeyPosition( This, Position )
  use GPF_Parameters            ,only:  KEY_Position_Valid
  use GPF_Tools                 ,only:  Is_Valid
  type(GPF_Key_Type)                                    ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Position
  This%Position =   ''
  if ( present(Position) ) then
    if ( Is_Valid(Position,KEY_Position_Valid) ) This%Position = trim(Position) // ' '
  end if
  if ( len_trim(This%Position) /= 0 ) This%Presence = .True.
End Subroutine

Subroutine SetKeySpacing( This, Spacing )
  use GPF_Tools                 ,only:  Is_Numeric
  type(GPF_Key_Type)                                    ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Spacing
  This%Spacing  =   ''
  if ( present(Spacing) ) then
    if ( Is_Numeric(trim(Spacing)) ) This%Spacing = 'spacing '// trim(Spacing) // ' '
  end if
  if ( len_trim(This%Spacing) /= 0 ) This%Presence = .True.
End Subroutine

Subroutine SetKeyUnset( This, Unset )
  implicit none
  type(GPF_Key_Type)                                    ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  Unset
  This%Unset    =   .False.
  if ( present(Unset) ) This%Unset      =   Unset
  if ( This%Unset ) This%Presence       =   .False.
End Subroutine

End SubModule