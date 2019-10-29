SubModule(GPF_Font_Class) GPF_Font_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeFont

  character(*)                                              ,parameter  ::  ProcName = "InitializeFont"
  character(*)                                              ,parameter  ::  Keyword='font'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetFontName" )
  call SetFontName( This, Name )

  if (Dbg) call Logger%Write( "Calling SetFontSize" )
  call SetFontSize( This, Size )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetFontCommand
  This%Command          =       ''
  if ( This%Presence ) This%Command = This%Keyword // '"' // This%Name // ',' // This%Size // '" '
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Pure Subroutine SetFontName( This, Name )
  use GPF_Parameters            ,only:  FontType_Default, FontType_Valid
  use GPF_Tools                 ,only:  Check_Validity
  type(GPF_Font_Type)                                   ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Name
  if ( present(Name) ) then
    This%Name  =       Check_Validity( Name, FontType_Valid, FontType_Default )
  else
    This%Name  =       FontType_Default
  end if
  if ( len_trim(This%Name) /= 0 ) This%Presence = .True.
End Subroutine

Pure Subroutine SetFontSize( This, Size )
  use GPF_Parameters            ,only:  FontSize_Default
  use GPF_Tools                 ,only:  Check_Numeric
  type(GPF_Font_Type)                                   ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Size
  if ( present(Size) ) then
    This%Size  =       Check_Numeric( Size, FontSize_Default )
  else
    This%Size  =       ''
  end if
  if ( len_trim(This%Size) /= 0 ) This%Presence = .True.
End Subroutine

End SubModule