SubModule(GPF_Term_WXT_Class) GPF_Term_WXT_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *    @TODO: ADD THE FOLLOWING INSTRUCTIONS TO THE TYPE DEFINITION AND ITS CONSTRUCTOR                         *
! **************************************************************************************************************
! **************************************************************************************************************

!     integer                                             ::  Unit                                            !< Window unit number
!     character(:)        ,allocatable                    ::  WSize                                           !< Window size
!     character(:)        ,allocatable                    ::  Background                                      !< Background
!     character(:)        ,allocatable                    ::  Raise                                           !< Rasied mode indicator
!     character(:)        ,allocatable                    ::  Ctrl                                            !< Control-key mode

!     procedure             ::  Set_Unit                                        !< Setting window unit number
!     procedure             ::  Set_WSize                                       !< Setting window size
!     procedure             ::  Set_Background                                  !< Setting window background color
!     procedure             ::  Set_Raise                                       !< Setting raise indicator
!     procedure             ::  Set_Ctrl                                        !< Setting control-key mode

!   character(*)                                ,optional ,intent(in)     ::  WSize                           !< Window size
!   character(*)                                ,optional ,intent(in)     ::  Background                      !< Background
!   logical                                     ,optional ,intent(in)     ::  Raise                           !< Rasied mode indicator
!   logical                                     ,optional ,intent(in)     ::  Ctrl                            !< Control-key mode

!     call This%Set_Unit( Unit )                                                                                  ! Setting window unit number
!     call This%Set_WSize( WSize )                                                                                ! Setting window size
!     call This%Set_Background( Background )                                                                      ! Setting window background color
!     call This%Set_Raise( Raise )                                                                                ! Setting
!     call This%Set_Ctrl( Ctrl )                                                                                  ! Setting
!     call This%Set_Command()                                                                                     ! Setting

Module Procedure GetName
  use GPF_Parameters            ,only:  KEY_wxt
  Name    =   KEY_wxt
End Procedure

Module Procedure InitializeTermWXT

  character(*)                                              ,parameter  ::  ProcName = "InitializeTermWXT"
  character(*)                                              ,parameter  ::  Keyword='term'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%SetEnhanced" )
  call This%SetEnhanced( Enhanced )

  if (Dbg) call Logger%Write( "Calling This%Font%Initialize" )
  call This%Font%Initialize( FontName, FontSize )

  if (Dbg) call Logger%Write( "Calling SetTerminalTitle" )
  call SetTerminalTitle( This, Title )

  if (Dbg) call Logger%Write( "Calling SetTerminalDashed" )
  call SetTerminalDashed( This, Dashed )

  if (Dbg) call Logger%Write( "Calling SetTerminalPersist" )
  call SetTerminalPersist( This, Persist )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetCommandTermWXT
  This%Command  =   'set '  //  &
    This%Keyword            //  &
    This%GetName()//" "     //      &
    This%Enhanced           //  &
    This%Font%Command       //  &
    This%Title              //  &
    This%Dashed             //  &
    This%Persist
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetTerminalTitle( This, Title )
  type(GPF_Term_WXT_Type)                               ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Title
  This%Title        =   ''
  if ( present(Title) ) then
    if ( len_trim(Title) /= 0) This%Title = 'title "' // Title // '" '
  end if
End Subroutine

Subroutine SetTerminalDashed( This, Dashed )
  use GPF_Parameters            ,only:  KEY_dashed, KEY_solid
  use Utilities_Library         ,only:  PresentAndFalse
  type(GPF_Term_WXT_Type)                               ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  Dashed
  This%Dashed   =   KEY_dashed
  if ( PresentAndFalse(Dashed) ) This%Dashed = KEY_solid
  This%Dashed   =   This%Dashed//' '
End Subroutine

Subroutine SetTerminalPersist( This, Persist )
  use GPF_Parameters            ,only:  KEY_persist
  use Utilities_Library         ,only:  PresentAndFalse
  type(GPF_Term_WXT_Type)                               ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  Persist
  This%Persist  =   KEY_persist
  if ( PresentAndFalse(Persist) ) This%Persist = 'no'//This%Persist
  This%Persist  =   This%Persist//' '
End Subroutine

End SubModule