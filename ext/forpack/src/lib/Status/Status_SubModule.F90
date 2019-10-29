SubModule(Status_Module) Status_SubModule

  implicit none

  contains

! Module Procedure AddProcName
!   if ( .Not. present(Status) ) return
!   if ( Status%Ok() ) return
!   call Status%AddProcedure(ProcName)
! End Procedure

!       type(Status_Type)                           ,optional ,intent(inout)  ::  Status
!       character(*)                                ,optional ,intent(in)     ::  ProcName
!       character(*)                                ,optional ,intent(in)     ::  Message
!       logical                                     ,optional ,intent(in)     ::  ExitLogger
!       logical                                                               ::  Indicator
Module Procedure UpdateStatusAndRetrun

  use Utilities_Library   ,only:  PresentAndTrue
  use Logger_Class        ,only:  Logger

  Indicator   =   .False.

  if ( .Not. present(Status) ) return

  if ( Status%Ok() ) return

  if ( present(ProcName) )          &
    call Status%Set(                &
          ProcName  =   ProcName  , &
          Message   =   Message     )

!   if ( present(ProcName) ) call Status%AddProcedure(ProcName)

  if ( PresentAndTrue(ExitLogger) ) call Logger%Exiting()

  Indicator   =   .True.

End Procedure




! Module Procedure UpdateStatusAndRetrun
!   use Utilities_Library   ,only:  PresentAndTrue
!   use Logger_Class        ,only:  Logger
!   Indicator   =   .False.
!   if ( .Not. present(Status) ) return
!   if ( Status%Ok() ) return
!   if ( present(ProcName) )          &
!     call Status%Set(                &
!           ProcName  =   ProcName  , &
!           Message   =   Message     )
! !   if ( present(ProcName) ) call Status%AddProcedure(ProcName)
!   if ( PresentAndTrue(ExitLogger) ) call Logger%Exiting()
!   Indicator   =   .True.
! End Procedure

End SubModule