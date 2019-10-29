SubModule(Status_Class) Status_SubClass

  implicit none

  contains

Module Procedure AddStatusLevel
  use StatusLevel_Class         ,only:  NewStatusLevel
  integer                                                               ::  i, N
  type(StatusLevel_Type)  ,allocatable                                  ::  List(:)
  type(StatusLevel_Type)                                                ::  Item
  if ( .Not. allocated(This%Items) ) allocate( This%Items(0) )
  This%NItems   =   size(This%Items)
  N             =   This%NItems
  allocate( List(N+1) )
  do i = 1,N
    List(i)     =   This%Items(i)
  end do
  List(N+1)     =   NewStatusLevel( File, Line, Proc, Desc )
  call move_alloc( List, This%Items )
  This%NItems   =   size(This%Items)
End Procedure

Module Procedure WriteStatus
  use Logger_Class              ,only:  Logger
  use String_Library            ,only:  Indent
  integer                                                               ::  i, k, N
  character(:)  ,allocatable                                            ::  Line
  if ( This%NItems == 0 ) return
  k   =   0
  do i = This%NItems,1,-1
    k     =   k + 1
    N     =   (k -  1) * 2
    Line  =   Indent( This%Items(i)%GetLine(), N )
    call Logger%Write( Line )
  end do
End Procedure

Module Procedure CheckStatus
  use Utilities_Library   ,only:  PresentAndTrue
  if ( This%Ok() ) return
  call This%AddLevel( File, Line, Proc, Desc )
  if ( PresentAndTrue(Write) ) call This%Write()
  if ( PresentAndTrue(Fatal) ) error stop
End Procedure

Module Procedure UpdateStatus
  use Utilities_Library   ,only:  PresentAndTrue
  use Logger_Class        ,only:  Logger
  Indicator   =   .False.
  if ( .Not. present(Status) ) return
  if ( present(iStat) )      Status = iStat
  if ( PresentAndTrue(Err) ) Status = 1


  if ( Status%Ok() ) return
  call Status%AddLevel( File, Line, Proc, Desc )
  if ( PresentAndTrue(ExitLogger) ) call Logger%Exiting()
  Indicator   =   .True.
End Procedure














Module Procedure IsStatusOk
  Ok    =   This%Code == 0
End Procedure

Module Procedure IsStatusNotOk
  NotOk =   This%Code /= 0
End Procedure














Module Procedure AddStatusProperties
  if ( present(ProcName)  ) call This%AddProcedure(ProcName)
  if ( present(Message)   ) call This%SetMessage(Message)
  if ( present(Messages)  ) call This%SetMessage(Messages)
End Procedure

Module Procedure AddProcedureToStatus
  if ( .Not. allocated(This%Procedure) ) This%Procedure = ""
  if ( len_trim(This%Procedure) /= 0 ) This%Procedure  =   " > "//This%Procedure
  This%Procedure  =   ProcName//This%Procedure
End Procedure

Module Procedure AddLineToStatus
  use Utilities_Library       ,only:  AddElementToArray
  call AddElementToArray( Line, This%Message )
End Procedure

Module Procedure SetStatusMessage_0d
  if ( allocated(This%Message) ) deallocate( This%Message )
  allocate( This%Message, source = [trim(Message)] )
End Procedure

Module Procedure SetStatusMessage_1d
  use String_Library    ,only:  VecTrim
  if ( allocated(This%Message) ) deallocate( This%Message )
  allocate( This%Message, source = VecTrim(Message) )
End Procedure

Module Procedure AssignStatusToINT
  lhs%Code   =   ivar
End Procedure

Module Procedure EquivStatusToINT
  Res   =   This%Code == ivar
End Procedure

Module Procedure NonEqStatusToINT
  Res   =   This%Code /= ivar
End Procedure

! Module Procedure SetStatusCode
!   This%Code   =   Code
! End Procedure
!
! Module Procedure SetStatusProcedure
!   This%Procedure  = Procedure
! End Procedure
!
!
! Module Procedure SetStatusParameters
!   This%Code   =   1
!   if ( present(Code)      ) This%Code       =       Code
!   if ( present(Procedure) ) This%Procedure  =       Procedure
!   if ( present(Msg)       ) allocate( This%Message, source = [Msg] )
!   if ( present(MsgVec)    ) allocate( This%Message, source = MsgVec )
! End Procedure



Module Procedure AddProcName
  if ( .Not. present(Status) ) return
  if ( Status%Ok() ) return
  call Status%AddProcedure(ProcName)
End Procedure

End SubModule