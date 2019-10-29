! File: inline-GetValue_0d.F90
! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.

  logical                                                               ::  Dbg
  integer                                                               ::  Status_

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( present(Found) ) Found = This%Defined

! ==============================================================================================================
!    PRINTING LOGS
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "This%Defined = ", This%Defined )
    call Logger%Write( "This%Properties%HasDefaultValue = ", This%Properties%HasDefaultValue )
    if ( This%Properties%HasDefaultValue ) call Logger%Write( "This%Properties%DefaultValue = ", This%Properties%DefaultValue )
    if ( This%Properties%HasValidValues  ) call Logger%Write( "This%Properties%ValidValues  = ", This%Properties%ValidValues )
  end if
! ==============================================================================================================

! ==============================================================================================================
!    CASE OF A DEFINED PARAMETER
! ==============================================================================================================
  if ( This%Defined ) then
    if (Dbg) call Logger%Write( "Defined parameter case" )
    if (Dbg) call Logger%Write( "-> Calling Convert: This%Value = ", This%Value )
    call Convert( This%Value, Value, Status=Status_ )
    if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
    if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage(This%Value),Proc=ProcName,ExitLogger=Dbg) ) return
    if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) &
    call Error_Converting_String( This, CallProc )
    if (Dbg) call Logger%Write( "-> Value = ", Value )
! ==============================================================================================================



! ==============================================================================================================
!    CASE OF A UNDEFINED PARAMETER
! ==============================================================================================================
  else
    if (Dbg) call Logger%Write( "Undefined parameter case" )
    if (Dbg) call Logger%Write( "-> This%Properties%Mandatory = ", This%Properties%Mandatory )
    if (This%Properties%Mandatory) then
      call Error_Undefined_Mandatory_Value( This, CallProc )
    else
      if (Dbg) call Logger%Write( "-> This%Properties%HasDefaultValue = ", This%Properties%HasDefaultValue )
      if (This%Properties%HasDefaultValue) then
        if (Dbg) call Logger%Write( "-> Calling Convert: This%Properties%DefaultValue = ", This%Properties%DefaultValue )
        call Convert( This%Properties%DefaultValue, Value, Status=Status_ )
        if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
        if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage(This%Properties%DefaultValue),Proc=ProcName,ExitLogger=Dbg) ) return
        if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) &
        call Error_Converting_String( This, CallProc )
      else
        if (Dbg) call Logger%Write( "-> Nothing done" )
      end if
    end if
  end if
  if (Dbg) call Logger%Write( "-> Value = ", Value )
! ==============================================================================================================

!   call This%Properties%CheckValueValidity( Value, IsValid )
!   if ( .Not. IsValid ) then
!     call Error_ValueNotValid( This, CallProc )
!   end if

  if (Dbg) call Logger%Exiting()

  contains

Pure Function ErrorMessage(Str) result(ErrMsg)
  character(*)                                          ,intent(in)     ::  Str
  character(:)  ,allocatable                                            ::  ErrMsg
  ErrMsg  =   "Error converting variable from 'character' to '"//VarType//"' for string '"//Str//"'."
End Function

