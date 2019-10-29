SubModule(DebugVariable_Class) DebugVariable_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_NOLOGS, LogLevel_HEAVYDEBUG
  implicit none

  contains

! @TODO: Add error checking
Module Procedure InitializeDebugVariableFromParam
  use String_Library            ,only:  IsIntegerNumber
  use Status_Library            ,only:  Status_Type
!   character(:)  ,allocatable                                            ::  ListValues(:), Value
!   integer                                                               ::  k, NValues
!   character(1)  ,allocatable                                            ::  VarType(:)
  type(Status_Type)                                                     ::  Status
  This%Name   =   Param%GetName()
  allocate( This%iValues(0) )
  allocate( character(0) :: This%cValues(0) )
!   call Param%GetValue( ListValues )
!   call Logger%Write( "  [InitializeDebugVariableFromParam] -> size(ListValues) = ", size(ListValues) )
!
!   NValues   =   size(ListValues)
!   allocate( VarType(NValues) )
!   do k = 1,size(ListValues)
!     Value   =   trim(ListValues(k))
!     call Logger%Write( "  [InitializeDebugVariableFromParam] -> k = ", k, "Value = ", Value )
!     if ( IsIntegerNumber(Value) ) then
!       VarType   =   "i"
!     else
!       VarType   =   "c"
!     end if
!   end do
!
!   if () cValues

  call Param%GetValue( This%iValues, Separator=" ", Status=Status )
  if ( Status%Ok() ) return
  call Param%GetValue( This%cValues, Separator=",", Status=Status )



End Procedure

Module Procedure OutputDebugVariableToString
  use String_Library            ,only:  Inline
  integer                                                               ::  i
  String    =   trim(This%Name)//"=["
  if ( size(This%iValues) > 0 ) String = String // Inline( This%iValues, Separator=",")
  if ( size(This%cValues) > 0 ) String = String // Inline( This%cValues, Separator=",")
  String    =   String // "]"
End Procedure

Module Procedure GetDebugVariableName
  Name    =   This%Name
End Procedure

Module Procedure NewDebugVariableFromNameValues
  This%Name   =   trim(Name)
  allocate( character(0) :: This%cValues(0) )
  if ( present(Values) ) then
    allocate( This%iValues, source = Values )
  else
    allocate( This%iValues(0) )
  end if
End Procedure

Module Procedure NewDebugVariableFromParam
  call This%Initialize(Param)
End Procedure

End SubModule