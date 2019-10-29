SubModule(DebuggerItem_Class) DebuggerItem_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_HEAVYDEBUG

  implicit none

  contains

Module Procedure NewDebuggerItem
  use String_Library            ,only:  EmptyString
  use Utilities_Library         ,only:  AddElementToArray
  This%Activated      =   .False.
  call EmptyString( This%IncludedComponentList )
  call EmptyString( This%ExcludedComponentList )
  allocate( This%Var(0) )
  if ( present(Var)  ) call This%AddVar( Var )
  if ( present(Vars) ) call This%AddVar( Vars )
  if ( present(Components) )          call AddElementToArray(Components,This%IncludedComponentList)
!   if ( present(ExcludedComponents) )  call AddElementToArray(Components,This%ExcludedComponentList)
End Procedure

Module Procedure GetDebuggerItemLabel
  Label   =   ''
  if ( .Not. allocated(This%IncludedComponentList) ) return
  if ( size(This%IncludedComponentList) <1 ) return
  Label   =   This%IncludedComponentList(1)
End Procedure

! @TODO: Pass the full Logger object to check if active for current file
Module Procedure ActiveDebuggerItem
  integer                                                               ::  i
  Active    =   DbgLog%Units(This%iLogUnit)%Active
!   return
!   Active    =   .False.
!   do i = 1,size(This%Var)
!     if ( .Not. This%Var(i)%Active ) return
!   end do
!   Active    =   .True.
End Procedure

! @TODO: Add the File info... need to pass the full Logger object for this
Module Procedure OutputDebuggerItem
  use String_Library            ,only:  Convert_To_String
  character(*)                                              ,parameter  ::  ProcName='OutputDebugger'
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  String, VarString
  String  =   "  -> "
!   if ( len_trim(This%Label) /= 0 ) String = String// " " // This%Label // ": "
  do i = 1,size(This%Var)
    call This%Var(i)%Output( VarString )
    String    =   String // VarString
    if ( i /= size(This%Var) ) String = String // "; "
  end do
  call Logger%Write( String )
End Procedure


Module Procedure AddDebugVariableToDebugger
  integer                                                               ::  i, N
  type(DebugVariable_Type)  ,allocatable                                ::  List(:)
  if ( .Not. allocated(This%Var) ) allocate( This%Var(0) )
  N         =   size(This%Var)
  allocate( List(N+1) )
  do i = 1,N
    List(i) =   This%Var(i)
  end do
  i         =   N + 1
  List(i)   =   Var
  call move_alloc( List, This%Var )
End Procedure

Module Procedure AddDebugVariablesToDebugger
  integer                                                               ::  i, N
  type(DebugVariable_Type)  ,allocatable                                ::  List(:)
  if ( .Not. allocated(This%Var) ) allocate( This%Var(0) )
  N         =   size(This%Var)
  allocate( List(N+size(Var)) )
  do i = 1,N
    List(i) =   This%Var(i)
  end do
  do i = 1,size(Var)
    List(i+N)   =   Var(i)
  end do
  call move_alloc( List, This%Var )
End Procedure

! This procedure return the index of a variable from its name.
! The identification of a stored variable from its name is case insensitive.
! If the variable name provided in input does not match any variable stored
! in the object, then 0 is returned.
Module Procedure GetVariableIndexFromVariableName
  use String_Library            ,only:  Equal
  integer                                                               ::  i
  iVar   =   0
  do i = 1,size(This%Var)
    if ( .Not. Equal(VarName,This%Var(i)%Name,CaseSensitive=.False.) ) cycle
    iVar =   i
    return
  end do
End Procedure






! This procedure assess whether the DebuggerItem should be active for the current conditions
Module Procedure AssessDebuggerItemConditionsIntegerVariable
  This%Activated   =   This%ConditionsOk( VarName, VarValue )
End Procedure

Module Procedure AssessDebuggerItemConditionsCharacterVariable
  This%Activated   =   This%ConditionsOk( VarName, VarValue )
End Procedure

! This procedure updates the internal debugger status of current item.
! If looks if the input variable/value is tracked by the Iterm.
! * If the input variable is tracked by the Item, one check if the
!   associated value is included in the list of tracked values.
!   The debugger status is set to ture/false if the value of the
!   variable is found/not found in the list.
! * If the input variable is NOT tracked by the Item, the status remain
!   unchanged.
Module Procedure UpdateDebuggerVariableStatus

  use iso_fortran_env     ,only:  INT8, INT16, INT32, INT64
  use Utilities_Library   ,only:  IsIncluded
  use String_Library      ,only:  CountPresence

  integer                                                               ::  i, iVar

!   call Logger%Entering( "UpdateDebuggerVariableStatus" )

! ==============================================================================================================
!    PROCESSING THE LIST OF COMPONENTS TO INCLUDE, IF ANY
! ==============================================================================================================
  ! If there is a specific list of components to include, then process it
  ! If the list of components currently loading includes any of the component
  ! names in the list "IncludedComponentList", then go on and check if the
  ! variable names match. Otherwise, stop here.
  if ( size(This%IncludedComponentList) > 0 ) then
!     call Logger%Write( "Processing inlcude-only list of components" )
!     call Logger%Write( "-> This%IncludedComponentList = ", This%IncludedComponentList )
    if ( .Not. Components%IncludesAny(This%IncludedComponentList) ) then
!       call Logger%Write( "-> No active component found in include-only list => Exiting !!!" )
      DbgLog%Units(This%iLogUnit)%Active   =   .False.
!       call Logger%Exiting()
      return
    end if
  end if
! ==============================================================================================================


! ==============================================================================================================
!    PROCESSING THE LIST OF COMPONENTS TO EXCLUDE, IF ANY
! ==============================================================================================================
  if ( size(This%ExcludedComponentList) > 0 ) then
!     call Logger%Write( "Processing excluded list of components" )
!     call Logger%Write( "-> This%ExcludedComponentList = ", This%ExcludedComponentList )
    if ( Components%IncludesAny(This%ExcludedComponentList) ) then
!       call Logger%Write( "-> Current component is in list of excluded components => Exiting !!!" )
      DbgLog%Units(This%iLogUnit)%Active   =   .False.
!       call Logger%Exiting()
      return
    end if
  end if
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THE VARIABLE
! ==============================================================================================================
  iVar    =   This%GetVariableIndex(Name)
    if ( iVar > 0 ) then
    associate( Var => This%Var(iVar) )
      Var%Active    =   .False.
      select type (Value)
        type is (integer(INT8)) ; Var%Active = IsIncluded(int(Value),Var%iValues)
        type is (integer(INT16)); Var%Active = IsIncluded(int(Value),Var%iValues)
        type is (integer(INT32)); Var%Active = IsIncluded(int(Value),Var%iValues)
        type is (integer(INT64)); Var%Active = IsIncluded(int(Value),Var%iValues)
        type is ( character(*) ); Var%Active = IsIncluded(Value,Var%cValues,CaseSensitive=.False.)
      end select
      DbgLog%Units(This%iLogUnit)%Active   =   Var%Active ! Now, update indicator in the local Logger object
    end associate
  end if
! ==============================================================================================================

!   call Logger%Exiting()

End Procedure




Module Procedure VariableMatchAcceptedValue

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64

  integer                                                               ::  i, iVar
  integer                                                               ::  iValue
  logical                                                               ::  IntVariable, ChaVariable

  Ok      =   .False.
  iVar    =   This%GetVariableIndex(Name)
  if ( iVar == 0 ) return

  IntVariable   =   .False.
  ChaVariable   =   .False.
  select type (Value)

    type is (integer(INT8)) ; iValue = Value; IntVariable = .True.
    type is (integer(INT16)); iValue = Value; IntVariable = .True.
    type is (integer(INT32)); iValue = Value; IntVariable = .True.
    type is (integer(INT64)); iValue = Value; IntVariable = .True.
    type is ( character(*) )
      do i = 1,size(This%Var(iVar)%cValues)
        if ( Value /= This%Var(iVar)%cValues(i) ) cycle
        Ok    =   .True.
        return
      end do
  end select

  if (IntVariable) then
    do i = 1,size(This%Var(iVar)%iValues)
      if ( iValue /= This%Var(iVar)%iValues(i) ) cycle
      Ok    =   .True.
      return
    end do
  end if
End Procedure

! Module Procedure ConditionsOkForDebuggerItem_INT
!   integer                                                               ::  i, iVar
!   Ok      =   .False.
!   iVar    =   This%GetVariableIndex(VarName)
!   if ( iVar == 0 ) return
!   do i = 1,size(This%Var(iVar)%iValues)
!     if ( VarValue /= This%Var(iVar)%iValues(i) ) cycle
!     Ok    =   .True.
!     return
!   end do
! End Procedure
!
! Module Procedure ConditionsOkForDebuggerItem_CHAR
!   integer                                                               ::  i, iVar
!   Ok      =   .False.
!   iVar    =   This%GetVariableIndex(VarName)
!   if ( iVar == 0 ) return
!   do i = 1,size(This%Var(iVar)%cValues)
!     if ( VarValue /= This%Var(iVar)%cValues(i) ) cycle
!     Ok    =   .True.
!     return
!   end do
! End Procedure

End SubModule